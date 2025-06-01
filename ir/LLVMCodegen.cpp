//
// Created by vadim on 06.10.24.
//

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/IR/Instructions.h>

#include "ast/FunctionNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/NumberNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/BinOpNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/IfStatement.h"
#include "ast/BooleanNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/StringNode.h"
#include "../type/Type.h"
#include "ast/LoopCondNode.h"
#include "ast/ReturnNode.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/MethodCallNode.h"
#include "ast/ArrayNode.h"

#include "LLVMCodegen.h"
#include "IRTypeFactory.h"
#include "ast/IndexAccessNode.h"
#include "ast/TypeCastNode.h"
#include "type/TypeFactory.h"
#include "type/FunctionType.h"
#include "ast/ModuleNode.h"
#include "./IdentNodeGenerator.h"

namespace {
    llvm::Function *getModuleFunction(const std::string &name,
                                      const ModuleContext &mc) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = mc.module->getFunction(name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.

        if (const auto &proto = mc.symTable.lookupFunction(name);
            !proto.empty()) {
            if (const auto functionType = proto[0]->type->asFunction()) {
                std::vector<llvm::Type *> functionParams;
                functionParams.reserve(functionType.value()->parametersType().size());
                for (const auto &param: functionType.value()->parametersType()) {
                    functionParams.push_back(
                            IRTypeFactory::from(param, mc.module->getContext())->getLLVMType(mc.module->getContext()));
                }
                auto *retType = IRTypeFactory::from(functionType.value()->returnType(), mc.module->getContext())->
                        getLLVMType(mc.module->getContext());
                auto *const llvmFunctionType = llvm::FunctionType::get(
                        retType, functionParams,
                        functionType.value()->isVariadic());
                return llvm::Function::Create(llvmFunctionType,
                                              llvm::Function::ExternalLinkage,
                                              name,
                                              mc.module.get());
            }
        }
        // If no existing prototype exists, return null.
        return nullptr;
    }

    std::string typeToString(const llvm::Type *type) {
        std::string typeStr;
        llvm::raw_string_ostream rso(typeStr);
        type->print(rso);
        return rso.str();
    }

    llvm::Value *tryCastValue(const std::unique_ptr<llvm::IRBuilder<>> &builder,
                              llvm::Value *const value,
                              llvm::Type *const destType) {
        if (value->getType() == destType) {
            return value;
        }

        const llvm::Type *const srcType = value->getType();

        auto getCastOp = [&]() -> std::optional<llvm::Instruction::CastOps> {
            if (destType->isIntegerTy(1) && !value->getType()->isIntegerTy(1)) {
                return std::nullopt;
            }

            // Floating -> Integer
            if (srcType->isFloatingPointTy() && destType->isIntegerTy()) {
                return llvm::Instruction::FPToSI;
            }

            // Integer -> Floating
            if (srcType->isIntegerTy() && destType->isFloatingPointTy()) {
                return llvm::Instruction::SIToFP;
            }

            // Integer extension/truncation
            if (srcType->isIntegerTy() && destType->isIntegerTy()) {
                const unsigned srcBits = srcType->getIntegerBitWidth();
                const unsigned destBits = destType->getIntegerBitWidth();

                if (srcBits == 1) {
                    return llvm::Instruction::ZExt;
                }
                return destBits > srcBits
                           ? llvm::Instruction::SExt
                           : llvm::Instruction::Trunc;
            }

            return std::nullopt;
        };

        if (const auto castOp = getCastOp()) {
            return builder->CreateCast(*castOp, value, destType);
        }

        throw std::logic_error("Unsupported cast from " +
                               typeToString(srcType) + " to " +
                               typeToString(destType));
    }

    void generateBasicBlock(llvm::BasicBlock *const basicBlock,
                            const BlockNode::Statements &statements,
                            ModuleContext &mc,
                            const std::optional<std::function<void()>> &prologue = std::nullopt) {
        mc.symTable.enterScope();

        mc.builder->SetInsertPoint(basicBlock);
        if (prologue.has_value()) {
            (*prologue)();
        }

        for (const auto &stmt: statements) {
            LLVMCodegen::generate(stmt.get(), mc);
        }

        mc.symTable.exitScope();
    }


    IRValue genGlobalDeclaration(const DeclarationNode *node,
                                 llvm::Type *type,
                                 llvm::Value *init,
                                 ModuleContext &mc) {
        llvm::Constant *constInit = nullptr;
        if (init) {
            constInit = llvm::dyn_cast<llvm::Constant>(init);
            if (!constInit) {
                throw std::logic_error(
                        "Global variable initializer must be constant: " + node->ident->name);
            }
        }

        auto *gVar = new llvm::GlobalVariable(*mc.module,
                                              type,
                                              true,
                                              llvm::GlobalValue::InternalLinkage,
                                              constInit,
                                              node->ident->name);

        gVar->setAlignment(llvm::MaybeAlign(8));
        gVar->setDSOLocal(true);

        mc.symTable.insert(node->ident->name, std::make_shared<GlobalSymbolInfo>(node->type, gVar));
        return IRValue::createGlobal(gVar,
                                     IRTypeFactory::from(node->type, mc.module->getContext()),
                                     node->ident->name);
    }

    IRValue genLocalDeclaration(const DeclarationNode *node,
                                llvm::Type *type,
                                llvm::Value *init,
                                ModuleContext &mc) {
        auto *alloca = mc.builder->CreateAlloca(type, nullptr, node->ident->name);

        if (init) {
            auto *const casted = tryCastValue(mc.builder, init, type);
            if (!casted) {
                throw std::logic_error("Type mismatch in initialization of: " + node->ident->name);
            }
            mc.builder->CreateStore(casted, alloca);
        }

        if (mc.symTable.lookup(node->ident->name)) {
            throw std::logic_error("Redeclaration of variable: " + node->ident->name);
        }

        mc.symTable.insert(node->ident->name,
                           std::make_shared<AllocaInstSymbolInfo>(node->type,
                                                                  alloca));
        return IRValue::createAlloca(alloca, IRTypeFactory::from(node->type, mc.module->getContext()),
                                     node->ident->name);
    }

    void processFunctionParameters(llvm::Function *func,
                                   llvm::BasicBlock *basicBlock,
                                   const FunctionNode *node,
                                   ModuleContext &mc) {
        mc.builder->SetInsertPoint(basicBlock);

        for (auto &arg: func->args()) {
            const auto &paramType = node->proto->params[arg.getArgNo()]->type;
            auto *const alloca = mc.builder->CreateAlloca(
                    IRTypeFactory::from(paramType, mc.module->getContext())->getLLVMType(mc.module->getContext()),
                    nullptr,
                    arg.getName());

            mc.builder->CreateStore(&arg, alloca);
            mc.symTable.insert(std::string(arg.getName()),
                               std::make_shared<AllocaInstSymbolInfo>(paramType, alloca));
        }
    }
} // namespace

LLVMCodegen::LLVMCodegen(ModuleContext &moduleContext) :
    mc(moduleContext) {
        generators[std::type_index(typeid(IdentNode))] = std::make_unique<IdentNodeGenerator>();
    }

void LLVMCodegen::visit(IdentNode *node) {
    res = generators[std::type_index(typeid(IdentNode))]->generate(node, mc);
}

void LLVMCodegen::visit(FunctionNode *const node) {
    generate(node->proto.get(), mc);
    auto *const func = getModuleFunction(node->proto->name, mc);
    if (!func) {
        throw std::logic_error("Function prototype generation failed for: " + node->proto->name);
    }
    auto *const basicBlock = llvm::BasicBlock::Create(mc.module->getContext(),
                                                      "entry",
                                                      func);

    generateBasicBlock(basicBlock,
                       node->body->statements,
                       mc,
                       [&]() {
                           processFunctionParameters(func, basicBlock, node, mc);
                       });

    if (node->proto->returnType->isVoid()) {
        mc.builder->CreateRetVoid();
    }

    std::string verifyError;
    llvm::raw_string_ostream os(verifyError);
    if (verifyFunction(*func, &os)) {
        throw std::logic_error("Function verification failed:\n" + os.str());
    }
}

void LLVMCodegen::visit(NumberNode *node) {
    auto type = IRTypeFactory::from(node->getType(), mc.module->getContext());
    res = IRValue::createValue(type->createConstant(node, *mc.builder, *mc.module), type);
}

void LLVMCodegen::visit(StringNode *node) {
    auto type = IRTypeFactory::from(node->getType(), mc.module->getContext());
    res = IRValue::createValue(type->createConstant(node, *mc.builder, *mc.module),
                               type,
                               node->str + ".str");
}

void LLVMCodegen::visit(BooleanNode *node) {
    auto type = IRTypeFactory::from(node->getType(), mc.module->getContext());
    res = IRValue::createValue(type->createConstant(node, *mc.builder, *mc.module), type);
}

void LLVMCodegen::visit(BinOpNode *node) {
    auto *const lhsValue = generate(node->lhs.get(), mc).value().createLoad(*mc.builder);
    auto *const rhsValue = generate(node->rhs.get(), mc).value().createLoad(*mc.builder);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        throw std::logic_error("Unexpected expression");
    }
    if (lhsValue->getType()->isPointerTy() || rhsValue->getType()->isPointerTy()) {
        throw std::logic_error("Unsupported operation");
    }
    if (const auto category = getOperationCategory(node->binOp); category == OperationCategory::Comparison) {
        const auto type = IRTypeFactory::from(node->lhs->getType(), mc.module->getContext());
        res = IRValue::createValue(type->createBinaryOp(*mc.builder, node->binOp, lhsValue, rhsValue, "binOp"), type);
    } else if (category == OperationCategory::Arithmetic) {
        const auto resultTypeNode = IRTypeFactory::from(node->getType(), mc.module->getContext());
        res = IRValue::createValue(
                resultTypeNode->createBinaryOp(*mc.builder, node->binOp, lhsValue, rhsValue, "binOp"),
                resultTypeNode);
    } else {
        throw std::logic_error("Unsupported operation");
    }
}

void LLVMCodegen::visit(ProtoFunctionStatement *node) {
    std::vector<llvm::Type *> functionParams;
    functionParams.reserve(node->params.size());
    for (const auto &param: node->params) {
        functionParams.push_back(IRTypeFactory::from(param->type, mc.module->getContext())
                ->getLLVMType(mc.module->getContext()));
    }

    auto *const functionType =
            llvm::FunctionType::get(IRTypeFactory::from(node->returnType, mc.module->getContext())
                                    ->getLLVMType(mc.module->getContext()),
                                    functionParams, node->isVarArgs);

    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  mc.module.get());
    std::vector<TypePtr> params;
    std::ranges::transform(node->params,
                           std::back_inserter(params),
                           [](const auto &i) {
                               return i->type;
                           });
    mc.symTable.insertFunction(node->name,
                               std::make_shared<SymbolInfo>(TypeFactory::makeFunction(node->returnType, params)));
    // function->addFnAttr(llvm::Attribute::NoUnwind);
    // function->addRetAttr(llvm::Attribute::ZExt);
    for (auto [index, arg]: llvm::enumerate(function->args())) {
        arg.setName(node->params[index]->ident->name);
    }
}

void LLVMCodegen::visit(AssignmentNode *const node) {
    const auto init = generate(node->rvalue.get(), mc);
    if (!init) {
        throw std::runtime_error("Error init value generation");
    }
    if (mc.builder->GetInsertBlock() != nullptr) {
        if (const auto var = mc.symTable.lookup(node->lvalue->name)) {
            if (const auto si = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(var.value())) {
                init.value().createStore(*mc.builder, si->inst);
                auto irType = IRTypeFactory::from(si->type, mc.module->getContext());
                res = IRValue::createAlloca(si->inst, std::move(irType));
            }
        } else if (const auto gVar = mc.symTable.lookupGlobal(node->lvalue->name)) {
            if (const auto &sig = std::dynamic_pointer_cast<const GlobalSymbolInfo>(gVar.value())) {
                if (sig->var->isConstant()) {
                    throw std::logic_error("Variable: " + node->lvalue->name + " is constant");
                }
                init.value().createStore(*mc.builder, sig->var);
                auto irType = IRTypeFactory::from(sig->type, mc.module->getContext());
                res = IRValue::createGlobal(sig->var, std::move(irType));
            }
        } else {
            throw std::logic_error("Undefined variable: " + node->lvalue->name);
        }
    }
}

void LLVMCodegen::visit(FunctionCallNode *const node) {
    auto *const calleeFunc = getModuleFunction(node->ident->name, mc);
    if (calleeFunc == nullptr) {
        throw std::runtime_error(std::format("Undefined reference: '{}'", node->ident->name));
    }

    std::vector<llvm::Value *> argsFunc;
    argsFunc.reserve(node->args.size());
    const auto *const funcType = calleeFunc->getFunctionType();
    for (size_t i = 0; i < node->args.size(); ++i) {
        const auto arg = generate(node->args[i].get(), mc);
        if (!arg) {
            throw std::runtime_error("Error argument function generation");
        }
        const auto argType = IRTypeFactory::from(node->args[i]->getType(),
                                                 mc.module->getContext());
        auto *argValue = argType->createLoad(*mc.builder, arg.value());
        if (i < funcType->getNumParams()) {
            argValue = tryCastValue(mc.builder, argValue, funcType->getParamType(i));
        }
        argsFunc.push_back(argValue);
    }

    auto irType = IRTypeFactory::from(node->getType(), mc.module->getContext());
    res = IRValue::createValue(mc.builder->CreateCall(calleeFunc, argsFunc),
                               std::move(irType));
}

void LLVMCodegen::visit(IfStatement *node) {
    auto *const firstCV = tryCastValue(mc.builder,
                                       generate(node->ifBranch.cond.get(), mc).value().getRawValue(),
                                       mc.builder->getInt1Ty());
    if (!firstCV) {
        throw std::logic_error("Condition must be boolean type");
    }

    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();

    auto *const firstIfBB = llvm::BasicBlock::Create(mc.module->getContext(), "if", parentFunc);
    auto *lastElseBB = llvm::BasicBlock::Create(mc.module->getContext(), "else");
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "merge_if");

    mc.builder->CreateCondBr(firstCV, firstIfBB, lastElseBB);

    mc.builder->SetInsertPoint(firstIfBB);
    generate(node->ifBranch.then.get(), mc);

    if (!mc.builder->GetInsertBlock()->getTerminator()) {
        mc.builder->CreateBr(mergeBB);
    }

    for (size_t i = 0; i < node->elseIfBranches.size(); ++i) {
        lastElseBB->insertInto(parentFunc);
        mc.builder->SetInsertPoint(lastElseBB);

        const auto &[cond, then] = node->elseIfBranches[i];
        auto *const value = tryCastValue(mc.builder,
                                         generate(cond.get(), mc).value().getRawValue(),
                                         mc.builder->getInt1Ty());
        if (!value) {
            throw std::logic_error("Condition must be boolean type");
        }
        auto *const ifBB = llvm::BasicBlock::Create(mc.module->getContext(),
                                                    "elif_" + std::to_string(i), parentFunc);
        lastElseBB = llvm::BasicBlock::Create(mc.module->getContext(), "else_" + std::to_string(i));
        mc.builder->CreateCondBr(value, ifBB, lastElseBB);

        mc.builder->SetInsertPoint(ifBB);
        generate(then.get(), mc);

        if (!mc.builder->GetInsertBlock()->getTerminator()) {
            mc.builder->CreateBr(mergeBB);
        }
    }

    lastElseBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(lastElseBB);
    if (node->elseBranch.has_value()) {
        generate(node->elseBranch.value().get(), mc);
    }
    if (!mc.builder->GetInsertBlock()->getTerminator()) {
        mc.builder->CreateBr(mergeBB);
    }

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);
}

void LLVMCodegen::visit(UnaryOpNode *node) {
    if (node->operatorType == TokenType::Increment
        || node->operatorType == TokenType::Decrement
        || node->operatorType == TokenType::Plus
        || node->operatorType == TokenType::Minus) {
        const auto irType = IRTypeFactory::from(node->getType(), mc.module->getContext());
        if (!isNode<IdentNode>(node->expr.get())
            && (node->operatorType == TokenType::Increment || node->operatorType == TokenType::Decrement)) {
            throw std::logic_error("Increment/decrement requires lvalue variable");
        }
        const auto irValue = generate(node->expr.get(), mc);

        res = IRValue::createValue(irValue.value().getType()->createUnaryOp(*mc.builder,
                                                                            node->operatorType,
                                                                            irValue.value().createLoad(*mc.builder),
                                                                            irValue.value().getRawValue(),
                                                                            "incdec"),
                                   irValue.value().getType());
    } else {
        throw std::logic_error("Unsupported unary operator");
    }
}

void LLVMCodegen::visit(LoopCondNode *node) {
    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB = nullptr;
    auto *const loopBB = llvm::BasicBlock::Create(mc.module->getContext(), "loop", parentFunc);
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "merge");

    if (node->loopType == LoopCondNode::Type::For) {
        if (node->init) {
            generate(node->init->get(), mc);
        }
        condBB = llvm::BasicBlock::Create(mc.module->getContext(), "for.cond", parentFunc);
        mc.builder->CreateBr(condBB);
    }

    switch (node->loopType) {
        case LoopCondNode::Type::For: {
            mc.builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(mc.builder,
                                            generate(node->condBranch.cond.get(), mc).value().getRawValue(),
                                            mc.builder->getInt1Ty());
            mc.builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::While: {
            condBB = llvm::BasicBlock::Create(mc.module->getContext(), "while.cond", parentFunc);
            mc.builder->CreateBr(condBB);
            mc.builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(mc.builder,
                                            generate(node->condBranch.cond.get(), mc).value().getRawValue(),
                                            mc.builder->getInt1Ty());
            mc.builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::DoWhile: {
            mc.builder->CreateBr(loopBB);
            break;
        }
    }

    mc.builder->SetInsertPoint(loopBB);
    generate(node->condBranch.then.get(), mc);

    switch (node->loopType) {
        case LoopCondNode::Type::For: {
            if (node->increment) {
                generate(node->increment->get(), mc);
            }
            mc.builder->CreateBr(condBB);
            break;
        }
        case LoopCondNode::Type::While: {
            mc.builder->CreateBr(condBB);
            break;
        }
        case LoopCondNode::Type::DoWhile: {
            const auto cond = generate(node->condBranch.cond.get(), mc);
            mc.builder->CreateCondBr(cond.value().getRawValue(), loopBB, mergeBB);
            break;
        }
    }

    if (!loopBB->getTerminator()) {
        mc.builder->CreateBr(mergeBB);
    }

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);
}

void LLVMCodegen::visit(BlockNode *node) {
    if (!mc.builder->GetInsertBlock()) {
        throw std::logic_error("Block generation outside of function context");
    }

    generateBasicBlock(mc.builder->GetInsertBlock(), node->statements, mc);
}

void LLVMCodegen::visit(DeclarationNode *node) {
    const auto irType = IRTypeFactory::from(node->type, mc.module->getContext());
    auto *const llvmType = irType->getLLVMType(mc.module->getContext());
    if (llvmType == nullptr) {
        throw std::logic_error("Unknown type for variable: " + node->ident->name);
    }

    llvm::Value *initValue = nullptr;
    if (node->init.has_value()) {
        const auto valueHandler = generate(node->init.value().get(), mc);
        initValue = valueHandler.value().getRawValue();
        if (node->init.value()->getType()->isArray()
            && isNode<IdentNode>(node->init.value().get())) {
            initValue = valueHandler.value().createLoad(*mc.builder);
        }
        if (!initValue) {
            throw std::logic_error("Failed to generate initializer for: " + node->ident->name);
        }
    } else {
        if (llvmType->isAggregateType()) {
            initValue = llvm::ConstantAggregateZero::get(llvmType);
        } else {
            initValue = llvm::Constant::getNullValue(llvmType);
        }
    }

    if (node->isGlobal) {
        genGlobalDeclaration(node, llvmType, initValue, mc);
    } else {
        genLocalDeclaration(node, llvmType, initValue, mc);
    }
}

void LLVMCodegen::visit(ReturnNode *node) {
    if (node->expr != nullptr) {
        mc.builder->CreateRet(generate(node->expr.get(), mc).value().createLoad(*mc.builder));
    } else {
        mc.builder->CreateRetVoid();
    }
}

void LLVMCodegen::visit(TernaryOperatorNode *node) {
    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();

    auto *const thenBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_then", parentFunc);
    auto *const elseBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_else");
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_merge");

    mc.builder->CreateCondBr(generate(node->cond.get(), mc).value().getRawValue(), thenBB, elseBB);

    mc.builder->SetInsertPoint(thenBB);
    const auto trueValueHandler = generate(node->trueExpr.get(), mc);
    auto *const trueVal = trueValueHandler.value().createLoad(*mc.builder);
    mc.builder->CreateBr(mergeBB);

    elseBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(elseBB);
    auto falseValueHandler = generate(node->falseExpr.get(), mc);
    auto *const falseVal = trueValueHandler.value().createLoad(*mc.builder);
    mc.builder->CreateBr(mergeBB);

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);

    if (trueVal->getType() != falseVal->getType()) {
        throw std::logic_error("Ternary expressions must be of the same type");
    }

    auto *const phi = mc.builder->CreatePHI(trueVal->getType(), 2, "tern_result");
    phi->addIncoming(trueVal, thenBB);
    phi->addIncoming(falseVal, elseBB);
    res = IRValue::createValue(phi, IRTypeFactory::from(node->getType(), mc.module->getContext()));
}

void LLVMCodegen::visit(MethodCallNode *node) {
    const auto objectType = IRTypeFactory::from(node->object->getType(), mc.module->getContext());
    std::vector<llvm::Value *> args;
    args.reserve(node->method->args.size());
    for (const auto &arg: node->method->args) {
        args.push_back(generate(arg.get(), mc).value().createLoad(*mc.builder));
    }

    if (const auto fType = node->method->getType()->asFunction()) {
        const auto methodInfo = MethodInfo::create(node->method->ident->name,
                                                   fType.value());
        res = IRValue::createValue(objectType->createMethodCall(*mc.builder,
                                                                methodInfo,
                                                                generate(node->object.get(), mc).value().getRawValue(),
                                                                args), IRTypeFactory::from(
                                           fType.value()->returnType(), mc.module->getContext()));
    }
}

void LLVMCodegen::visit(FieldAccessNode *node) {
    throw std::logic_error("FieldAccessNode not implemented");
}

void LLVMCodegen::visit(CommentNode *node) {
    // skip comments
}

void LLVMCodegen::visit(ModuleNode *node) {
    mc.symTable.enterScope();
    for (const auto &statement: node->statements) {
        generate(statement.get(), mc);
    }
    mc.symTable.exitScope();
}

void LLVMCodegen::visit(TypeCastNode *node) {
    const auto value = generate(node->expr.get(), mc).value();
    const auto irType = IRTypeFactory::from(node->targetType, mc.module->getContext());
    res = IRValue::createValue(
            tryCastValue(mc.builder, value.getRawValue(), irType->getLLVMType(mc.module->getContext())),
            irType);
}

void LLVMCodegen::visit(ArrayNode *node) {
    const auto arrayType = IRTypeFactory::from(node->getType(), mc.module->getContext());
    res = IRValue::createValue(arrayType->createConstant(node, *mc.builder, *mc.module), arrayType);
}

void LLVMCodegen::visit(IndexAccessNode *node) {
    const auto object = generate(node->object.get(), mc);
    const auto index = generate(node->index.get(), mc);

    if (const auto arrayType = node->object->getType()->asArray()) {
        const auto elementType = arrayType.value()->getElementType();
        const auto arrayIrType = IRTypeFactory::from(arrayType.value(), mc.module->getContext());
        auto *const arrayLlvmType = llvm::dyn_cast<llvm::ArrayType>(arrayIrType->getLLVMType(mc.module->getContext()));
        auto elementIrType = IRTypeFactory::from(arrayType.value()->getElementType(),
                                                 mc.module->getContext());
        const std::vector<llvm::Value *> indices = {
                mc.builder->getInt64(0),
                index.value().createLoad(*mc.builder)
        };
        auto *const elementLlvmValue = mc.builder->CreateLoad(arrayIrType->getLLVMElementType(mc.module->getContext()),
                                                              mc.builder->CreateInBoundsGEP(
                                                                      arrayLlvmType,
                                                                      object.value().getRawValue(),
                                                                      indices,
                                                                      "elem_ptr"));
        res = IRValue::createValue(elementLlvmValue, std::move(elementIrType));
    } else {
        throw std::runtime_error("Unsupported index access type");
    }
}

IRValueOpt LLVMCodegen::value() const {
    return res;
}
