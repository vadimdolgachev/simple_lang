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
            [[maybe_unused]] auto *const val = LLVMCodegen::generate(
                    stmt.get(), mc);
        }

        mc.symTable.exitScope();
    }


    llvm::GlobalVariable *genGlobalDeclaration(const DeclarationNode *node,
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
        return gVar;
    }

    llvm::AllocaInst *genLocalDeclaration(const DeclarationNode *node,
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
        return alloca;
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

LLVMCodegen::LLVMCodegen(ModuleContext &moduleContext):
    mc(moduleContext) {}

void LLVMCodegen::visit(IdentNode *node) {
    if (const auto symbol = mc.symTable.lookup(node->name)) {
        if (const auto &sig = std::dynamic_pointer_cast<const GlobalSymbolInfo>(symbol.value())) {
            value_ = mc.builder->CreateLoad(sig->var->getValueType(),
                                            sig->var,
                                            node->name + ".global");
        } else if (const auto &sia = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(symbol.value())) {
            if (sia->inst == nullptr) {
                throw std::runtime_error(std::format("Unknown variable name: {}", node->name));
            }
            if (sia->type->isArray()) {
                value_ = sia->inst;
            } else {
                value_ = mc.builder->CreateLoad(sia->inst->getAllocatedType(),
                                                sia->inst,
                                                node->name);
            }
        }
    }
}

void LLVMCodegen::visit(FunctionNode *const node) {
    auto *const func = llvm::dyn_cast<llvm::Function>(
            generate(node->proto.get(), mc));

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

    value_ = func;
}

void LLVMCodegen::visit(NumberNode *node) {
    value_ = IRTypeFactory::from(node->getType(), mc.module->getContext())->createConstant(
            node, *mc.builder, *mc.module);
}

void LLVMCodegen::visit(StringNode *node) {
    value_ = IRTypeFactory::from(node->getType(), mc.module->getContext())->createConstant(
            node, *mc.builder, *mc.module);
}

void LLVMCodegen::visit(BooleanNode *node) {
    value_ = IRTypeFactory::from(node->getType(), mc.module->getContext())->createConstant(
            node, *mc.builder, *mc.module);
}

void LLVMCodegen::visit(BinOpNode *node) {
    auto *lhsValue = generate(node->lhs.get(),
                              mc);
    auto *rhsValue = generate(node->rhs.get(),
                              mc);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        throw std::logic_error("Unexpected expression");
    }
    if (lhsValue->getType()->isPointerTy() || rhsValue->getType()->isPointerTy()) {
        throw std::logic_error("Unsupported operation");
    }
    if (const auto category = getOperationCategory(node->binOp); category == OperationCategory::Comparison) {
        value_ = IRTypeFactory::from(node->lhs->getType(), mc.module->getContext())
                ->createBinaryOp(*mc.builder, node->binOp, lhsValue, rhsValue, "binOp");
    } else if (category == OperationCategory::Arithmetic) {
        const auto resultTypeNode = IRTypeFactory::from(node->getType(), mc.module->getContext());
        value_ = resultTypeNode->createBinaryOp(*mc.builder, node->binOp, lhsValue, rhsValue, "binOp");
    } else {
        throw std::logic_error("Unsupported operation");
    }
}

void LLVMCodegen::visit(ProtoFunctionStatement *node) {
    std::vector<llvm::Type *> functionParams;
    functionParams.reserve(node->params.size());
    for (const auto &param: node->params) {
        functionParams.push_back(
                IRTypeFactory::from(param->type, mc.module->getContext())->getLLVMType(mc.module->getContext()));
    }

    auto *const functionType = llvm::FunctionType::get(
            IRTypeFactory::from(node->returnType, mc.module->getContext())->getLLVMType(mc.module->getContext()),
            functionParams,
            node->isVarArgs);

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
    value_ = function;
}

void LLVMCodegen::visit(AssignmentNode *const node) {
    auto *const init = generate(node->rvalue.get(), mc);
    if (mc.builder->GetInsertBlock() != nullptr) {
        if (const auto var = mc.symTable.lookup(node->lvalue->name)) {
            if (const auto si = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(var.value())) {
                mc.builder->CreateStore(tryCastValue(mc.builder, init, si->inst->getAllocatedType()),
                                        si->inst);
                value_ = si->inst;
            }
        } else if (const auto gVar = mc.symTable.lookupGlobal(node->lvalue->name)) {
            if (const auto &sig = std::dynamic_pointer_cast<const GlobalSymbolInfo>(gVar.value())) {
                if (sig->var->isConstant()) {
                    throw std::logic_error("Variable: " + node->lvalue->name + " is constant");
                }
                mc.builder->CreateStore(tryCastValue(mc.builder, init, sig->var->getValueType()),
                                        sig->var);
                value_ = sig->var;
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
        auto *argValue = generate(node->args[i].get(), mc);
        if (i < funcType->getNumParams()) {
            argValue = tryCastValue(mc.builder, argValue, funcType->getParamType(i));
        }
        argsFunc.push_back(argValue);
    }

    value_ = mc.builder->CreateCall(calleeFunc, argsFunc);
}

void LLVMCodegen::visit(IfStatement *node) {
    auto *const firstCV = tryCastValue(mc.builder,
                                       generate(node->ifBranch.cond.get(), mc),
                                       mc.builder->getInt1Ty());
    if (!firstCV) {
        throw std::logic_error("Condition must be boolean type");
    }

    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();

    auto *const firstIfBB = llvm::BasicBlock::Create(mc.module->getContext(), "if", parentFunc);
    auto *lastElseBB = llvm::BasicBlock::Create(mc.module->getContext(), "else");
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "merge_if");

    value_ = mc.builder->CreateCondBr(firstCV, firstIfBB, lastElseBB);

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
                                         generate(cond.get(), mc),
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
    if (node->operatorType == TokenType::PlusPlus || node->operatorType == TokenType::MinusMinus) {
        const auto *ident = dynamic_cast<IdentNode *>(node->expr.get());
        if (ident == nullptr) {
            throw std::logic_error("Increment/decrement requires lvalue variable");
        }

        const auto var = mc.symTable.lookup(ident->name);
        if (var == std::nullopt) {
            throw std::logic_error("Undefined variable: " + ident->name);
        }
        const auto symbolInfoAlloca = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(var.value());
        auto *const varType = symbolInfoAlloca->inst->getAllocatedType();

        if (!varType->isIntOrIntVectorTy() &&
            !varType->isFPOrFPVectorTy() &&
            !varType->isPointerTy()) {
            throw std::logic_error("Invalid type for increment/decrement");
        }

        auto *const loadedVal = mc.builder->CreateLoad(varType,
                                                       symbolInfoAlloca->inst,
                                                       ident->name + ".val");

        llvm::Value *delta = nullptr;
        if (varType->isIntegerTy()) {
            delta = llvm::ConstantInt::get(varType, 1);
        } else {
            delta = llvm::ConstantFP::get(varType, 1.0);
        }
        if (node->operatorType == TokenType::MinusMinus) {
            delta = mc.builder->CreateNeg(delta, "neg.tmp");
        }

        auto *const newVal = mc.builder->CreateAdd(
                loadedVal,
                delta,
                "incdec.tmp");

        mc.builder->CreateStore(newVal, symbolInfoAlloca->inst);
        value_ = node->unaryPosType == UnaryOpNode::UnaryOpType::Prefix ? newVal : loadedVal;
    } else if (node->operatorType == TokenType::Minus) {
        value_ = mc.builder->CreateNeg(generate(node->expr.get(), mc));
    } else if (node->operatorType == TokenType::Plus) {
        value_ = generate(node->expr.get(), mc);
    } else {
        throw std::logic_error("Not supported unary operator");
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
                                            generate(node->condBranch.cond.get(), mc),
                                            mc.builder->getInt1Ty());
            mc.builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::While: {
            condBB = llvm::BasicBlock::Create(mc.module->getContext(), "while.cond", parentFunc);
            mc.builder->CreateBr(condBB);
            mc.builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(mc.builder,
                                            generate(node->condBranch.cond.get(), mc),
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
            auto *const cond = generate(node->condBranch.cond.get(), mc);
            mc.builder->CreateCondBr(cond, loopBB, mergeBB);
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
    const auto type = IRTypeFactory::from(node->type, mc.module->getContext());
    auto *const llvmType = type->getLLVMType(mc.module->getContext());
    if (llvmType == nullptr) {
        throw std::logic_error("Unknown type for variable: " + node->ident->name);
    }

    llvm::Value *initValue = nullptr;
    if (node->init.has_value()) {
        initValue = generate(node->init.value().get(), mc);
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
        value_ = genGlobalDeclaration(node, llvmType, initValue, mc);
    } else {
        value_ = genLocalDeclaration(node, llvmType, initValue, mc);
    }
}

void LLVMCodegen::visit(ReturnNode *node) {
    if (node->expr != nullptr) {
        value_ = mc.builder->CreateRet(generate(node->expr.get(), mc));
    } else {
        value_ = mc.builder->CreateRetVoid();
    }
}

void LLVMCodegen::visit(TernaryOperatorNode *node) {
    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();

    auto *const thenBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_then", parentFunc);
    auto *const elseBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_else");
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_merge");

    mc.builder->CreateCondBr(generate(node->cond.get(), mc), thenBB, elseBB);

    mc.builder->SetInsertPoint(thenBB);
    auto *const trueVal = generate(node->trueExpr.get(), mc);
    mc.builder->CreateBr(mergeBB);

    elseBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(elseBB);
    auto *const falseVal = generate(node->falseExpr.get(), mc);
    mc.builder->CreateBr(mergeBB);

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);

    if (trueVal->getType() != falseVal->getType()) {
        throw std::logic_error("Ternary expressions must be of the same type");
    }

    auto *const phi = mc.builder->CreatePHI(trueVal->getType(), 2, "tern_result");
    phi->addIncoming(trueVal, thenBB);
    phi->addIncoming(falseVal, elseBB);
    value_ = phi;
}

void LLVMCodegen::visit(MethodCallNode *node) {
    const auto objectType = IRTypeFactory::from(node->object->getType(), mc.module->getContext());
    std::vector<llvm::Value *> args;
    args.reserve(node->method->args.size());
    for (const auto &arg: node->method->args) {
        args.push_back(generate(arg.get(), mc));
    }

    if (const auto fType = node->method->getType()->asFunction()) {
        const auto methodInfo = MethodInfo::create(node->method->ident->name,
                                                   fType.value());
        auto *object = generate(node->object.get(), mc);
        value_ = objectType->createMethodCall(*mc.builder,
                                              methodInfo,
                                              object,
                                              args);
    }
}

void LLVMCodegen::visit(FieldAccessNode *node) {
    throw std::logic_error("FieldAccessNode not implemented");
}

void LLVMCodegen::visit(CommentNode *node) {
    // skip comments
}

void LLVMCodegen::visit(ModuleNode *node) {}

void LLVMCodegen::visit(TypeCastNode *node) {
    value_ = tryCastValue(mc.builder, generate(node->expr.get(), mc),
                          IRTypeFactory::from(node->targetType, mc.module->getContext())->getLLVMType(
                                  mc.module->getContext()));
}

void LLVMCodegen::visit(ArrayNode *node) {
    const auto arrayType = IRTypeFactory::from(node->getType(), mc.module->getContext());
    value_ = arrayType->createConstant(node, *mc.builder, *mc.module);
}

void LLVMCodegen::visit(IndexAccessNode *node) {
    auto *const objectValue = generate(node->object.get(), mc);
    auto *const indexValue = generate(node->index.get(), mc);

    if (node->object->getType()->isArray()) {
        auto *const llvmArrayType = llvm::dyn_cast<llvm::ArrayType>(
                IRTypeFactory::from(node->object->getType(),
                                    mc.module->getContext())->getLLVMType(mc.module->getContext()));
        const std::vector<llvm::Value *> indices = {
                mc.builder->getInt64(0),
                indexValue
        };
        value_ = mc.builder->CreateLoad(llvmArrayType->getElementType(),
                                        mc.builder->CreateInBoundsGEP(llvmArrayType, objectValue, indices, "elem_ptr"));
    } else {
        throw std::runtime_error("Unsupported index access type");
    }
}

llvm::Value *LLVMCodegen::value() const {
    return value_;
}
