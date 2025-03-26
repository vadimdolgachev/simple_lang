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
#include "ast/TypeNode.h"

#include "LLVMCodegen.h"

#include "IRType.h"
#include "TypeFactory.h"
#include "ast/LoopCondNode.h"
#include "ast/ReturnNode.h"
#include "ast/TernaryOperatorNode.h"

namespace {
    llvm::Function *getModuleFunction(const std::string &name,
                                      const std::unique_ptr<llvm::IRBuilder<>> &builder,
                                      const std::unique_ptr<llvm::Module> &module,
                                      ModuleContext &mc) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = module->getFunction(name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        if (const auto *const proto = mc.symTable.lookupFunction(name)) {
            auto *const fun = LLVMCodegen::generate(proto,
                                                    builder,
                                                    module,
                                                    mc);
            return llvm::dyn_cast<llvm::Function>(fun);
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
                            const std::unique_ptr<llvm::IRBuilder<>> &builder,
                            const std::unique_ptr<llvm::Module> &module,
                            ModuleContext &mc,
                            const std::optional<std::function<void()>> &prologue = std::nullopt) {
        mc.symTable.enterScope();

        builder->SetInsertPoint(basicBlock);
        if (prologue.has_value()) {
            (*prologue)();
        }

        for (const auto &stmt: statements) {
            [[maybe_unused]] auto *const val = LLVMCodegen::generate(
                    stmt.get(), builder, module, mc);
        }

        mc.symTable.exitScope();
    }


    llvm::GlobalVariable *genGlobalDeclaration(const DeclarationNode *node,
                                               llvm::Type *type,
                                               llvm::Value *init,
                                               const std::unique_ptr<llvm::Module> &module,
                                               ModuleContext &mc) {
        llvm::Constant *constInit = nullptr;
        if (init) {
            constInit = llvm::dyn_cast<llvm::Constant>(init);
            if (!constInit) {
                throw std::logic_error(
                        "Global variable initializer must be constant: " + node->ident->name);
            }
        }

        auto *gVar = new llvm::GlobalVariable(*module,
                                              type,
                                              true,
                                              llvm::GlobalValue::InternalLinkage,
                                              constInit,
                                              node->ident->name);

        gVar->setAlignment(llvm::MaybeAlign(8));
        gVar->setDSOLocal(true);

        mc.symTable.insert(node->ident->name, node->type, gVar);
        return gVar;
    }

    llvm::AllocaInst *genLocalDeclaration(const DeclarationNode *node,
                                          llvm::Type *type,
                                          llvm::Value *init,
                                          const std::unique_ptr<llvm::IRBuilder<>> &builder,
                                          ModuleContext &mc) {
        auto *alloca = builder->CreateAlloca(type, nullptr, node->ident->name);

        if (init) {
            auto *const casted = tryCastValue(builder, init, type);
            if (!casted) {
                throw std::logic_error("Type mismatch in initialization of: " + node->ident->name);
            }
            builder->CreateStore(casted, alloca);
        }

        if (mc.symTable.lookup(node->ident->name)) {
            throw std::logic_error("Redeclaration of variable: " + node->ident->name);
        }

        mc.symTable.insert(node->ident->name,
                           node->type,
                           alloca);
        return alloca;
    }

    void processFunctionParameters(llvm::Function *func,
                                   llvm::BasicBlock *basicBlock,
                                   const FunctionNode *node,
                                   const std::unique_ptr<llvm::IRBuilder<>> &builder,
                                   const std::unique_ptr<llvm::Module> &module,
                                   ModuleContext &mc) {
        builder->SetInsertPoint(basicBlock);

        for (auto &arg: func->args()) {
            const auto &paramType = node->proto->params[arg.getArgNo()].type;
            auto *const alloca = builder->CreateAlloca(
                    TypeFactory::from(paramType)->getLLVMType(module->getContext()),
                    nullptr,
                    arg.getName());

            builder->CreateStore(&arg, alloca);

            if (mc.symTable.lookup(std::string(arg.getName()))) {
                throw std::logic_error("Duplicate parameter name: " + std::string(arg.getName()));
            }
            mc.symTable.insert(std::string(arg.getName()), paramType, alloca);
        }
    }
} // namespace

LLVMCodegen::LLVMCodegen(const std::unique_ptr<llvm::IRBuilder<>> &builder,
                         const std::unique_ptr<llvm::Module> &module,
                         ModuleContext &mc):
    builder(builder),
    module(module),
    mc(mc) {}

void LLVMCodegen::visit(const IdentNode *node) {
    if (const auto gv = mc.symTable.lookupGlobal(node->name)) {
        value_ = builder->CreateLoad(gv->value->getValueType(),
                                     gv->value,
                                     node->name + ".global");
    } else {
        auto *const alloc = mc.symTable.lookup(node->name)->value;
        if (alloc == nullptr) {
            throw std::runtime_error(std::format("Unknown variable name: {}", node->name));
        }
        value_ = builder->CreateLoad(alloc->getAllocatedType(), alloc, node->name);
    }
}

void LLVMCodegen::visit(const FunctionNode *const node) {
    auto *const func = llvm::dyn_cast<llvm::Function>(
            generate(node->proto.get(), builder, module, mc));

    if (!func) {
        throw std::logic_error("Function prototype generation failed for: " + node->proto->name);
    }
    auto *const basicBlock = llvm::BasicBlock::Create(module->getContext(),
                                                      "entry",
                                                      func);

    generateBasicBlock(basicBlock,
                       node->body->statements,
                       builder,
                       module,
                       mc,
                       [&]() {
                           processFunctionParameters(func, basicBlock, node, builder, module, mc);
                       });

    if (node->proto->returnType.isVoid()) {
        builder->CreateRetVoid();
    }

    std::string verifyError;
    llvm::raw_string_ostream os(verifyError);
    if (verifyFunction(*func, &os)) {
        throw std::logic_error("Function verification failed:\n" + os.str());
    }

    value_ = func;
}

void LLVMCodegen::visit(const NumberNode *node) {
    if (node->isFloat) {
        value_ = llvm::ConstantFP::get(TypeFactory::from(TypeKind::Double, false)->getLLVMType(module->getContext()),
                                       llvm::APFloat(node->value));
    } else {
        value_ = llvm::ConstantInt::get(TypeFactory::from(TypeKind::Integer, false)->getLLVMType(module->getContext()),
                                        llvm::APInt(32, static_cast<int64_t>(node->value),
                                                    true));
    }
}

void LLVMCodegen::visit(const StringNode *node) {
    auto *strConstant = llvm::ConstantDataArray::getString(module->getContext(), node->str);
    auto *var = new llvm::GlobalVariable(*module,
                                         strConstant->getType(),
                                         true,
                                         llvm::GlobalValue::ExternalLinkage,
                                         strConstant,
                                         "str");

    value_ = builder->CreateInBoundsGEP(strConstant->getType(),
                                        var,
                                        {builder->getInt32(0), builder->getInt32(0)});
}

void LLVMCodegen::visit(const BooleanNode *node) {
    value_ = llvm::ConstantInt::getBool(
            TypeFactory::from(TypeKind::Boolean, false)->getLLVMType(module->getContext()),
            node->value);
}

void LLVMCodegen::visit(const BinOpNode *node) {
    auto *lhsValue = generate(node->lhs.get(),
                              builder,
                              module,
                              mc);
    auto *rhsValue = generate(node->rhs.get(),
                              builder,
                              module,
                              mc);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        throw std::logic_error("Unexpected expression");
    }
    if (lhsValue->getType()->isPointerTy() || rhsValue->getType()->isPointerTy()) {
        throw std::logic_error("Unsupported operation");
    }

    const auto resultTypeNode = TypeFactory::from(node, mc);
    lhsValue = tryCastValue(builder, lhsValue, resultTypeNode->getLLVMType(module->getContext()));
    rhsValue = tryCastValue(builder, rhsValue, resultTypeNode->getLLVMType(module->getContext()));
    value_ = resultTypeNode->createBinaryOp(*builder, node->binOp, lhsValue, rhsValue, "binOp");
}

void LLVMCodegen::visit(const ProtoFunctionStatement *node) {
    std::vector<llvm::Type *> functionParams;
    functionParams.reserve(node->params.size());
    for (const auto &param: node->params) {
        functionParams.push_back(TypeFactory::from(param.type)->getLLVMType(module->getContext()));
    }
    auto *const functionType = llvm::FunctionType::get(
            TypeFactory::from(node->returnType)->getLLVMType(module->getContext()),
            functionParams,
            node->isVarArgs);
    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  module.get());
    // function->addFnAttr(llvm::Attribute::NoUnwind);
    // function->addRetAttr(llvm::Attribute::ZExt);
    for (auto *it = function->arg_begin(); it != function->arg_end(); ++it) {
        const auto index = std::distance(function->arg_begin(), it);
        it->setName(node->params[index].ident->name);
    }
    value_ = function;
}

void LLVMCodegen::visit(const AssignmentNode *const node) {
    auto *const init = generate(node->rvalue.get(),
                                builder,
                                module,
                                mc);
    if (builder->GetInsertBlock() == nullptr) {} else {
        if (const auto var = mc.symTable.lookup(node->lvalue->name)) {
            builder->CreateStore(tryCastValue(builder, init, var->value->getAllocatedType()),
                                 var->value);
            value_ = var->value;
        } else if (const auto gVar = mc.symTable.lookupGlobal(node->lvalue->name)) {
            if (gVar->value->isConstant()) {
                throw std::logic_error("Variable: " + node->lvalue->name + " is constant");
            }
            builder->CreateStore(tryCastValue(builder, init, gVar->value->getValueType()),
                                 gVar->value);
            value_ = gVar->value;
        } else {
            throw std::logic_error("Undefined variable: " + node->lvalue->name);
        }
    }
}

void LLVMCodegen::visit(const FunctionCallNode *const node) {
    auto *calleeFunc = getModuleFunction(node->ident->name,
                                         builder,
                                         module,
                                         mc);
    if (calleeFunc == nullptr) {
        throw std::runtime_error(std::format("Undefined reference: '{}'", node->ident->name));
    }

    // If argument mismatch error.
    if (!calleeFunc->isVarArg() && calleeFunc->arg_size() != node->args.size()) {
        throw std::logic_error("Argument mismatch error");
    }

    std::vector<llvm::Value *> argsFunc;
    argsFunc.reserve(node->args.size());
    const auto *const funcType = calleeFunc->getFunctionType();
    for (size_t i = 0; i < node->args.size(); ++i) {
        auto *argValue = generate(node->args[i].get(),
                                  builder,
                                  module,
                                  mc);
        if (i < funcType->getNumParams()) {
            argValue = tryCastValue(builder, argValue, funcType->getParamType(i));
        }
        argsFunc.push_back(argValue);
    }

    value_ = builder->CreateCall(calleeFunc, argsFunc);
}

void LLVMCodegen::visit(const IfStatement *node) {
    auto *const firstCV = tryCastValue(builder,
                                       generate(node->ifBranch.cond.get(), builder, module, mc),
                                       builder->getInt1Ty());
    if (!firstCV) {
        throw std::logic_error("Condition must be boolean type");
    }

    auto *const parentFunc = builder->GetInsertBlock()->getParent();

    auto *const firstIfBB = llvm::BasicBlock::Create(module->getContext(), "if", parentFunc);
    auto *lastElseBB = llvm::BasicBlock::Create(module->getContext(), "else");
    auto *const mergeBB = llvm::BasicBlock::Create(module->getContext(), "merge_if");

    value_ = builder->CreateCondBr(firstCV, firstIfBB, lastElseBB);

    builder->SetInsertPoint(firstIfBB);
    generate(node->ifBranch.then.get(), builder, module, mc);

    if (!builder->GetInsertBlock()->getTerminator()) {
        builder->CreateBr(mergeBB);
    }

    for (size_t i = 0; i < node->elseIfBranches.size(); ++i) {
        lastElseBB->insertInto(parentFunc);
        builder->SetInsertPoint(lastElseBB);

        const auto &[cond, then] = node->elseIfBranches[i];
        auto *const value = tryCastValue(builder,
                                         generate(cond.get(), builder, module, mc),
                                         builder->getInt1Ty());
        if (!value) {
            throw std::logic_error("Condition must be boolean type");
        }
        auto *const ifBB = llvm::BasicBlock::Create(module->getContext(),
                                                    "elif_" + std::to_string(i), parentFunc);
        lastElseBB = llvm::BasicBlock::Create(module->getContext(), "else_" + std::to_string(i));
        builder->CreateCondBr(value, ifBB, lastElseBB);

        builder->SetInsertPoint(ifBB);
        generate(then.get(), builder, module, mc);

        if (!builder->GetInsertBlock()->getTerminator()) {
            builder->CreateBr(mergeBB);
        }
    }

    lastElseBB->insertInto(parentFunc);
    builder->SetInsertPoint(lastElseBB);
    if (node->elseBranch.has_value()) {
        generate(node->elseBranch.value().get(), builder, module, mc);
    }
    if (!builder->GetInsertBlock()->getTerminator()) {
        builder->CreateBr(mergeBB);
    }

    mergeBB->insertInto(parentFunc);
    builder->SetInsertPoint(mergeBB);
}

void LLVMCodegen::visit(const UnaryOpNode *node) {
    if (node->operatorType == TokenType::IncrementOperator
        || node->operatorType == TokenType::DecrementOperator) {
        const auto *ident = dynamic_cast<IdentNode *>(node->expr.get());
        if (ident == nullptr) {
            throw std::logic_error("Increment/decrement requires lvalue variable");
        }

        const auto var = mc.symTable.lookup(ident->name);
        if (var == std::nullopt) {
            throw std::logic_error("Undefined variable: " + ident->name);
        }

        auto *const varType = var->value->getAllocatedType();

        if (!varType->isIntOrIntVectorTy() &&
            !varType->isFPOrFPVectorTy() &&
            !varType->isPointerTy()) {
            throw std::logic_error("Invalid type for increment/decrement");
        }

        auto *const loadedVal = builder->CreateLoad(varType,
                                                    var->value,
                                                    ident->name + ".val");

        llvm::Value *delta = nullptr;
        if (varType->isIntegerTy()) {
            delta = llvm::ConstantInt::get(varType, 1);
        } else {
            delta = llvm::ConstantFP::get(varType, 1.0);
        }
        if (node->operatorType == TokenType::DecrementOperator) {
            delta = builder->CreateNeg(delta, "neg.tmp");
        }

        auto *const newVal = builder->CreateAdd(
                loadedVal,
                delta,
                "incdec.tmp");

        builder->CreateStore(newVal, var->value);
        value_ = node->unaryPosType == UnaryOpNode::UnaryOpType::Prefix ? newVal : loadedVal;
    } else if (node->operatorType == TokenType::Minus) {
        value_ = builder->CreateNeg(generate(node->expr.get(),
                                             builder,
                                             module,
                                             mc));
    } else if (node->operatorType == TokenType::Plus) {
        value_ = generate(node->expr.get(),
                          builder,
                          module,
                          mc);
    } else {
        throw std::logic_error("Not supported unary operator");
    }
}

void LLVMCodegen::visit(const LoopCondNode *node) {
    auto *const parentFunc = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB = nullptr;
    auto *const loopBB = llvm::BasicBlock::Create(module->getContext(), "loop", parentFunc);
    auto *const mergeBB = llvm::BasicBlock::Create(module->getContext(), "merge");

    if (node->loopType == LoopCondNode::Type::For) {
        if (node->init) {
            generate(node->init->get(), builder, module, mc);
        }
        condBB = llvm::BasicBlock::Create(module->getContext(), "for.cond", parentFunc);
        builder->CreateBr(condBB);
    }

    switch (node->loopType) {
        case LoopCondNode::Type::For: {
            builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(builder,
                                            generate(node->condBranch.cond.get(), builder, module,
                                                     mc),
                                            builder->getInt1Ty());
            builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::While: {
            condBB = llvm::BasicBlock::Create(module->getContext(), "while.cond", parentFunc);
            builder->CreateBr(condBB);
            builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(builder,
                                            generate(node->condBranch.cond.get(), builder, module,
                                                     mc),
                                            builder->getInt1Ty());
            builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::DoWhile: {
            builder->CreateBr(loopBB);
            break;
        }
    }

    builder->SetInsertPoint(loopBB);
    generate(node->condBranch.then.get(), builder, module, mc);

    switch (node->loopType) {
        case LoopCondNode::Type::For: {
            if (node->increment) {
                generate(node->increment->get(), builder, module, mc);
            }
            builder->CreateBr(condBB);
            break;
        }
        case LoopCondNode::Type::While: {
            builder->CreateBr(condBB);
            break;
        }
        case LoopCondNode::Type::DoWhile: {
            auto *const cond = generate(node->condBranch.cond.get(), builder, module, mc);
            builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
    }

    if (!loopBB->getTerminator()) {
        builder->CreateBr(mergeBB);
    }

    mergeBB->insertInto(parentFunc);
    builder->SetInsertPoint(mergeBB);
}

void LLVMCodegen::visit(const BlockNode *node) {
    if (!builder->GetInsertBlock()) {
        throw std::logic_error("Block generation outside of function context");
    }

    generateBasicBlock(builder->GetInsertBlock(),
                       node->statements,
                       builder,
                       module,
                       mc);
}

void LLVMCodegen::visit(const DeclarationNode *node) {
    const auto type = TypeFactory::from(node->type);
    auto *const llvmType = type->getLLVMType(module->getContext());
    if (llvmType == nullptr) {
        throw std::logic_error("Unknown type for variable: " + node->ident->name);
    }

    llvm::Value *initValue = nullptr;
    if (node->init.has_value()) {
        initValue = generate(node->init.value().get(), builder, module, mc);
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

    // type->createStore(node->ident, type->defaultValue(), builder);

    if (builder->GetInsertBlock() == nullptr) {
        value_ = genGlobalDeclaration(node, llvmType, initValue, module, mc);
    } else {
        value_ = genLocalDeclaration(node, llvmType, initValue, builder, mc);
    }
}

void LLVMCodegen::visit(const ReturnNode *node) {
    if (node->expr != nullptr) {
        value_ = builder->CreateRet(generate(node->expr.get(), builder, module, mc));
    } else {
        value_ = builder->CreateRetVoid();
    }
}

void LLVMCodegen::visit(const TernaryOperatorNode *node) {
    auto *const parentFunc = builder->GetInsertBlock()->getParent();

    auto *const thenBB = llvm::BasicBlock::Create(module->getContext(), "tern_then", parentFunc);
    auto *const elseBB = llvm::BasicBlock::Create(module->getContext(), "tern_else");
    auto *const mergeBB = llvm::BasicBlock::Create(module->getContext(), "tern_merge");

    builder->CreateCondBr(generate(node->cond.get(), builder, module, mc), thenBB, elseBB);

    builder->SetInsertPoint(thenBB);
    auto *const trueVal = generate(node->trueExpr.get(), builder, module, mc);
    builder->CreateBr(mergeBB);

    elseBB->insertInto(parentFunc);
    builder->SetInsertPoint(elseBB);
    auto *const falseVal = generate(node->falseExpr.get(), builder, module, mc);
    builder->CreateBr(mergeBB);

    mergeBB->insertInto(parentFunc);
    builder->SetInsertPoint(mergeBB);

    if (trueVal->getType() != falseVal->getType()) {
        throw std::logic_error("Ternary expressions must be of the same type");
    }

    auto *const phi = builder->CreatePHI(trueVal->getType(), 2, "tern_result");
    phi->addIncoming(trueVal, thenBB);
    phi->addIncoming(falseVal, elseBB);
    value_ = phi;
}

llvm::Value *LLVMCodegen::value() const {
    return value_;
}
