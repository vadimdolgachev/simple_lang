//
// Created by vadim on 06.10.24.
//

#include <iostream>

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include "ast/FunctionNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/NumberNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/BinOpNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/ForLoopNode.h"
#include "ast/IfStatement.h"
#include "ast/BooleanNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/StringNode.h"

#include "LLVMCodegen.h"

namespace {
    llvm::Value *codegenBlock(const std::unique_ptr<BlockNode> &block,
                              const std::unique_ptr<llvm::LLVMContext> &llvmContext,
                              const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
                              const std::unique_ptr<llvm::Module> &llvmModule,
                              std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                              std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &
                              functionProtos,
                              std::unordered_map<std::string, llvm::Value *> &namedValues) {

        for (auto it = block->statements.begin(); it != block->statements.end(); ++it) {
            auto *const ir = LLVMCodegen::generate(it->get(),
                                                   llvmContext,
                                                   llvmIRBuilder,
                                                   llvmModule,
                                                   globalValues,
                                                   functionProtos,
                                                   namedValues);
            if (*it == block->statements.back() && ir != nullptr) {
                return ir;
            }
        }
        return nullptr;
    }

    llvm::Function *getModuleFunction(const std::string &name,
                                      const std::unique_ptr<llvm::LLVMContext> &llvmContext,
                                      const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
                                      const std::unique_ptr<llvm::Module> &llvmModule,
                                      std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                                      std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &
                                      functionProtos,
                                      std::unordered_map<std::string, llvm::Value *> &namedValues) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = llvmModule->getFunction(name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        if (const auto proto = functionProtos.find(name); proto != functionProtos.end()) {
            auto *const ir = LLVMCodegen::generate(proto->second.get(),
                                                   llvmContext,
                                                   llvmIRBuilder,
                                                   llvmModule,
                                                   globalValues,
                                                   functionProtos,
                                                   namedValues);
            return reinterpret_cast<llvm::Function *>(ir);
        }

        // If no existing prototype exists, return null.
        return nullptr;
    }
} // namespace

LLVMCodegen::LLVMCodegen(
        const std::unique_ptr<llvm::LLVMContext> &llvmContext,
        const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
        const std::unique_ptr<llvm::Module> &llvmModule,
        std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
        std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &functionProtos,
        std::unordered_map<std::string, llvm::Value *> &namedValues):
    llvmContext(llvmContext),
    llvmIRBuilder(llvmIRBuilder),
    llvmModule(llvmModule),
    globalValues(globalValues),
    functionProtos(functionProtos),
    namedValues(namedValues) {}

void LLVMCodegen::visit(const IdentNode *node) {
    if (globalValues.contains(node->name)) {
        value_ = llvmIRBuilder->CreateLoad(globalValues[node->name]->getValueType(),
                                           globalValues[node->name],
                                           node->name + ".global");
    } else {
        auto *const variable = dyn_cast<llvm::AllocaInst>(namedValues[node->name]);
        value_ = llvmIRBuilder->CreateLoad(variable->getAllocatedType(), variable);
    }
}

void LLVMCodegen::visit(const FunctionNode *const node) {
    assert(llvmContext != nullptr);
    const auto &proto = *node->proto;
    functionProtos[proto.name] = std::make_unique<ProtoFunctionStatement>(node->proto->name, node->proto->params);
    auto *const function = getModuleFunction(proto.name,
                                             llvmContext,
                                             llvmIRBuilder,
                                             llvmModule,
                                             globalValues,
                                             functionProtos,
                                             namedValues);
    if (function == nullptr) {
        return;
    }

    // Create a new basic block to start insertion into.
    auto *const basicBlock = llvm::BasicBlock::Create(*llvmContext, "entry", function);
    llvmIRBuilder->SetInsertPoint(basicBlock);

    // Record the function arguments in the namedValues map.
    namedValues.clear();
    for (auto &arg: function->args()) {
        namedValues[std::string(arg.getName())] = &arg;
    }

    if ([[maybe_unused]] auto *const returnValue = codegenBlock(node->body,
                                                                llvmContext,
                                                                llvmIRBuilder,
                                                                llvmModule,
                                                                globalValues,
                                                                functionProtos,
                                                                namedValues)) {
        llvmIRBuilder->CreateRetVoid();
        verifyFunction(*function);
        value_ = function;
    } else {
        // Error reading body, remove function.
        function->eraseFromParent();
    }
}

void LLVMCodegen::visit(const NumberNode *node) {
    assert(llvmContext != nullptr);
    value_ = llvm::ConstantFP::get(*llvmContext, llvm::APFloat(node->value));
}

void LLVMCodegen::visit(const StringNode *node) {
    auto *strConstant = llvm::ConstantDataArray::getString(*llvmContext, node->str);
    auto *var = new llvm::GlobalVariable(
            *llvmModule, strConstant->getType(), true,
            llvm::GlobalValue::ExternalLinkage, strConstant, "str");

    value_ = llvmIRBuilder->CreateInBoundsGEP(
            strConstant->getType(), var,
            {llvmIRBuilder->getInt32(0), llvmIRBuilder->getInt32(0)});
}

void LLVMCodegen::visit(const BooleanNode *node) {
    value_ = llvm::ConstantInt::getBool(llvmIRBuilder->getInt1Ty(), node->value);
}

void LLVMCodegen::visit(const BinOpNode *node) {
    assert(llvmContext != nullptr);
    auto *lhsValue = generate(node->lhs.get(),
                              llvmContext,
                              llvmIRBuilder,
                              llvmModule,
                              globalValues,
                              functionProtos,
                              namedValues);
    auto *rhsValue = generate(node->rhs.get(),
                              llvmContext,
                              llvmIRBuilder,
                              llvmModule,
                              globalValues,
                              functionProtos,
                              namedValues);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        return;
    }
    if (lhsValue->getType()->isPointerTy()) {
        lhsValue = llvmIRBuilder->CreateLoad(llvm::Type::getDoubleTy(*llvmContext), lhsValue);
    }
    if (rhsValue->getType()->isPointerTy()) {
        rhsValue = llvmIRBuilder->CreateLoad(llvm::Type::getDoubleTy(*llvmContext), rhsValue);
    }

    switch (node->binOp) {
        case TokenType::Plus:
            value_ = llvmIRBuilder->CreateFAdd(lhsValue, rhsValue, "add_tmp");
            return;
        case TokenType::Minus:
            value_ = llvmIRBuilder->CreateFSub(lhsValue, rhsValue, "sub_tmp");
            return;
        case TokenType::Star:
            value_ = llvmIRBuilder->CreateFMul(lhsValue, rhsValue, "mul_tmp");
            return;
        case TokenType::Slash:
            value_ = llvmIRBuilder->CreateFDiv(lhsValue, rhsValue, "div_tmp");
            return;
        case TokenType::LeftAngleBracket:
            lhsValue = llvmIRBuilder->CreateFCmpULT(lhsValue, rhsValue, "cmp_tmp");
        // Convert bool 0/1 to double 0.0 or 1.0
            value_ = llvmIRBuilder->CreateUIToFP(lhsValue, llvm::Type::getDoubleTy(*llvmContext), "bool_tmp");
        default:
            break;
    }
}

void LLVMCodegen::visit(const ProtoFunctionStatement *node) {
    assert(llvmContext != nullptr);
    const std::vector functionParams(node->params.size(), llvmIRBuilder->getVoidTy());
    auto *const functionType = llvm::FunctionType::get(llvmIRBuilder->getVoidTy(),
                                                       functionParams,
                                                       functionProtos[node->name]->isVarArgs);
    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  llvmModule.get());
    for (auto *it = function->arg_begin(); it != function->arg_end(); ++it) {
        const auto index = std::distance(function->arg_begin(), it);
        it->setName(node->params[index]);
    }
    value_ = function;
}

void LLVMCodegen::visit(const AssignmentNode *const node) {
    assert(llvmContext != nullptr);
    auto *const init = generate(node->rvalue.get(),
                                llvmContext,
                                llvmIRBuilder,
                                llvmModule,
                                globalValues,
                                functionProtos,
                                namedValues);
    if (llvmIRBuilder->GetInsertBlock() == nullptr) {
        auto *const variable = new llvm::GlobalVariable(
                *llvmModule,
                init->getType(),
                true,
                llvm::GlobalValue::ExternalLinkage,
                nullptr,
                node->name);
        variable->setInitializer(dyn_cast<llvm::Constant>(init));
        globalValues[node->name] = variable;
        value_ = variable;
    } else {
        auto *const variable = new llvm::AllocaInst(init->getType(), 0, node->name,
                                                    llvmIRBuilder->GetInsertBlock());

        llvmIRBuilder->CreateStore(init, variable);
        namedValues[node->name] = variable;
        value_ = variable;
    }
}

void LLVMCodegen::visit(const FunctionCallNode *const node) {
    assert(llvmContext != nullptr);
    // Look up the name in the global module table.
    auto *calleeFunc = getModuleFunction(node->ident->name,
                                         llvmContext,
                                         llvmIRBuilder,
                                         llvmModule,
                                         globalValues,
                                         functionProtos,
                                         namedValues);
    if (calleeFunc == nullptr) {
        return;
    }

    // If argument mismatch error.
    if (!functionProtos[node->ident->name]->isVarArgs && calleeFunc->arg_size() != node->args.size()) {
        return;
    }

    std::vector<llvm::Value *> argsFunc;
    for (const auto &arg: node->args) {
        argsFunc.push_back(
                generate(arg.get(),
                         llvmContext,
                         llvmIRBuilder,
                         llvmModule,
                         globalValues,
                         functionProtos,
                         namedValues));
        if (!argsFunc.back()) {
            return;
        }
    }

    value_ = llvmIRBuilder->CreateCall(calleeFunc, argsFunc);
}

void LLVMCodegen::visit(const IfStatement *node) {
    auto *condValue = generate(node->ifBranch.cond.get(), llvmContext, llvmIRBuilder, llvmModule, globalValues,
                               functionProtos,
                               namedValues);
    if (condValue == nullptr) {
        return;
    }
    condValue = llvmIRBuilder->CreateFCmpONE(condValue,
                                             llvm::ConstantFP::get(*llvmContext,
                                                                   llvm::APFloat(0.0)),
                                             "if_cond");
    auto *const insertBlock = llvmIRBuilder->GetInsertBlock();
    if (insertBlock == nullptr) {
        return;
    }
    auto *const function = insertBlock->getParent();
    auto *thenBasicBlock = llvm::BasicBlock::Create(*llvmContext, "thenBasicBlock", function);
    auto *elseBasicBlock = llvm::BasicBlock::Create(*llvmContext, "elseBasicBlock");
    auto *const finishBasicBlock = llvm::BasicBlock::Create(*llvmContext, "finishBasicBlock");

    // if condition
    llvmIRBuilder->CreateCondBr(condValue, thenBasicBlock, elseBasicBlock);

    // then base block
    llvmIRBuilder->SetInsertPoint(thenBasicBlock);
    auto *const thenValue = codegenBlock(node->ifBranch.then,
                                         llvmContext,
                                         llvmIRBuilder,
                                         llvmModule,
                                         globalValues,
                                         functionProtos,
                                         namedValues);
    if (thenValue == nullptr) {
        return;
    }
    llvmIRBuilder->CreateBr(finishBasicBlock);
    thenBasicBlock = llvmIRBuilder->GetInsertBlock();

    // else base block
    function->insert(function->end(), elseBasicBlock);
    llvmIRBuilder->SetInsertPoint(elseBasicBlock);
    auto *const elseValue = node->elseBranch.has_value()
                                ? codegenBlock(node->elseBranch.value(),
                                               llvmContext,
                                               llvmIRBuilder,
                                               llvmModule,
                                               globalValues,
                                               functionProtos,
                                               namedValues)
                                : nullptr;
    llvmIRBuilder->CreateBr(finishBasicBlock);
    elseBasicBlock = llvmIRBuilder->GetInsertBlock();

    // merge base block
    function->insert(function->end(), finishBasicBlock);
    llvmIRBuilder->SetInsertPoint(finishBasicBlock);

    // phi node
    auto *const phiNode =
            llvmIRBuilder->CreatePHI(llvm::Type::getDoubleTy(*llvmContext), 2, "if_tmp");
    phiNode->addIncoming(thenValue, thenBasicBlock);
    phiNode->addIncoming(
            elseValue ? elseValue : llvm::ConstantFP::getNullValue(llvm::Type::getDoubleTy(*llvmContext)),
            elseBasicBlock);
    value_ = phiNode;
}

void LLVMCodegen::visit(const ForLoopNode *node) {
    assert(llvmIRBuilder->GetInsertBlock());
    auto *const currFunction = llvmIRBuilder->GetInsertBlock()->getParent();
    auto *const beforeLoopBB = llvmIRBuilder->GetInsertBlock();
    auto *const loopBB = llvm::BasicBlock::Create(*llvmContext,
                                                  "for_loop",
                                                  currFunction);
    llvmIRBuilder->CreateBr(loopBB);
    llvmIRBuilder->SetInsertPoint(loopBB);

    const auto *const initVarAst = dynamic_cast<const AssignmentNode *>(node->init.get());
    if (initVarAst == nullptr) {
        return;
    }
    auto *const loopVarValue = llvmIRBuilder->CreatePHI(llvm::Type::getDoubleTy(*llvmContext),
                                                        2,
                                                        initVarAst->name);
    auto *const OldVar = namedValues[initVarAst->name];
    namedValues[initVarAst->name] = loopVarValue;

    auto *const initValue = generate(initVarAst->rvalue.get(),
                                     llvmContext,
                                     llvmIRBuilder,
                                     llvmModule,
                                     globalValues,
                                     functionProtos,
                                     namedValues);
    if (initValue == nullptr) {
        return;
    }
    loopVarValue->addIncoming(initValue, beforeLoopBB);
    if (codegenBlock(node->body,
                     llvmContext,
                     llvmIRBuilder,
                     llvmModule,
                     globalValues,
                     functionProtos,
                     namedValues) ==
        nullptr) {
        return;
    }

    llvm::Value *nextValue;
    if (node->next) {
        nextValue = generate(node->next.get(),
                             llvmContext,
                             llvmIRBuilder,
                             llvmModule,
                             globalValues,
                             functionProtos,
                             namedValues);
        if (nextValue == nullptr) {
            return;
        }
    } else {
        nextValue = llvmIRBuilder->CreateFAdd(loopVarValue,
                                              llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0)),
                                              "next_var");
    }

    auto *condExprValue = generate(node->conditional.get(),
                                   llvmContext,
                                   llvmIRBuilder,
                                   llvmModule,
                                   globalValues,
                                   functionProtos,
                                   namedValues);
    if (condExprValue == nullptr) {
        return;
    }
    condExprValue = llvmIRBuilder->CreateFCmpONE(
            condExprValue, llvm::ConstantFP::get(*llvmContext, llvm::APFloat(0.0)),
            "loop_cond");

    auto *const loopEndBB = llvmIRBuilder->GetInsertBlock();
    loopVarValue->addIncoming(nextValue, loopEndBB);

    auto *const afterLoopBB = llvm::BasicBlock::Create(*llvmContext,
                                                       "after_loop",
                                                       currFunction);
    llvmIRBuilder->CreateCondBr(condExprValue, loopBB, afterLoopBB);
    llvmIRBuilder->SetInsertPoint(afterLoopBB);

    if (OldVar != nullptr) {
        namedValues[initVarAst->name] = OldVar;
    } else {
        namedValues.erase(initVarAst->name);
    }
    value_ = llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*llvmContext));
}

void LLVMCodegen::visit(const UnaryOpNode *node) {
    if (node->operatorType == TokenType::IncrementOperator) {
        value_ = llvmIRBuilder->CreateFAdd(generate(node->expr.get(),
                                                    llvmContext,
                                                    llvmIRBuilder,
                                                    llvmModule,
                                                    globalValues,
                                                    functionProtos,
                                                    namedValues),
                                           llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0)), "increment");
    } else if (node->operatorType == TokenType::DecrementOperator) {
        value_ = llvmIRBuilder->CreateFSub(
                generate(node->expr.get(),
                         llvmContext,
                         llvmIRBuilder,
                         llvmModule,
                         globalValues,
                         functionProtos,
                         namedValues),
                llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0)), "decrement");
    }
}

void LLVMCodegen::visit(const LoopCondNode *node) {
    throw std::runtime_error("not implemented");
}

void LLVMCodegen::visit(const BlockNode *node) {
    throw std::runtime_error("not implemented");
}

llvm::Value *LLVMCodegen::value() const {
    return value_;
}
