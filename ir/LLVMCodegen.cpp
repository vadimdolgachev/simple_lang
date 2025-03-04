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
                              const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                              const std::unique_ptr<llvm::Module> &module,
                              std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                              std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &
                              functionProtos,
                              std::unordered_map<std::string, llvm::AllocaInst *> &namedValues) {

        for (auto it = block->statements.begin(); it != block->statements.end(); ++it) {
            auto *const ir = LLVMCodegen::generate(it->get(),
                                                   iRBuilder,
                                                   module,
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
                                      const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                                      const std::unique_ptr<llvm::Module> &module,
                                      std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                                      std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &
                                      functionProtos,
                                      std::unordered_map<std::string, llvm::AllocaInst *> &namedValues) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = module->getFunction(name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        if (const auto proto = functionProtos.find(name); proto != functionProtos.end()) {
            auto *const ir = LLVMCodegen::generate(proto->second.get(),
                                                   iRBuilder,
                                                   module,
                                                   globalValues,
                                                   functionProtos,
                                                   namedValues);
            return llvm::dyn_cast<llvm::Function>(ir);
        }

        // If no existing prototype exists, return null.
        return nullptr;
    }

    llvm::AllocaInst *createEntryBlockAlloca(llvm::Type *type,
                                             llvm::Function *function,
                                             const llvm::StringRef varName) {
        llvm::IRBuilder tmpBuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
        return tmpBuilder.CreateAlloca(type, nullptr, varName);
    }
} // namespace

LLVMCodegen::LLVMCodegen(const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                         const std::unique_ptr<llvm::Module> &module,
                         std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                         std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &functionProtos,
                         std::unordered_map<std::string, llvm::AllocaInst *> &namedValues):
    iRBuilder(iRBuilder),
    module(module),
    globalValues(globalValues),
    functionProtos(functionProtos),
    namedValues(namedValues) {}

void LLVMCodegen::visit(const IdentNode *node) {
    if (globalValues.contains(node->name)) {
        value_ = iRBuilder->CreateLoad(globalValues[node->name]->getValueType(),
                                       globalValues[node->name],
                                       node->name + ".global");
    } else {
        auto *const alloc = namedValues[node->name];
        if (alloc == nullptr) {
            throw std::runtime_error(std::format("Unknown variable name: {}", node->name));
        }
        value_ = iRBuilder->CreateLoad(alloc->getAllocatedType(), alloc, node->name);
    }
}

void LLVMCodegen::visit(const FunctionNode *const node) {
    const auto &proto = *node->proto;
    functionProtos[proto.name] = std::make_unique<ProtoFunctionStatement>(node->proto->name, node->proto->params);
    auto *const function = getModuleFunction(proto.name,
                                             iRBuilder,
                                             module,
                                             globalValues,
                                             functionProtos,
                                             namedValues);
    if (function == nullptr) {
        return;
    }

    // Create a new basic block to start insertion into.
    auto *const basicBlock = llvm::BasicBlock::Create(module->getContext(), "entry", function);
    iRBuilder->SetInsertPoint(basicBlock);

    // Record the function arguments in the namedValues map.
    namedValues.clear();
    for (auto &arg: function->args()) {
        auto *const alloca = createEntryBlockAlloca(llvm::Type::getDoubleTy(module->getContext()), function,
                                                    arg.getName());
        iRBuilder->CreateStore(&arg, alloca);
        namedValues[std::string(arg.getName())] = alloca;
    }

    if ([[maybe_unused]] auto *const returnValue = codegenBlock(node->body,
                                                                iRBuilder,
                                                                module,
                                                                globalValues,
                                                                functionProtos,
                                                                namedValues)) {
        iRBuilder->CreateRetVoid();
        verifyFunction(*function);
        value_ = function;
    } else {
        // Error reading body, remove function.
        function->eraseFromParent();
    }
}

void LLVMCodegen::visit(const NumberNode *node) {
    value_ = llvm::ConstantFP::get(module->getContext(), llvm::APFloat(node->value));
}

void LLVMCodegen::visit(const StringNode *node) {
    auto *strConstant = llvm::ConstantDataArray::getString(module->getContext(), node->str);
    auto *var = new llvm::GlobalVariable(
            *module, strConstant->getType(), true,
            llvm::GlobalValue::ExternalLinkage, strConstant, "str");

    value_ = iRBuilder->CreateInBoundsGEP(
            strConstant->getType(), var,
            {iRBuilder->getInt32(0), iRBuilder->getInt32(0)});
}

void LLVMCodegen::visit(const BooleanNode *node) {
    value_ = llvm::ConstantInt::getBool(iRBuilder->getInt1Ty(), node->value);
}

void LLVMCodegen::visit(const BinOpNode *node) {
    auto *lhsValue = generate(node->lhs.get(),
                              iRBuilder,
                              module,
                              globalValues,
                              functionProtos,
                              namedValues);
    auto *rhsValue = generate(node->rhs.get(),
                              iRBuilder,
                              module,
                              globalValues,
                              functionProtos,
                              namedValues);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        return;
    }
    if (lhsValue->getType()->isPointerTy()) {
        lhsValue = iRBuilder->CreateLoad(llvm::Type::getDoubleTy(module->getContext()), lhsValue);
    }
    if (rhsValue->getType()->isPointerTy()) {
        rhsValue = iRBuilder->CreateLoad(llvm::Type::getDoubleTy(module->getContext()), rhsValue);
    }

    switch (node->binOp) {
        case TokenType::Plus:
            value_ = iRBuilder->CreateFAdd(lhsValue, rhsValue, "add_tmp");
            return;
        case TokenType::Minus:
            value_ = iRBuilder->CreateFSub(lhsValue, rhsValue, "sub_tmp");
            return;
        case TokenType::Star:
            value_ = iRBuilder->CreateFMul(lhsValue, rhsValue, "mul_tmp");
            return;
        case TokenType::Slash:
            value_ = iRBuilder->CreateFDiv(lhsValue, rhsValue, "div_tmp");
            return;
        case TokenType::LeftAngleBracket:
            lhsValue = iRBuilder->CreateFCmpULT(lhsValue, rhsValue, "cmp_tmp");
        // Convert bool 0/1 to double 0.0 or 1.0
            value_ = iRBuilder->CreateUIToFP(lhsValue, llvm::Type::getDoubleTy(module->getContext()), "bool_tmp");
        default:
            break;
    }
}

void LLVMCodegen::visit(const ProtoFunctionStatement *node) {
    const std::vector functionParams(node->params.size(),
                                     node->name.starts_with("print")
                                         ? iRBuilder->getVoidTy()
                                         : iRBuilder->getDoubleTy());
    auto *const functionType = llvm::FunctionType::get(iRBuilder->getVoidTy(),
                                                       functionParams,
                                                       functionProtos[node->name]->isVarArgs);
    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  module.get());
    for (auto *it = function->arg_begin(); it != function->arg_end(); ++it) {
        const auto index = std::distance(function->arg_begin(), it);
        it->setName(node->params[index]);
    }
    value_ = function;
}

void LLVMCodegen::visit(const AssignmentNode *const node) {
    auto *const init = generate(node->rvalue.get(),
                                iRBuilder,
                                module,
                                globalValues,
                                functionProtos,
                                namedValues);
    if (iRBuilder->GetInsertBlock() == nullptr) {
        auto *const variable = new llvm::GlobalVariable(
                *module,
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
                                                    iRBuilder->GetInsertBlock());

        iRBuilder->CreateStore(init, variable);
        namedValues[node->name] = variable;
        value_ = variable;
    }
}

void LLVMCodegen::visit(const FunctionCallNode *const node) {
    // Look up the name in the global module table.
    auto *calleeFunc = getModuleFunction(node->ident->name,
                                         iRBuilder,
                                         module,
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
                         iRBuilder,
                         module,
                         globalValues,
                         functionProtos,
                         namedValues));
        if (!argsFunc.back()) {
            return;
        }
    }

    value_ = iRBuilder->CreateCall(calleeFunc, argsFunc);
}

void LLVMCodegen::visit(const IfStatement *node) {
    auto *condValue = generate(node->ifBranch.cond.get(), iRBuilder, module, globalValues,
                               functionProtos,
                               namedValues);
    if (condValue == nullptr) {
        return;
    }
    condValue = iRBuilder->CreateFCmpONE(condValue,
                                         llvm::ConstantFP::get(module->getContext(),
                                                               llvm::APFloat(0.0)),
                                         "if_cond");
    auto *const insertBlock = iRBuilder->GetInsertBlock();
    if (insertBlock == nullptr) {
        return;
    }
    auto *const function = insertBlock->getParent();
    auto *thenBasicBlock = llvm::BasicBlock::Create(module->getContext(), "thenBasicBlock", function);
    auto *elseBasicBlock = llvm::BasicBlock::Create(module->getContext(), "elseBasicBlock");
    auto *const finishBasicBlock = llvm::BasicBlock::Create(module->getContext(), "finishBasicBlock");

    // if condition
    iRBuilder->CreateCondBr(condValue, thenBasicBlock, elseBasicBlock);

    // then base block
    iRBuilder->SetInsertPoint(thenBasicBlock);
    auto *const thenValue = codegenBlock(node->ifBranch.then,
                                         iRBuilder,
                                         module,
                                         globalValues,
                                         functionProtos,
                                         namedValues);
    if (thenValue == nullptr) {
        return;
    }
    iRBuilder->CreateBr(finishBasicBlock);
    thenBasicBlock = iRBuilder->GetInsertBlock();

    // else base block
    function->insert(function->end(), elseBasicBlock);
    iRBuilder->SetInsertPoint(elseBasicBlock);
    auto *const elseValue = node->elseBranch.has_value()
                                ? codegenBlock(node->elseBranch.value(),
                                               iRBuilder,
                                               module,
                                               globalValues,
                                               functionProtos,
                                               namedValues)
                                : nullptr;
    iRBuilder->CreateBr(finishBasicBlock);
    elseBasicBlock = iRBuilder->GetInsertBlock();

    // merge base block
    function->insert(function->end(), finishBasicBlock);
    iRBuilder->SetInsertPoint(finishBasicBlock);

    // phi node
    auto *const phiNode =
            iRBuilder->CreatePHI(llvm::Type::getDoubleTy(module->getContext()), 2, "if_tmp");
    phiNode->addIncoming(thenValue, thenBasicBlock);
    phiNode->addIncoming(
            elseValue ? elseValue : llvm::ConstantFP::getNullValue(llvm::Type::getDoubleTy(module->getContext())),
            elseBasicBlock);
    value_ = phiNode;
}

void LLVMCodegen::visit(const ForLoopNode *node) {
    assert(iRBuilder->GetInsertBlock());
    auto *const currFunction = iRBuilder->GetInsertBlock()->getParent();
    auto *const beforeLoopBB = iRBuilder->GetInsertBlock();
    auto *const loopBB = llvm::BasicBlock::Create(module->getContext(),
                                                  "for_loop",
                                                  currFunction);
    iRBuilder->CreateBr(loopBB);
    iRBuilder->SetInsertPoint(loopBB);

    const auto *const initVarAst = dynamic_cast<const AssignmentNode *>(node->init.get());
    if (initVarAst == nullptr) {
        return;
    }
    auto *const loopVarValue = iRBuilder->CreatePHI(llvm::Type::getDoubleTy(module->getContext()),
                                                    2,
                                                    initVarAst->name);
    auto *const OldVar = namedValues[initVarAst->name];
    // namedValues[initVarAst->name] = loopVarValue;

    auto *const initValue = generate(initVarAst->rvalue.get(),
                                     iRBuilder,
                                     module,
                                     globalValues,
                                     functionProtos,
                                     namedValues);
    if (initValue == nullptr) {
        return;
    }
    loopVarValue->addIncoming(initValue, beforeLoopBB);
    if (codegenBlock(node->body,
                     iRBuilder,
                     module,
                     globalValues,
                     functionProtos,
                     namedValues) ==
        nullptr) {
        return;
    }

    llvm::Value *nextValue;
    if (node->next) {
        nextValue = generate(node->next.get(),
                             iRBuilder,
                             module,
                             globalValues,
                             functionProtos,
                             namedValues);
        if (nextValue == nullptr) {
            return;
        }
    } else {
        nextValue = iRBuilder->CreateFAdd(loopVarValue,
                                          llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)),
                                          "next_var");
    }

    auto *condExprValue = generate(node->conditional.get(),
                                   iRBuilder,
                                   module,
                                   globalValues,
                                   functionProtos,
                                   namedValues);
    if (condExprValue == nullptr) {
        return;
    }
    condExprValue = iRBuilder->CreateFCmpONE(
            condExprValue, llvm::ConstantFP::get(module->getContext(), llvm::APFloat(0.0)),
            "loop_cond");

    auto *const loopEndBB = iRBuilder->GetInsertBlock();
    loopVarValue->addIncoming(nextValue, loopEndBB);

    auto *const afterLoopBB = llvm::BasicBlock::Create(module->getContext(),
                                                       "after_loop",
                                                       currFunction);
    iRBuilder->CreateCondBr(condExprValue, loopBB, afterLoopBB);
    iRBuilder->SetInsertPoint(afterLoopBB);

    if (OldVar != nullptr) {
        namedValues[initVarAst->name] = OldVar;
    } else {
        namedValues.erase(initVarAst->name);
    }
    value_ = llvm::Constant::getNullValue(llvm::Type::getDoubleTy(module->getContext()));
}

void LLVMCodegen::visit(const UnaryOpNode *node) {
    if (node->operatorType == TokenType::IncrementOperator) {
        value_ = iRBuilder->CreateFAdd(generate(node->expr.get(),
                                                iRBuilder,
                                                module,
                                                globalValues,
                                                functionProtos,
                                                namedValues),
                                       llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)), "increment");
    } else if (node->operatorType == TokenType::DecrementOperator) {
        value_ = iRBuilder->CreateFSub(
                generate(node->expr.get(),
                         iRBuilder,
                         module,
                         globalValues,
                         functionProtos,
                         namedValues),
                llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)), "decrement");
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
