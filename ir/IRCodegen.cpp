//
// Created by vadim on 06.10.24.
//

#include <list>

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

#include "IRCodegen.h"

namespace {
    llvm::Value *codegenExpressions(const std::list<std::unique_ptr<BaseNode> > &expressions,
                                    const std::unique_ptr<llvm::LLVMContext> &llvmContext,
                                    const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder,
                                    const std::unique_ptr<llvm::Module> &llvmModule,
                                    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement> > &
                                    functionProtos,
                                    std::unordered_map<std::string, llvm::Value *> &namedValues) {
        for (auto it = expressions.begin(); it != expressions.end(); ++it) {
            auto *const ir = generateIR(it->get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
            if (auto *const var = dynamic_cast<AssignmentNode *>(it->get()); var != nullptr) {
                namedValues[var->name] = ir;
            }
            if (*it == expressions.back() && ir != nullptr) {
                return ir;
            }
        }
        return nullptr;
    }

    llvm::Function *getFunction(const std::string &Name,
                                const std::unique_ptr<llvm::LLVMContext> &llvmContext,
                                const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder,
                                const std::unique_ptr<llvm::Module> &llvmModule,
                                std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement> > &
                                functionProtos,
                                std::unordered_map<std::string, llvm::Value *> &namedValues) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = llvmModule->getFunction(Name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        if (const auto iterator = functionProtos.find(Name); iterator != functionProtos.end()) {
            IRCodegen codegen(llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
            iterator->second->visit(&codegen);
            return reinterpret_cast<llvm::Function *>(codegen.value());
        }

        // If no existing prototype exists, return null.
        return nullptr;
    }
} // namespace

IRCodegen::IRCodegen(
    const std::unique_ptr<llvm::LLVMContext> &llvmContext,
    const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder,
    const std::unique_ptr<llvm::Module> &llvmModule,
    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement> > &functionProtos,
    std::unordered_map<std::string, llvm::Value *> &namedValues
) : llvmContext(llvmContext),
    llvmIRBuilder(llvmIRBuilder),
    llvmModule(llvmModule),
    functionProtos(functionProtos),
    namedValues(namedValues) {
}

void IRCodegen::visit(const IdentNode *node) {
    value_ = namedValues[node->name];
}

void IRCodegen::visit(const FunctionNode *const node) {
    assert(llvmContext != nullptr);
    // Transfer ownership of the prototype to the functionProtos map, but keep a
    // reference to it for use below.
    const auto &p = *node->proto;
    functionProtos[p.name] = std::make_unique<ProtoFunctionStatement>(node->proto->name,
                                                                      node->proto->args);
    auto *const function = getFunction(p.name,
                                       llvmContext,
                                       llvmIRBuilder,
                                       llvmModule,
                                       functionProtos,
                                       namedValues
    );
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

    if (auto *const returnValue = codegenExpressions(node->body, llvmContext, llvmIRBuilder, llvmModule, functionProtos,
                                                     namedValues)) {
        llvmIRBuilder->CreateRet(returnValue);
        verifyFunction(*function);
        value_ = function;
        return;
    }

    // Error reading body, remove function.
    function->eraseFromParent();
}

void IRCodegen::visit(const NumberNode *node) {
    assert(llvmContext != nullptr);
    value_ = llvm::ConstantFP::get(*llvmContext, llvm::APFloat(node->value));
}

void IRCodegen::visit(const StringNode *node) {
    throw std::runtime_error("not implemented");
}

void IRCodegen::visit(const BinOpNode *node) {
    assert(llvmContext != nullptr);
    auto *lhsValue = generateIR(node->lhs.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
    auto *rhsValue = generateIR(node->rhs.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
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

void IRCodegen::visit(const ProtoFunctionStatement *node) {
    assert(llvmContext != nullptr);
    std::vector const functionParams(node->args.size(), llvm::Type::getDoubleTy(*llvmContext));
    auto *const functionType = llvm::FunctionType::get(llvm::Type::getDoubleTy(*llvmContext), functionParams,
                                                       false);
    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  llvmModule.get());
    for (auto *it = function->arg_begin(); it != function->arg_end(); ++it) {
        const auto index = std::distance(function->arg_begin(), it);
        it->setName(node->args[index]);
    }
    value_ = function;
}

void IRCodegen::visit(const AssignmentNode *const node) {
    assert(llvmContext != nullptr);
    if (llvmIRBuilder->GetInsertBlock() == nullptr) {
        auto *const variable = new llvm::GlobalVariable(
            *llvmModule,
            llvmIRBuilder->getDoubleTy(),
            false,
            llvm::GlobalValue::CommonLinkage,
            nullptr,
            node->name
        );

        auto *const init = generateIR(node->rvalue.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos,
                                      namedValues);
        variable->setInitializer(reinterpret_cast<llvm::ConstantFP *>(init));
        value_ = variable;
        return;
    }
    auto *const variable = new llvm::AllocaInst(llvmIRBuilder->getDoubleTy(), 0, node->name,
                                                llvmIRBuilder->GetInsertBlock());

    llvmIRBuilder->CreateStore(
        generateIR(node->rvalue.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues),
        variable
    );
    value_ = variable;
}

void IRCodegen::visit(const FunctionCallNode *const node) {
    assert(llvmContext != nullptr);
    // Look up the name in the global module table.
    auto *calleeFunc = getFunction(node->name,
                                   llvmContext,
                                   llvmIRBuilder,
                                   llvmModule,
                                   functionProtos,
                                   namedValues);
    if (calleeFunc == nullptr) {
        return;
    }

    // If argument mismatch error.
    if (calleeFunc->arg_size() != node->args.size()) {
        return;
    }

    std::vector<llvm::Value *> argsFunc;
    for (const auto &arg: node->args) {
        argsFunc.push_back(
            generateIR(arg.get(),
                llvmContext,
                llvmIRBuilder,
                llvmModule,
                functionProtos,
                namedValues));
        if (!argsFunc.back()) {
            return;
        }
    }

    value_ = llvmIRBuilder->CreateCall(calleeFunc, argsFunc, "calltmp");
}

void IRCodegen::visit(const IfStatement *node) {
    auto *condValue = generateIR(node->cond.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
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
    auto *const thenValue = codegenExpressions(node->thenBranch, llvmContext, llvmIRBuilder, llvmModule, functionProtos,
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
                                ? codegenExpressions(node->elseBranch.value(), llvmContext, llvmIRBuilder, llvmModule,
                                                     functionProtos, namedValues)
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

void IRCodegen::visit(const ForLoopNode *node) {
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

    auto *const initValue = generateIR(initVarAst->rvalue.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos,
                                       namedValues);
    if (initValue == nullptr) {
        return;
    }
    loopVarValue->addIncoming(initValue, beforeLoopBB);
    if (codegenExpressions(node->body, llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues) ==
        nullptr) {
        return;
    }

    llvm::Value *nextValue;
    if (node->next) {
        nextValue = generateIR(node->next.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
        if (nextValue == nullptr) {
            return;
        }
    } else {
        nextValue = llvmIRBuilder->CreateFAdd(loopVarValue,
                                              llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0)),
                                              "next_var");
    }

    auto *condExprValue = generateIR(node->conditional.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos,
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

void IRCodegen::visit(const UnaryOpNode *node) {
    if (node->operatorType == TokenType::IncrementOperator) {
        value_ = llvmIRBuilder->CreateFAdd(generateIR(node->expr.get(),
                                                      llvmContext, llvmIRBuilder, llvmModule, functionProtos,
                                                      namedValues),
                                           llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0)), "increment");
    } else if (node->operatorType == TokenType::DecrementOperator) {
        value_ = llvmIRBuilder->CreateFSub(
            generateIR(node->expr.get(), llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues),
            llvm::ConstantFP::get(*llvmContext, llvm::APFloat(1.0)), "decrement");
    }
}

llvm::Value *IRCodegen::value() const {
    return value_;
}
