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
#include "ast/TypeNode.h"

#include "LLVMCodegen.h"

namespace {
    llvm::Value *codegenBlock(const std::unique_ptr<BlockNode> &block,
                              const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                              const std::unique_ptr<llvm::Module> &module,
                              ContextModule &cm) {

        for (auto it = block->statements.begin(); it != block->statements.end(); ++it) {
            auto *const ir = LLVMCodegen::generate(it->get(),
                                                   iRBuilder,
                                                   module,
                                                   cm);
            if (*it == block->statements.back() && ir != nullptr) {
                return ir;
            }
        }
        return nullptr;
    }

    llvm::Function *getModuleFunction(const std::string &name,
                                      const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                                      const std::unique_ptr<llvm::Module> &module,
                                      ContextModule &cm) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = module->getFunction(name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        if (const auto proto = cm.functions.find(name); proto != cm.functions.end()) {
            auto *const fun = LLVMCodegen::generate(proto->second.get(),
                                                    iRBuilder,
                                                    module,
                                                    cm);
            return llvm::dyn_cast<llvm::Function>(fun);
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

    llvm::Type *generateType(const std::unique_ptr<PrimitiveType> &typeNode,
                             llvm::LLVMContext &context) {
        llvm::Type *llvmType = nullptr;
        switch (typeNode->type) {
                using enum PrimitiveTypeKind;
            case Boolean: {
                llvmType = llvm::Type::getInt8Ty(context);
                break;
            }
            case Byte: {
                llvmType = llvm::Type::getInt8Ty(context);
                break;
            }
            case Char: {
                llvmType = llvm::Type::getInt8Ty(context);
                break;
            }
            case Double: {
                llvmType = llvm::Type::getDoubleTy(context);
                break;
            }
            case Integer: {
                llvmType = llvm::Type::getInt32Ty(context);
                break;
            }
            case Void: {
                llvmType = llvm::Type::getVoidTy(context);
                break;
            }
            case Str: {
                llvmType = llvm::PointerType::get(llvm::Type::getInt8Ty(context), 0);
                break;
            }
            default: {
                throw std::logic_error("Unknown type");
            }
        }
        if (typeNode->isPointer) {
            llvmType = llvm::PointerType::get(llvmType, 0);
        }

        return llvmType;
    }

    llvm::Value *tryCastValue(const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                              llvm::Value *value,
                              llvm::Type *destType) {
        if (destType != value->getType()) {
            std::optional<llvm::Instruction::CastOps> castOps;
            if (value->getType() == llvm::Type::getDoubleTy(iRBuilder->getContext())) {
                if (destType == llvm::Type::getInt32Ty(iRBuilder->getContext())
                    || destType == llvm::Type::getInt8Ty(iRBuilder->getContext())) {
                    castOps = llvm::Instruction::CastOps::FPToSI;
                }
            } else if (value->getType() == llvm::Type::getInt32Ty(iRBuilder->getContext())) {
                if (destType == llvm::Type::getDoubleTy(iRBuilder->getContext())) {
                    castOps = llvm::Instruction::CastOps::SIToFP;
                }
            } else if (value->getType() == llvm::Type::getInt8Ty(iRBuilder->getContext())) {
                if (destType == llvm::Type::getDoubleTy(iRBuilder->getContext())) {
                    castOps = llvm::Instruction::CastOps::SIToFP;
                } else if (destType == llvm::Type::getInt32Ty(iRBuilder->getContext())) {
                    castOps = llvm::Instruction::CastOps::SExt;
                }
            }
            if (castOps.has_value()) {
                return iRBuilder->CreateCast(*castOps, value, destType);
            }
            throw std::logic_error("Type cast error");
        }
        return value;
    }
} // namespace

LLVMCodegen::LLVMCodegen(const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                         const std::unique_ptr<llvm::Module> &module,
                         ContextModule &cm):
    iRBuilder(iRBuilder),
    module(module),
    cm(cm) {}

void LLVMCodegen::visit(const IdentNode *node) {
    if (const auto gv = cm.gValues.find(node->name); gv != cm.gValues.end()) {
        value_ = iRBuilder->CreateLoad(gv->second->getValueType(),
                                       gv->second,
                                       node->name + ".global");
    } else {
        auto *const alloc = cm.symTable.lookup(node->name);
        if (alloc == nullptr) {
            throw std::runtime_error(std::format("Unknown variable name: {}", node->name));
        }
        value_ = iRBuilder->CreateLoad(alloc->getAllocatedType(), alloc, node->name);
    }
}

void LLVMCodegen::visit(const FunctionNode *const node) {
    auto *const proto = dyn_cast<llvm::Function>(generate(node->proto.get(),
                                                          iRBuilder,
                                                          module,
                                                          cm));
    cm.symTable.enterScope();
    // Create a new basic block to start insertion into.
    auto *const basicBlock = llvm::BasicBlock::Create(module->getContext(), "entry", proto);
    iRBuilder->SetInsertPoint(basicBlock);

    for (size_t i = 0; i < proto->arg_size(); ++i) {
        auto *const alloca = createEntryBlockAlloca(
                generateType(node->proto->params[i].type, module->getContext()),
                proto,
                proto->getArg(i)->getName());
        iRBuilder->CreateStore(proto->getArg(i), alloca);
        cm.symTable.insert(std::string(proto->getArg(i)->getName()), alloca);
    }

    if ([[maybe_unused]] auto *const returnValue = codegenBlock(node->body,
                                                                iRBuilder,
                                                                module,
                                                                cm)) {
        iRBuilder->CreateRetVoid();
        verifyFunction(*proto);
        value_ = proto;
    } else {
        value_ = iRBuilder->CreateRetVoid();
    }
    cm.symTable.exitScope();
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
                              cm);
    auto *rhsValue = generate(node->rhs.get(),
                              iRBuilder,
                              module,
                              cm);
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
    std::vector<llvm::Type *> functionParams;
    functionParams.reserve(node->params.size());
    for (const auto &param: node->params) {
        functionParams.push_back(generateType(param.type, module->getContext()));
    }
    auto *const functionType = llvm::FunctionType::get(generateType(node->returnType, module->getContext()),
                                                       functionParams,
                                                       node->isVarArgs);
    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  module.get());
    for (auto *it = function->arg_begin(); it != function->arg_end(); ++it) {
        const auto index = std::distance(function->arg_begin(), it);
        it->setName(node->params[index].ident->name);
    }
    value_ = function;
}

void LLVMCodegen::visit(const AssignmentNode *const node) {
    auto *const init = generate(node->rvalue.get(),
                                iRBuilder,
                                module,
                                cm);
    if (iRBuilder->GetInsertBlock() == nullptr) {
        auto *const variable = new llvm::GlobalVariable(
                *module,
                init->getType(),
                true,
                llvm::GlobalValue::ExternalLinkage,
                nullptr,
                node->name);
        variable->setInitializer(dyn_cast<llvm::Constant>(init));
        cm.gValues[node->name] = variable;
        value_ = variable;
    } else {
        auto *const variable = cm.symTable.lookup(node->name);
        if (variable == nullptr) {
            throw std::logic_error("Undefined variable: " + node->name);
        }
        iRBuilder->CreateStore(
                tryCastValue(iRBuilder, init, variable->getAllocatedType()),
                variable);
        cm.symTable.insert(node->name, variable);
        value_ = variable;
    }
}

void LLVMCodegen::visit(const FunctionCallNode *const node) {
    auto *calleeFunc = getModuleFunction(node->ident->name,
                                         iRBuilder,
                                         module,
                                         cm);
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
                                  iRBuilder,
                                  module,
                                  cm);
        if (i < funcType->getNumParams()) {
            argValue = tryCastValue(iRBuilder, argValue, funcType->getParamType(i));
        }
        argsFunc.push_back(argValue);
    }

    value_ = iRBuilder->CreateCall(calleeFunc, argsFunc);
}

void LLVMCodegen::visit(const IfStatement *node) {
    auto *condValue = generate(node->ifBranch.cond.get(), iRBuilder, module, cm);
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
                                         cm);
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
                                               cm)
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
    auto *const OldVar = cm.symTable.lookup(initVarAst->name);
    // namedValues[initVarAst->name] = loopVarValue;

    auto *const initValue = generate(initVarAst->rvalue.get(),
                                     iRBuilder,
                                     module,
                                     cm);
    if (initValue == nullptr) {
        return;
    }
    loopVarValue->addIncoming(initValue, beforeLoopBB);
    if (codegenBlock(node->body,
                     iRBuilder,
                     module,
                     cm) == nullptr) {
        return;
    }

    llvm::Value *nextValue;
    if (node->next) {
        nextValue = generate(node->next.get(),
                             iRBuilder,
                             module,
                             cm);
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
                                   cm);
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
        cm.symTable.insert(initVarAst->name, OldVar);
    } else {
        // namedValues.erase(initVarAst->name);
    }
    value_ = llvm::Constant::getNullValue(llvm::Type::getDoubleTy(module->getContext()));
}

void LLVMCodegen::visit(const UnaryOpNode *node) {
    if (node->operatorType == TokenType::IncrementOperator) {
        value_ = iRBuilder->CreateFAdd(generate(node->expr.get(),
                                                iRBuilder,
                                                module,
                                                cm),
                                       llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)), "increment");
    } else if (node->operatorType == TokenType::DecrementOperator) {
        value_ = iRBuilder->CreateFSub(
                generate(node->expr.get(),
                         iRBuilder,
                         module,
                         cm),
                llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)), "decrement");
    }
}

void LLVMCodegen::visit(const LoopCondNode *node) {
    throw std::runtime_error("not implemented");
}

void LLVMCodegen::visit(const BlockNode *node) {
    cm.symTable.enterScope();
    cm.symTable.exitScope();
    throw std::runtime_error("not implemented");
}

void LLVMCodegen::visit(const DeclarationNode *node) {
    llvm::Value *init = nullptr;
    if (node->init.has_value()) {
        init = generate(node->init.value().get(),
                        iRBuilder,
                        module,
                        cm);
    }

    auto *const type = generateType(node->type, module->getContext());

    if (iRBuilder->GetInsertBlock() == nullptr) {
        auto *const variable = new llvm::GlobalVariable(
                *module,
                type,
                true,
                llvm::GlobalValue::ExternalLinkage,
                nullptr,
                node->ident->name);
        if (init != nullptr) {
            variable->setInitializer(dyn_cast<llvm::Constant>(init));
        }
        cm.gValues[node->ident->name] = variable;
        value_ = variable;
    } else {
        auto *const variable = new llvm::AllocaInst(type, 0, node->ident->name,
                                                    iRBuilder->GetInsertBlock());
        if (node->init.has_value()) {
            iRBuilder->CreateStore(
                    tryCastValue(iRBuilder, init, variable->getAllocatedType()),
                    variable);
        }
        cm.symTable.insert(node->ident->name, variable);
        value_ = variable;
    }
}

llvm::Value *LLVMCodegen::value() const {
    return value_;
}
