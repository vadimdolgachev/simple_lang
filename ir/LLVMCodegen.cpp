//
// Created by vadim on 06.10.24.
//

#include <iostream>

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/ADT/APSInt.h>

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
                              ModuleContext &mc) {

        for (auto it = block->statements.begin(); it != block->statements.end(); ++it) {
            auto *const ir = LLVMCodegen::generate(it->get(),
                                                   iRBuilder,
                                                   module,
                                                   mc);
            if (*it == block->statements.back() && ir != nullptr) {
                return ir;
            }
        }
        return nullptr;
    }

    llvm::Function *getModuleFunction(const std::string &name,
                                      const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                                      const std::unique_ptr<llvm::Module> &module,
                                      ModuleContext &mc) {
        // First, see if the function has already been added to the current module.
        if (auto *const function = module->getFunction(name)) {
            return function;
        }

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        if (const auto proto = mc.functions.find(name); proto != mc.functions.end()) {
            auto *const fun = LLVMCodegen::generate(proto->second.get(),
                                                    iRBuilder,
                                                    module,
                                                    mc);
            return llvm::dyn_cast<llvm::Function>(fun);
        }

        // If no existing prototype exists, return null.
        return nullptr;
    }

    llvm::AllocaInst *createEntryBlockAlloca(llvm::Type *type,
                                             llvm::BasicBlock &block,
                                             const llvm::StringRef varName) {
        llvm::IRBuilder tmpBuilder(&block, block.begin());
        return tmpBuilder.CreateAlloca(type, nullptr, varName);
    }

    llvm::Type *generateType(const std::unique_ptr<PrimitiveType> &typeNode,
                             llvm::LLVMContext &context) {
        llvm::Type *llvmType = nullptr;
        switch (typeNode->type) {
                using enum PrimitiveTypeKind;
            case Boolean: {
                llvmType = llvm::Type::getInt1Ty(context);
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

    llvm::Type *getResultType(llvm::Type *lhsType, llvm::Type *rhsType, llvm::LLVMContext &context) {
        if (lhsType == rhsType) {
            return lhsType;
        }

        if (lhsType->isDoubleTy() || rhsType->isDoubleTy()) {
            return llvm::Type::getDoubleTy(context);
        }

        if (lhsType->isIntegerTy() && rhsType->isIntegerTy()) {
            const unsigned lhsBits = lhsType->getIntegerBitWidth();
            const unsigned rhsBits = rhsType->getIntegerBitWidth();
            return lhsBits > rhsBits ? lhsType : rhsType;
        }

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

    llvm::Value *createAdd(const std::unique_ptr<llvm::IRBuilder<>> &builder, llvm::Value *lhs, llvm::Value *rhs,
                           const llvm::Type *type) {
        return type->isFloatingPointTy()
                   ? builder->CreateFAdd(lhs, rhs, "fadd_tmp")
                   : builder->CreateAdd(lhs, rhs, "iadd_tmp");
    }

    llvm::Value *createSub(const std::unique_ptr<llvm::IRBuilder<>> &builder, llvm::Value *lhs, llvm::Value *rhs,
                           const llvm::Type *type) {
        return type->isFloatingPointTy()
                   ? builder->CreateFSub(lhs, rhs, "fsub_tmp")
                   : builder->CreateSub(lhs, rhs, "isub_tmp");
    }

    llvm::Value *createMul(const std::unique_ptr<llvm::IRBuilder<>> &builder, llvm::Value *lhs, llvm::Value *rhs,
                           const llvm::Type *type) {
        return type->isFloatingPointTy()
                   ? builder->CreateFMul(lhs, rhs, "fmul_tmp")
                   : builder->CreateMul(lhs, rhs, "imul_tmp");
    }

    llvm::Value *createDiv(const std::unique_ptr<llvm::IRBuilder<>> &builder, llvm::Value *lhs, llvm::Value *rhs,
                           const llvm::Type *type) {
        return type->isFloatingPointTy()
                   ? builder->CreateFDiv(lhs, rhs, "fdiv_tmp")
                   : builder->CreateSDiv(lhs, rhs, "sdiv_tmp");
    }

    llvm::Value *createCompare(const std::unique_ptr<llvm::IRBuilder<>> &builder,
                               const TokenType op,
                               llvm::Value *lhs,
                               llvm::Value *rhs) {
        llvm::CmpInst::Predicate pred;

        if (lhs->getType()->isFloatingPointTy()) {
            switch (op) {
                case TokenType::LeftAngleBracket:
                    pred = llvm::CmpInst::FCMP_OLT;
                    break;
                case TokenType::LeftAngleBracketEqual:
                    pred = llvm::CmpInst::FCMP_OLE;
                    break;
                case TokenType::RightAngleBracket:
                    pred = llvm::CmpInst::FCMP_OGT;
                    break;
                case TokenType::RightAngleBracketEqual:
                    pred = llvm::CmpInst::FCMP_OGE;
                    break;
                case TokenType::Equal:
                    pred = llvm::CmpInst::FCMP_OEQ;
                    break;
                case TokenType::NotEqual:
                    pred = llvm::CmpInst::FCMP_ONE;
                    break;
                default:
                    throw std::logic_error("Unsupported float comparison");
            }
            return builder->CreateFCmp(pred, lhs, rhs, "fcmp");
        }
        switch (op) {
            case TokenType::LeftAngleBracket:
                pred = llvm::CmpInst::ICMP_SLT;
                break;
            case TokenType::LeftAngleBracketEqual:
                pred = llvm::CmpInst::ICMP_SLT;
                break;
            case TokenType::RightAngleBracket:
                pred = llvm::CmpInst::ICMP_SGT;
                break;
            case TokenType::RightAngleBracketEqual:
                pred = llvm::CmpInst::ICMP_SGE;
                break;
            case TokenType::Equal:
                pred = llvm::CmpInst::ICMP_EQ;
                break;
            case TokenType::NotEqual:
                pred = llvm::CmpInst::ICMP_NE;
                break;
            default:
                throw std::logic_error("Unsupported integer comparison");
        }
        return builder->CreateICmp(pred, lhs, rhs, "icmp");
    }
} // namespace

LLVMCodegen::LLVMCodegen(const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                         const std::unique_ptr<llvm::Module> &module,
                         ModuleContext &mc):
    iRBuilder(iRBuilder),
    module(module),
    mc(mc) {}

void LLVMCodegen::visit(const IdentNode *node) {
    if (const auto gv = mc.gValues.find(node->name); gv != mc.gValues.end()) {
        value_ = iRBuilder->CreateLoad(gv->second->getValueType(),
                                       gv->second,
                                       node->name + ".global");
    } else {
        auto *const alloc = mc.symTable.lookup(node->name);
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
                                                          mc));
    mc.symTable.enterScope();
    // Create a new basic block to start insertion into.
    auto *const basicBlock = llvm::BasicBlock::Create(module->getContext(), "entry", proto);
    iRBuilder->SetInsertPoint(basicBlock);

    for (size_t i = 0; i < proto->arg_size(); ++i) {
        auto *const alloca = createEntryBlockAlloca(
                generateType(node->proto->params[i].type, module->getContext()),
                proto->getEntryBlock(),
                proto->getArg(i)->getName());
        iRBuilder->CreateStore(proto->getArg(i), alloca);
        mc.symTable.insert(std::string(proto->getArg(i)->getName()), alloca);
    }

    if ([[maybe_unused]] auto *const returnValue = codegenBlock(node->body,
                                                                iRBuilder,
                                                                module,
                                                                mc)) {
        iRBuilder->CreateRetVoid();
        verifyFunction(*proto);
        value_ = proto;
    } else {
        value_ = iRBuilder->CreateRetVoid();
    }
    mc.symTable.exitScope();
}

void LLVMCodegen::visit(const NumberNode *node) {
    if (node->isFloat) {
        value_ = llvm::ConstantFP::get(
                llvm::Type::getDoubleTy(module->getContext()),
                llvm::APFloat(node->value)
                );
    } else {
        value_ = llvm::ConstantInt::get(llvm::Type::getInt32Ty(module->getContext()),
                                        llvm::APInt(32, static_cast<uint64_t>(node->value),
                                                    true));
    }
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
                              mc);
    auto *rhsValue = generate(node->rhs.get(),
                              iRBuilder,
                              module,
                              mc);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        return;
    }
    if (lhsValue->getType()->isPointerTy() || rhsValue->getType()->isPointerTy()) {
        throw std::logic_error("Unsupported operation");
    }

    auto *resultType = getResultType(lhsValue->getType(), rhsValue->getType(), module->getContext());
    if (resultType == nullptr) {
        throw std::runtime_error("Type mismatch: " +
                                 typeToString(lhsValue->getType()) + " and " +
                                 typeToString(rhsValue->getType()));
    }

    lhsValue = tryCastValue(iRBuilder, lhsValue, resultType);
    rhsValue = tryCastValue(iRBuilder, rhsValue, resultType);

    switch (node->binOp) {
        case TokenType::Plus:
            value_ = createAdd(iRBuilder, lhsValue, rhsValue, resultType);
            return;
        case TokenType::Minus:
            value_ = createSub(iRBuilder, lhsValue, rhsValue, resultType);
            return;
        case TokenType::Star:
            value_ = createMul(iRBuilder, lhsValue, rhsValue, resultType);
            return;
        case TokenType::Slash:
            value_ = createDiv(iRBuilder, lhsValue, rhsValue, resultType);
            return;
        case TokenType::LeftAngleBracket:
        case TokenType::LeftAngleBracketEqual:
        case TokenType::RightAngleBracket:
        case TokenType::RightAngleBracketEqual:
        case TokenType::Equal:
        case TokenType::NotEqual:
            value_ = createCompare(iRBuilder, node->binOp, lhsValue, rhsValue);
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
                                mc);
    if (iRBuilder->GetInsertBlock() == nullptr) {
        auto *const variable = new llvm::GlobalVariable(
                *module,
                init->getType(),
                true,
                llvm::GlobalValue::ExternalLinkage,
                nullptr,
                node->name);
        variable->setInitializer(dyn_cast<llvm::Constant>(init));
        mc.gValues[node->name] = variable;
        value_ = variable;
    } else {
        auto *const variable = mc.symTable.lookup(node->name);
        if (variable == nullptr) {
            throw std::logic_error("Undefined variable: " + node->name);
        }
        iRBuilder->CreateStore(
                tryCastValue(iRBuilder, init, variable->getAllocatedType()),
                variable);
        mc.symTable.insert(node->name, variable);
        value_ = variable;
    }
}

void LLVMCodegen::visit(const FunctionCallNode *const node) {
    auto *calleeFunc = getModuleFunction(node->ident->name,
                                         iRBuilder,
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
                                  iRBuilder,
                                  module,
                                  mc);
        if (i < funcType->getNumParams()) {
            argValue = tryCastValue(iRBuilder, argValue, funcType->getParamType(i));
        }
        argsFunc.push_back(argValue);
    }

    value_ = iRBuilder->CreateCall(calleeFunc, argsFunc);
}

void LLVMCodegen::visit(const IfStatement *node) {
    auto *condValue = generate(node->ifBranch.cond.get(), iRBuilder, module, mc);
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
                                         mc);
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
                                               mc)
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
    auto *const OldVar = mc.symTable.lookup(initVarAst->name);
    // namedValues[initVarAst->name] = loopVarValue;

    auto *const initValue = generate(initVarAst->rvalue.get(),
                                     iRBuilder,
                                     module,
                                     mc);
    if (initValue == nullptr) {
        return;
    }
    loopVarValue->addIncoming(initValue, beforeLoopBB);
    if (codegenBlock(node->body,
                     iRBuilder,
                     module,
                     mc) == nullptr) {
        return;
    }

    llvm::Value *nextValue;
    if (node->next) {
        nextValue = generate(node->next.get(),
                             iRBuilder,
                             module,
                             mc);
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
                                   mc);
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
        mc.symTable.insert(initVarAst->name, OldVar);
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
                                                mc),
                                       llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)), "increment");
    } else if (node->operatorType == TokenType::DecrementOperator) {
        value_ = iRBuilder->CreateFSub(
                generate(node->expr.get(),
                         iRBuilder,
                         module,
                         mc),
                llvm::ConstantFP::get(module->getContext(), llvm::APFloat(1.0)), "decrement");
    }
}

void LLVMCodegen::visit(const LoopCondNode *node) {
    throw std::runtime_error("not implemented");
}

void LLVMCodegen::visit(const BlockNode *node) {
    mc.symTable.enterScope();
    mc.symTable.exitScope();
    throw std::runtime_error("not implemented");
}

void LLVMCodegen::visit(const DeclarationNode *node) {
    llvm::Value *init = nullptr;
    if (node->init.has_value()) {
        init = generate(node->init.value().get(),
                        iRBuilder,
                        module,
                        mc);
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
        mc.gValues[node->ident->name] = variable;
        value_ = variable;
    } else {
        auto *const variable = new llvm::AllocaInst(type, 0, node->ident->name,
                                                    iRBuilder->GetInsertBlock());
        if (node->init.has_value()) {
            iRBuilder->CreateStore(
                    tryCastValue(iRBuilder, init, variable->getAllocatedType()),
                    variable);
        }
        mc.symTable.insert(node->ident->name, variable);
        value_ = variable;
    }
}

llvm::Value *LLVMCodegen::value() const {
    return value_;
}
