//
// Created by vadim on 06.10.24.
//

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
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

#include "ArrayNodeGenerator.h"
#include "AssignmentNodeGenerator.h"
#include "IRTypeFactory.h"
#include "ast/IndexAccessNode.h"
#include "ast/TypeCastNode.h"
#include "ir/FunctionNodeGenerator.h"
#include "type/FunctionType.h"
#include "ast/ModuleNode.h"
#include "IdentNodeGenerator.h"
#include "BinOpNodeGenerator.h"
#include "BlockNodeGenerator.h"
#include "BooleanNodeGenerator.h"
#include "DeclarationNodeGenerator.h"
#include "FunctionCallNodeGenerator.h"
#include "IfStatementGenerator.h"
#include "IndexAccessNodeGenerator.h"
#include "LoopCondNodeGenerator.h"
#include "MethodCallNodeGenerator.h"
#include "ModuleNodeGenerator.h"
#include "NumberNodeGenerator.h"
#include "ProtoFunctionGenerator.h"
#include "ReturnNodeGenerator.h"
#include "StringNodeGenerator.h"
#include "TernaryOperatorNodeGenerator.h"
#include "TypeCastNodeGenerator.h"
#include "UnaryOpNodeGenerator.h"

namespace {
    std::string typeToString(const llvm::Type *type) {
        std::string typeStr;
        llvm::raw_string_ostream rso(typeStr);
        type->print(rso);
        return rso.str();
    }
} // namespace

LLVMCodegen::LLVMCodegen(ModuleContext &moduleContext) :
    mc(moduleContext) {
    generators[std::type_index(typeid(IdentNode))] = std::make_unique<IdentNodeGenerator>();
    generators[std::type_index(typeid(FunctionNode))] = std::make_unique<FunctionNodeGenerator>();
    generators[std::type_index(typeid(BinOpNode))] = std::make_unique<BinOpNodeGenerator>();
    generators[std::type_index(typeid(ProtoFunctionStatement))] = std::make_unique<ProtoFunctionGenerator>();
    generators[std::type_index(typeid(AssignmentNode))] = std::make_unique<AssignmentNodeGenerator>();
    generators[std::type_index(typeid(FunctionCallNode))] = std::make_unique<FunctionCallNodeGenerator>();
    generators[std::type_index(typeid(IfStatement))] = std::make_unique<IfStatementGenerator>();
    generators[std::type_index(typeid(UnaryOpNode))] = std::make_unique<UnaryOpNodeGenerator>();
    generators[std::type_index(typeid(LoopCondNode))] = std::make_unique<LoopCondNodeGenerator>();
    generators[std::type_index(typeid(DeclarationNode))] = std::make_unique<DeclarationNodeGenerator>();
    generators[std::type_index(typeid(ReturnNode))] = std::make_unique<ReturnNodeGenerator>();
    generators[std::type_index(typeid(BlockNode))] = std::make_unique<BlockNodeGenerator>();
    generators[std::type_index(typeid(TernaryOperatorNode))] = std::make_unique<TernaryOperatorNodeGenerator>();
    generators[std::type_index(typeid(MethodCallNode))] = std::make_unique<MethodCallNodeGenerator>();
    generators[std::type_index(typeid(ModuleNode))] = std::make_unique<ModuleNodeGenerator>();
    generators[std::type_index(typeid(TypeCastNode))] = std::make_unique<TypeCastNodeGenerator>();
    generators[std::type_index(typeid(ArrayNode))] = std::make_unique<ArrayNodeGenerator>();
    generators[std::type_index(typeid(IndexAccessNode))] = std::make_unique<IndexAccessNodeGenerator>();
    generators[std::type_index(typeid(NumberNode))] = std::make_unique<NumberNodeGenerator>();
    generators[std::type_index(typeid(StringNode))] = std::make_unique<StringNodeGenerator>();
    generators[std::type_index(typeid(BooleanNode))] = std::make_unique<BooleanNodeGenerator>();
}

void LLVMCodegen::visit(IdentNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(FunctionNode *const node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(NumberNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(StringNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(BooleanNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(BinOpNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(ProtoFunctionStatement *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(AssignmentNode *const node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(FunctionCallNode *const node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(IfStatement *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(UnaryOpNode *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(LoopCondNode *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(BlockNode *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(DeclarationNode *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(ReturnNode *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(TernaryOperatorNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(MethodCallNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(FieldAccessNode *node) {
    throw std::logic_error("FieldAccessNode not implemented");
}

void LLVMCodegen::visit(CommentNode *node) {
    // skip comments
}

void LLVMCodegen::visit(ModuleNode *node) {
    generateValue(node, mc);
}

void LLVMCodegen::visit(TypeCastNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(ArrayNode *node) {
    res = generateValue(node, mc);
}

void LLVMCodegen::visit(IndexAccessNode *node) {
    res = generateValue(node, mc);
}

IRValueOpt LLVMCodegen::value() const {
    return res;
}

void generateBasicBlock(llvm::BasicBlock *const basicBlock,
                        const BlockNode::Statements &statements,
                        ModuleContext &moduleContext,
                        const std::optional<std::function<void()>> &prologue) {
    moduleContext.symTable.enterScope();

    moduleContext.builder->SetInsertPoint(basicBlock);
    if (prologue.has_value()) {
        (*prologue)();
    }

    for (const auto &stmt: statements) {
        LLVMCodegen::generate(stmt.get(), moduleContext);
    }

    moduleContext.symTable.exitScope();
}

llvm::Function *getModuleFunction(const std::string &name, const ModuleContext &mc) {
    // First, see if the function has already been added to the current module.
    if (auto *const function = mc.module->getFunction(name)) {
        return function;
    }

    // If not, check whether we can codegen the declaration from some existing
    // prototype.

    if (const auto &proto = mc.symTable.lookupFunction(name); !proto.empty()) {
        if (const auto functionType = proto[0]->type->asFunction()) {
            std::vector<llvm::Type *> functionParams;
            functionParams.reserve(functionType.value()->parametersType().size());
            for (const auto &param: functionType.value()->parametersType()) {
                functionParams.push_back(
                        IRTypeFactory::from(param, mc.module->getContext())->getLLVMType(mc.module->getContext()));
            }
            auto *retType = IRTypeFactory::from(functionType.value()->returnType(), mc.module->getContext())
                    ->getLLVMType(mc.module->getContext());
            auto *const llvmFunctionType =
                    llvm::FunctionType::get(retType, functionParams, functionType.value()->isVariadic());
            return llvm::Function::Create(llvmFunctionType, llvm::Function::ExternalLinkage, name, mc.module.get());
        }
    }
    // If no existing prototype exists, return null.
    return nullptr;
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