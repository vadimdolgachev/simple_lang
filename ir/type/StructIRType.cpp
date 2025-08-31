//
// Created by vadim on 05.08.2025.
//

#include "StructIRType.h"
#include "ast/StructInitNode.h"
#include "ir/IRTypeFactory.h"
#include "../LLVMCodegen.h"

#include <ranges>

namespace {
    std::unordered_map<std::string, llvm::StructType *> cachedStructTypes;
}

StructIRType::StructIRType(StructTypePtr structType) :
    IRType(false),
    structType(std::move(structType)) {}

llvm::Value *StructIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                          TokenType op,
                                          llvm::Value *lhs,
                                          llvm::Value *rhs,
                                          const std::string &name) const {
    throw std::runtime_error("Not implemented");
}

llvm::Value *StructIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                         TokenType op,
                                         llvm::Value *operand,
                                         llvm::Value *storage,
                                         const std::string &name) const {
    throw std::runtime_error("Not implemented");
}

llvm::Type *StructIRType::getLLVMType(llvm::LLVMContext &context) const {
    std::vector<llvm::Type *> fieldTypes;
    for (const auto &[name, type]: structType->getFields()) {
        fieldTypes.push_back(IRTypeFactory::from(type, context)->getLLVMType(context));
    }
    if (!cachedStructTypes.contains(structType->getName())) {
        cachedStructTypes[structType->getName()] = llvm::StructType::create(context, fieldTypes, structType->getName());
    }
    return cachedStructTypes[structType->getName()];
}

llvm::Type *StructIRType::getLLVMElementType(llvm::LLVMContext &context) const {
    throw std::runtime_error("Not supported");
}

llvm::Constant *StructIRType::createConstant(const BaseNode *node, ModuleContext &mc) const {
    if (const auto *structInit = dynamic_cast<const StructInitNode *>(node); structInit != nullptr) {
        std::vector<llvm::Constant *> fieldValues;
        fieldValues.reserve(structInit->designator.size());
        for (const auto &valueNode: structInit->designator | std::views::values) {
            auto *const constValue = LLVMCodegen::generate(valueNode.get(), mc).value().getRawValue();
            fieldValues.push_back(llvm::dyn_cast<llvm::Constant>(constValue));
        }
        return llvm::ConstantStruct::get(llvm::cast<llvm::StructType>(getLLVMType(*mc.context)),
                                         fieldValues);
    }
    throw std::runtime_error("Unexpected node type");
}

llvm::Value *StructIRType::createGlobal(const BaseNode *node, ModuleContext &mc) const {
    if (const auto *structInit = dynamic_cast<const StructInitNode *>(node); structInit != nullptr) {
        auto *const llvmStructType = getLLVMType(*mc.context);
        auto *gv = new llvm::GlobalVariable(*mc.module,
                                            llvmStructType,
                                            false,
                                            llvm::GlobalValue::PrivateLinkage,
                                            llvm::ConstantAggregateZero::get(llvmStructType),
                                            structInit->ident + ".struct");
        for (const auto &[fieldName, initNode]: structInit->designator) {
            if (const auto index = structType->findFieldIndex(fieldName)) {
                auto *const initValue = LLVMCodegen::generate(initNode.get(), mc).value().load(*mc.builder);
                auto *fieldPtr = mc.builder->CreateStructGEP(llvmStructType,
                                                             gv,
                                                             index.value(),
                                                             "field" + std::to_string(index.value()) + ".ptr");
                mc.builder->CreateStore(initValue, fieldPtr);
            }
        }
        return mc.builder->CreateLoad(llvmStructType, gv);
    }
    throw std::runtime_error("Unexpected node type");
}

llvm::Value *StructIRType::createUndef(const BaseNode *node, ModuleContext &mc) const {
    if (const auto *structInit = dynamic_cast<const StructInitNode *>(node); structInit != nullptr) {
        auto *const llvmStructType = getLLVMType(*mc.context);
        llvm::Value *result = llvm::UndefValue::get(llvmStructType);
        for (const auto &[fieldName, initNode]: structInit->designator) {
            auto *const initValue = LLVMCodegen::generate(initNode.get(), mc).value().load(*mc.builder);
            result = mc.builder->CreateInsertValue(
                    result,
                    initValue,
                    structType->findFieldIndex(fieldName).value(),
                    "struct.tmp");
        }
        return result;
    }
    throw std::runtime_error("Unexpected node type");
}
