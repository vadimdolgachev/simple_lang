//
// Created by vadim on 19.03.25.
//

#include "DoubleIRType.h"

#include "ast/NumberNode.h"

DoubleIRType::DoubleIRType(const bool isPointer):
    NumericIRType(isPointer, false, true) {}

llvm::Value *DoubleIRType::createValue(const BaseNode *node, llvm::IRBuilder<> & /*builder*/, llvm::Module &module) {
    const auto *const numberNode = dynamic_cast<const NumberNode *>(node);
    return llvm::ConstantFP::get(getLLVMType(module.getContext()),
                                 llvm::APFloat(numberNode->value));
}

llvm::Type *DoubleIRType::getBaseLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getDoubleTy(context);
}
