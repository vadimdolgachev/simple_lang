//
// Created by vadim on 19.03.25.
//

#include "IntIRType.h"
#include "ast/NumberNode.h"

#include <llvm/IR/Type.h>

IntIRType::IntIRType(const bool isPointer):
    NumericIRType(isPointer, false, false) {}

llvm::Constant *IntIRType::createConstant(const BaseNode *node,
                                          llvm::IRBuilder<> & /*builder*/,
                                          llvm::Module &module) {
    const auto *const numberNode = dynamic_cast<const NumberNode *>(node);
    return llvm::ConstantInt::get(getLLVMType(module.getContext()),
                                  llvm::APInt(32, static_cast<int64_t>(numberNode->value),
                                              true));
}

llvm::Type *IntIRType::getBaseLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getInt32Ty(context);
}
