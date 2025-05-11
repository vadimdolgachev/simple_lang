//
// Created by vadim on 25.03.25.
//

#include "BooleanIRType.h"

#include "ast/BooleanNode.h"

BooleanIRType::BooleanIRType(bool isPointer):
    IRType(isPointer) {}

llvm::Value *BooleanIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                           TokenType op,
                                           llvm::Value *operand,
                                           llvm::Value *storage,
                                           const std::string &name) const {
    throw std::logic_error("Not implemented");
}

llvm::Value *BooleanIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                          TokenType op,
                                          llvm::Value *operand,
                                          llvm::Value *storage,
                                          const std::string &name) const {
    return nullptr;
}

llvm::Value *BooleanIRType::createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    const auto *const boolNode = dynamic_cast<const BooleanNode *>(node);
    return llvm::ConstantInt::getBool(getLLVMType(module.getContext()),boolNode->value);
}

llvm::Type *BooleanIRType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getInt1Ty(context);
}
