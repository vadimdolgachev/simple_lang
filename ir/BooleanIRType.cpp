//
// Created by vadim on 25.03.25.
//

#include "BooleanIRType.h"

#include "ast/BooleanNode.h"

BooleanIRType::BooleanIRType(bool isPointer):
    IRType(isPointer) {}

bool BooleanIRType::isOperationSupported(const TokenType op, const IRType *rhs) const {
    if (dynamic_cast<const BooleanIRType *>(rhs)) {
        return op == TokenType::Equal ||
               op == TokenType::NotEqual ||
               op == TokenType::LogicalAnd ||
               op == TokenType::LogicalOr;
    }
    return false;
}

llvm::Value *BooleanIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                           TokenType op,
                                           llvm::Value *operand,
                                           llvm::Value *storage,
                                           const std::string &name) const {
    throw std::logic_error("Not implemented");
}

bool BooleanIRType::isUnaryOperationSupported(TokenType /*op*/) const {
    return false;
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
