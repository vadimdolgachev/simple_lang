//
// Created by vadim on 26.03.25.
//

#include "VoidIRType.h"

bool VoidIRType::isOperationSupported(TokenType /*op*/, const IRType * /*other*/) const {
    return false;
}

llvm::Type *VoidIRType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getVoidTy(context);
}

llvm::Value *VoidIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                        TokenType op,
                                        llvm::Value *lhs,
                                        llvm::Value *rhs,
                                        const std::string &name) const {
    return nullptr;
}

bool VoidIRType::isUnaryOperationSupported(TokenType /*op*/) const {
    return false;
}

llvm::Value *VoidIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                       TokenType op,
                                       llvm::Value *operand,
                                       llvm::Value *storage,
                                       const std::string &name) const {
    return nullptr;
}

llvm::Value * VoidIRType::createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    throw std::logic_error("Not supported");
}
