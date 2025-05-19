//
// Created by vadim on 26.03.25.
//

#include "VoidIRType.h"

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

llvm::Value *VoidIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                       TokenType op,
                                       llvm::Value *operand,
                                       llvm::Value *storage,
                                       const std::string &name) const {
    return nullptr;
}

llvm::Constant *VoidIRType::createConstant(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    throw std::logic_error("Not supported");
}
