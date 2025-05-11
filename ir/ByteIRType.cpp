//
// Created by vadim on 25.03.25.
//

#include "ByteIRType.h"

#include "ast/NumberNode.h"

llvm::Value *ByteIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                        const TokenType op,
                                        llvm::Value *lhs,
                                        llvm::Value *rhs,
                                        const std::string &name) const {
    switch (op) {
        case TokenType::BitwiseAnd:
            return builder.CreateAnd(lhs, rhs, name);
        case TokenType::BitwiseOr:
            return builder.CreateOr(lhs, rhs, name);
        case TokenType::BitwiseXor:
            return builder.CreateXor(lhs, rhs, name);
        // case TokenType::ShiftLeft:
        // return builder.CreateShl(lhs, rhs, name);
        // case TokenType::ShiftRight:
        // return builder.CreateLShr(lhs, rhs, name);
        default:
            return NumericIRType::createBinaryOp(builder, op, lhs, rhs, name);
    }
}

llvm::Value * ByteIRType::createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    const auto *const numberNode = dynamic_cast<const NumberNode *>(node);
    return llvm::ConstantInt::get(getLLVMType(module.getContext()),
                                  static_cast<uint8_t>(numberNode->value),
                                              false);
}

llvm::Type * ByteIRType::getBaseLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getInt8Ty(context);
}
