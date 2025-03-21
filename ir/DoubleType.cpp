//
// Created by vadim on 19.03.25.
//

#include "DoubleType.h"

#include "IntType.h"

bool DoubleType::isOperationSupported(const TokenType op, const Type *other) const {
    if (op == TokenType::Equal
        || op == TokenType::NotEqual
        || op == TokenType::LeftAngleBracket
        || op == TokenType::LeftAngleBracketEqual
        || op == TokenType::RightAngleBracket
        || op == TokenType::RightAngleBracketEqual) {
        return dynamic_cast<const IntType *>(other) ||
               dynamic_cast<const DoubleType *>(other);
    }
    return false;
}

llvm::Value *DoubleType::createBinaryOp(llvm::IRBuilder<> &builder,
                                        const TokenType op,
                                        llvm::Value *lhs,
                                        llvm::Value *rhs,
                                        const std::string &name) const {
    if (rhs->getType()->isIntegerTy()) {
        rhs = builder.CreateFPToSI(rhs, lhs->getType());
    }

    switch (op) {
        case TokenType::Plus:
            return builder.CreateFAdd(lhs, rhs, name);
        case TokenType::Minus:
            return builder.CreateFSub(lhs, rhs, name);
        case TokenType::Star:
            return builder.CreateFMul(lhs, rhs, name);
        case TokenType::Slash:
            return builder.CreateFDiv(lhs, rhs, name);
        default:
            throw std::invalid_argument("Unsupported integer operation");
    }
}

bool DoubleType::isUnaryOperationSupported(TokenType op) const {
    return op == TokenType::IncrementOperator
           || op == TokenType::DecrementOperator
           || op == TokenType::Plus
           || op == TokenType::Minus;
}

llvm::Value *DoubleType::createUnaryOp(llvm::IRBuilder<> &builder,
                                       const TokenType op,
                                       llvm::Value *operand,
                                       llvm::Value *storage,
                                       const std::string &name) const {
    llvm::Value *delta = llvm::ConstantInt::get(
            operand->getType(),
            op == TokenType::IncrementOperator ? 1 : -1);

    llvm::Value *result = builder.CreateAdd(operand, delta, name);

    if (storage) {
        builder.CreateStore(result, storage);
    }

    return op == TokenType::IncrementOperator ? result : operand;
}

llvm::Type *DoubleType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getDoubleTy(context);
}
