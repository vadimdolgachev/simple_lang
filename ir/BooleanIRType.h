//
// Created by vadim on 25.03.25.
//

#ifndef BOOLEANIRTYPE_H
#define BOOLEANIRTYPE_H

#include "IRType.h"


class BooleanIRType : public IRType {
public:
    explicit BooleanIRType(bool isPointer = false);

    [[nodiscard]] bool isOperationSupported(TokenType op, const IRType *rhs) const override;

    [[nodiscard]] llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                                     TokenType op,
                                                     llvm::Value *operand,
                                                     llvm::Value *storage,
                                                     const std::string &name) const override;

    [[nodiscard]] llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;
    [[nodiscard]] bool isUnaryOperationSupported(TokenType op) const override;
    [[nodiscard]] llvm::Value * createUnaryOp(llvm::IRBuilder<> &builder,
            TokenType op,
            llvm::Value *operand,
            llvm::Value *storage,
            const std::string &name) const override;
};


#endif //BOOLEANIRTYPE_H
