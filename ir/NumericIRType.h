//
// Created by vadim on 24.03.25.
//

#ifndef NUMERICIRTYPE_H
#define NUMERICIRTYPE_H

#include "IRType.h"

class NumericIRType : public IRType {
protected:
    explicit NumericIRType(bool isPointer,
                           bool isSigned,
                           bool isFloat);

public:
    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;

    [[nodiscard]] bool isOperationSupported(TokenType op, const IRType *rhs) const override;

    [[nodiscard]] bool isUnaryOperationSupported(TokenType op) const override;

    llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                TokenType op,
                                llvm::Value *lhs,
                                llvm::Value *rhs,
                                const std::string &name) const override;

    llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder, TokenType op, llvm::Value *operand, llvm::Value *storage, const std::string &name) const override;

protected:
    llvm::Value *createAdd(llvm::IRBuilder<> &builder,
                           llvm::Value *lhs,
                           llvm::Value *rhs,
                           const std::string &name) const;

    llvm::Value *CreateSub(llvm::IRBuilder<> &builder,
                           llvm::Value *lhs,
                           llvm::Value *rhs,
                           const std::string &name) const;

    llvm::Value *CreateMul(llvm::IRBuilder<> &builder,
                           llvm::Value *lhs,
                           llvm::Value *rhs,
                           const std::string &name) const;

    llvm::Value *CreateDiv(llvm::IRBuilder<> &builder,
                           llvm::Value *lhs,
                           llvm::Value *rhs,
                           const std::string &name) const;

    [[nodiscard]] virtual llvm::CmpInst::Predicate getComparePredicate(TokenType op) const;

    virtual llvm::Value *createCompare(llvm::IRBuilder<> &builder,
                                       llvm::CmpInst::Predicate pred,
                                       llvm::Value *lhs,
                                       llvm::Value *rhs) const;

    virtual llvm::Type *getBaseLLVMType(llvm::LLVMContext &context) const = 0;

    const bool isSigned;
    const bool isFloat;
};

#endif //NUMERICIRTYPE_H
