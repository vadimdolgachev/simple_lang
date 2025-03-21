//
// Created by vadim on 19.03.25.
//

#ifndef FLOATTYPE_H
#define FLOATTYPE_H

#include "Type.h"

class DoubleType final : public Type {
public:
    bool isOperationSupported(TokenType op, const Type *other) const override;
    llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                TokenType op,
                                llvm::Value *lhs,
                                llvm::Value *rhs,
                                const std::string &name) const override;
    bool isUnaryOperationSupported(TokenType op) const override;
    llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder,
                               TokenType op,
                               llvm::Value *operand,
                               llvm::Value *storage,
                               const std::string &name) const override;
    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;
};


#endif //FLOATTYPE_H
