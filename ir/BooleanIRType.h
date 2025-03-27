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

    llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                TokenType op,
                                llvm::Value *operand,
                                llvm::Value *storage,
                                const std::string &name) const override;

    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;
    [[nodiscard]] bool isUnaryOperationSupported(TokenType op) const override;
    llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder,
                               TokenType op,
                               llvm::Value *operand,
                               llvm::Value *storage,
                               const std::string &name) const override;

    llvm::Value *createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) override;
};


#endif //BOOLEANIRTYPE_H
