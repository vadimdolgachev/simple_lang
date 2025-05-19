//
// Created by vadim on 18.05.25.
//

#ifndef ARRAYIRTYPE_H
#define ARRAYIRTYPE_H

#include "IRType.h"

class ArrayIRType final : public IRType {
public:
    ArrayIRType(llvm::Type *elementType, size_t size, bool isPointer = false);

    llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                TokenType op,
                                llvm::Value *operand,
                                llvm::Value *storage,
                                const std::string &name) const override;

    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;

    llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder,
                               TokenType op,
                               llvm::Value *operand,
                               llvm::Value *storage,
                               const std::string &name) const override;

    llvm::Constant *createConstant(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) override;

private:
    llvm::Type *const elementType;
    const size_t arraySize;
};

#endif //ARRAYIRTYPE_H
