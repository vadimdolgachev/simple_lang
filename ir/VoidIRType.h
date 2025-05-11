//
// Created by vadim on 25.03.25.
//

#ifndef VOIDIRTYPE_H
#define VOIDIRTYPE_H

#include "IRType.h"

class VoidIRType final : public IRType {
public:
    VoidIRType() :
        IRType(false) {}

    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;

    llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                TokenType op,
                                llvm::Value *lhs,
                                llvm::Value *rhs,
                                const std::string &name) const override;

    llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder,
                               TokenType op,
                               llvm::Value *operand,
                               llvm::Value *storage,
                               const std::string &name) const override;

    llvm::Value *createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) override;
};

#endif //VOIDIRTYPE_H
