//
// Created by vadim on 05.08.2025.
//

#ifndef STRUCTIRTYPE_H
#define STRUCTIRTYPE_H

#include "IRType.h"
#include "type/StructType.h"

class StructIRType final : public IRType {
public:
    explicit StructIRType(StructTypePtr structType);

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
    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;
    llvm::Type *getLLVMElementType(llvm::LLVMContext &context) const override;
    llvm::Constant *createConstant(const BaseNode *node, ModuleContext &mc) const override;
    llvm::Value *createGlobal(const BaseNode *node, ModuleContext &mc) const;
    llvm::Value *createUndef(const BaseNode *node, ModuleContext &mc) const override;

private:
    const StructTypePtr structType;
};

#endif //STRUCTIRTYPE_H