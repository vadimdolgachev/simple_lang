//
// Created by vadim on 19.03.25.
//

#ifndef INTIRTYPE_H
#define INTIRTYPE_H

#include "NumericIRType.h"

class IntIRType final : public NumericIRType {
public:
    explicit IntIRType(bool isPointer = false);

    llvm::Constant *createConstant(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) override;

protected:
    llvm::Type *getBaseLLVMType(llvm::LLVMContext &context) const override;
};

#endif // INTIRTYPE_H
