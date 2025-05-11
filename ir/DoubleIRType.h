//
// Created by vadim on 19.03.25.
//

#ifndef DOUBLEIRTYPE_H
#define DOUBLEIRTYPE_H

#include "NumericIRType.h"

class DoubleIRType final : public NumericIRType {
public:
    explicit DoubleIRType(bool isPointer = false);

    llvm::Value *createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) override;

protected:
    llvm::Type *getBaseLLVMType(llvm::LLVMContext &context) const override;
};


#endif //DOUBLEIRTYPE_H
