//
// Created by vadim on 19.03.25.
//

#ifndef DOUBLEIRTYPE_H
#define DOUBLEIRTYPE_H

#include "NumericIRType.h"

class DoubleIRType final : public NumericIRType {
public:
    explicit DoubleIRType(bool isPointer = false);

    llvm::Constant *createConstant(const BaseNode *node, ModuleContext &mc) const override;

protected:
    llvm::Type *getBaseLLVMType(llvm::LLVMContext &context) const override;
};


#endif //DOUBLEIRTYPE_H
