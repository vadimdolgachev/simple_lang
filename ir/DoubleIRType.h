//
// Created by vadim on 19.03.25.
//

#ifndef FLOATIRTYPE_H
#define FLOATIRTYPE_H

#include "IRType.h"
#include "NumericIRType.h"

class DoubleIRType final : public NumericIRType {
public:
    explicit DoubleIRType(bool isPointer = false);

protected:
    [[nodiscard]] llvm::Type *getBaseLLVMType(llvm::LLVMContext &context) const override;
};


#endif //FLOATIRTYPE_H
