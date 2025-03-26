//
// Created by vadim on 19.03.25.
//

#include "DoubleIRType.h"

#include "IntIRType.h"


DoubleIRType::DoubleIRType(const bool isPointer):
    NumericIRType(isPointer, false, true) {}

llvm::Type *DoubleIRType::getBaseLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getDoubleTy(context);
}
