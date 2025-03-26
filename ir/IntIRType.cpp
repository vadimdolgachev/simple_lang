//
// Created by vadim on 19.03.25.
//

#include "IntIRType.h"

#include <llvm/IR/Type.h>

#include "DoubleIRType.h"

IntIRType::IntIRType(const bool isPointer):
    NumericIRType(isPointer, false, false) {}

llvm::Type *IntIRType::getBaseLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getInt32Ty(context);
}
