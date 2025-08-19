//
// Created by vadim on 25.03.25.
//

#include "CharIRType.h"


CharIRType::CharIRType(const bool isPointer):
    NumericIRType(isPointer, true, false) {}

llvm::Constant *CharIRType::createConstant(const BaseNode *node, ModuleContext &mc) const {
    // return llvm::ConstantInt::get(getLLVMType(context),
    //                               static_cast<int8_t>(charNode->value));
    throw std::logic_error("Not supoported");
}

llvm::Type *CharIRType::getBaseLLVMType(llvm::LLVMContext &context) const {
    return llvm::Type::getInt8Ty(context);
}
