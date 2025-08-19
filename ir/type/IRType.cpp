//
// Created by vadim on 19.03.25.
//

#include "IRType.h"
#include "../IRValue.h"
#include "../IRTypeFactory.h"

IRType::IRType(const bool isPointer) :
    isPointer(isPointer) {}

void IRType::registerCustomOperation(const TokenType op, llvm::Function *function) {}

llvm::Value * IRType::createUndef(const BaseNode *node, ModuleContext &mc) const {
    throw std::runtime_error("Not implemented");
}

llvm::Value *IRType::createMethodCall(llvm::IRBuilder<> &builder,
                                      const MethodInfoPtr &methodInfo,
                                      llvm::Value *object,
                                      const std::vector<llvm::Value *> &args) const {
    throw std::runtime_error("Method '" + methodInfo->name + "' not supported");
}

llvm::Value *IRType::createLoad(llvm::IRBuilder<> &builder, const IRValue &value) const {
    llvm::Type *type = nullptr;
    if (const auto *const alloca = llvm::dyn_cast<llvm::AllocaInst>(value.getRawValue())) {
        type = alloca->getAllocatedType();
    } else if (const auto *const gv = llvm::dyn_cast<llvm::GlobalVariable>(value.getRawValue())) {
        type = gv->getValueType();
    } else {
        type = value.getType()->getLLVMType(builder.getContext());
    }

    if (type && value.getRawValue()->getType()->isPointerTy()) {
        return builder.CreateLoad(type, value.getRawValue());
    }
    return value.getRawValue();
}

llvm::StoreInst *IRType::createStore(llvm::IRBuilder<> &builder, const IRValue &value, llvm::Value *ptr) const {
    return builder.CreateStore(value.createLoad(builder), ptr);
}