//
// Created by vadim on 26.05.2025.
//

#include "IRValue.h"

#include "IRType.h"

IRValue::IRValue(llvm::Value *const value, std::shared_ptr<IRType> type, std::string name) :
    value(value),
    type(std::move(type)),
    name(std::move(name)) {}

llvm::Value *IRValue::getRawValue() const {
    return value;
}

llvm::Value *IRValue::createLoad(llvm::IRBuilder<> &builder) const {
    return type->createLoad(builder, *this);
}

void IRValue::createStore(llvm::IRBuilder<> &builder, llvm::Value *ptr) const {
    type->createStore(builder, *this, ptr);
}

IRValue IRValue::create(llvm::Value *value, std::shared_ptr<IRType> type, const std::string &name) {
    return IRValue(value, std::move(type), name);
}