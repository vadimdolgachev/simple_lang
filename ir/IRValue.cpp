//
// Created by vadim on 26.05.2025.
//

#include "IRValue.h"
#include "type/IRType.h"

IRValue::IRValue(llvm::Value *const value,
                 std::shared_ptr<IRType> type,
                 const ValueKind valueKind,
                 std::string name) :
    value(value),
    type(std::move(type)),
    valueKind(valueKind),
    name(std::move(name)) {}

llvm::Value *IRValue::getRawValue() const {
    return value;
}

std::shared_ptr<IRType> IRValue::getType() const {
    return type;
}

llvm::Value *IRValue::createLoad(llvm::IRBuilder<> &builder) const {
    return type->createLoad(builder, *this);
}

void IRValue::createStore(llvm::IRBuilder<> &builder, llvm::Value *ptr) const {
    type->createStore(builder, *this, ptr);
}

IRValue IRValue::createValue(llvm::Value *value, std::shared_ptr<IRType> type, const std::string &name) {
    return {value, std::move(type), ValueKind::Value, name};
}

IRValue IRValue::createAlloca(llvm::Value *value, std::shared_ptr<IRType> type, const std::string &name) {
    return {value, std::move(type), ValueKind::Local, name};
}

IRValue IRValue::createGlobal(llvm::Value *value, std::shared_ptr<IRType> type, const std::string &name) {
    return {value, std::move(type), ValueKind::Global, name};
}
