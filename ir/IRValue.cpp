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
    name(std::move(name)) {
}

llvm::Value *IRValue::getRawValue() const {
    return value;
}

std::shared_ptr<IRType> IRValue::getType() const {
    return type;
}

llvm::Value *IRValue::load(llvm::IRBuilder<> &builder) const {
    if (valueKind == ValueKind::Memory) {
        return builder.CreateLoad(type->getLLVMType(builder.getContext()), value);
    }
    return value;
}

void IRValue::store(llvm::IRBuilder<> &builder, llvm::Value *ptr) {
    if (valueKind == ValueKind::Constant) {
        builder.CreateStore(value, ptr);
    } else {
        value = ptr;
    }
}

IRValue IRValue::createConstant(llvm::Value *value, std::shared_ptr<IRType> type, const std::string &name) {
    return {value, std::move(type), ValueKind::Constant, name};
}

IRValue IRValue::createMemory(llvm::Value *value, std::shared_ptr<IRType> type, const std::string &name) {
    return {value, std::move(type), ValueKind::Memory, name};
}

IRValue::ValueKind IRValue::getKind() const {
    return valueKind;
}
