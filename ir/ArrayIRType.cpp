//
// Created by vadim on 18.05.25.
//

#include "ArrayIRType.h"
#include "ir/IRTypeFactory.h"
#include "ast/ArrayNode.h"

ArrayIRType::ArrayIRType(llvm::Type *const elementType, const size_t size, const bool isPointer) :
    IRType(isPointer),
    elementType(elementType),
    arraySize(size) {}

llvm::Value *ArrayIRType::createBinaryOp(llvm::IRBuilder<> &builder,
                                         TokenType op,
                                         llvm::Value *operand,
                                         llvm::Value *storage,
                                         const std::string &name) const {
    throw std::logic_error("Unsupported operation");
}

llvm::Type *ArrayIRType::getLLVMType(llvm::LLVMContext &context) const {
    return llvm::ArrayType::get(elementType, arraySize);
}

llvm::Type *ArrayIRType::getLLVMElementType(llvm::LLVMContext &context) const {
    return elementType;
}

llvm::Value *ArrayIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                        TokenType op,
                                        llvm::Value *operand,
                                        llvm::Value *storage,
                                        const std::string &name) const {
    throw std::logic_error("Unsupported operation");
}

llvm::Constant *ArrayIRType::createConstant(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    const auto arrayNode = asNode<ArrayNode>(node);
    if (!arrayNode) {
        throw std::logic_error("Expected ArrayNode");
    }

    if (arrayNode.value()->elements.empty()) {
        return llvm::ConstantAggregateZero::get(llvm::ArrayType::get(elementType, arrayNode.value()->elements.size()));
    }
    if (arrayNode.value()->elements.size() != arraySize) {
        throw std::logic_error("Array size mismatch");
    }

    std::vector<llvm::Constant *> elements;
    elements.reserve(arrayNode.value()->elements.size());

    const auto elemIRType =
            IRTypeFactory::from(arrayNode.value()->getType()->getElementType(), module.getContext());
    for (const auto &element: arrayNode.value()->elements) {
        auto *const elementConstant =
                elemIRType->createConstant(element.get(), builder, module);
        if (!elementConstant) {
            throw std::logic_error("Array element type mismatch");
        }
        elements.push_back(elementConstant);
    }
    return llvm::ConstantArray::get(llvm::ArrayType::get(elementType, arraySize), elements);
}

llvm::Value *ArrayIRType::createMethodCall(llvm::IRBuilder<> &builder,
                                           const MethodInfoPtr &methodInfo,
                                           llvm::Value *object,
                                           const std::vector<llvm::Value *> &args) const {
    if (methodInfo->name == "len") {
        return llvm::ConstantInt::get(getLLVMType(builder.getContext()),
                                      llvm::APInt(32, static_cast<int64_t>(arraySize), true));
    }
    return IRType::createMethodCall(builder, methodInfo, object, args);
}
