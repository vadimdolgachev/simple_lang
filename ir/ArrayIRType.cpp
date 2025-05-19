//
// Created by vadim on 18.05.25.
//

#include "ArrayIRType.h"
#include "ir/IRTypeFactory.h"
#include "ast/ArrayNode.h"

ArrayIRType::ArrayIRType(llvm::Type *elementType, const size_t size, const bool isPointer):
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

llvm::Value *ArrayIRType::createUnaryOp(llvm::IRBuilder<> &builder,
                                        TokenType op,
                                        llvm::Value *operand,
                                        llvm::Value *storage,
                                        const std::string &name) const {
    throw std::logic_error("Unsupported operation");
}

llvm::Constant *ArrayIRType::createConstant(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) {
    const auto *arrayNode = dynamic_cast<const ArrayNode *>(node);
    if (!arrayNode) {
        throw std::logic_error("Expected ArrayNode");
    }
    // const auto *const arrayNode = *arrayNodeOpt;
    if (arrayNode->elements.empty()) {
        return llvm::ConstantAggregateZero::get(llvm::ArrayType::get(elementType, arrayNode->elements.size()));
    }
    if (arrayNode->elements.size() != arraySize) {
        throw std::logic_error("Array size mismatch");
    }

    std::vector<llvm::Constant *> elements;
    elements.reserve(arrayNode->elements.size());

    const auto elemIRType =
            IRTypeFactory::from(arrayNode->getType()->getElementType(), module.getContext());
    for (const auto &element: arrayNode->elements) {
        auto *const elementConstant = llvm::dyn_cast<llvm::Constant>(
                elemIRType->createConstant(element.get(), builder, module));
        if (!elementConstant) {
            throw std::logic_error("Array element type mismatch");
        }
        elements.push_back(elementConstant);
    }
    return llvm::ConstantArray::get(llvm::ArrayType::get(elementType, arraySize), elements);
}
