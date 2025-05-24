//
// Created by vadim on 16.05.25.
//

#include "ArrayType.h"
#include "TypeFactory.h"

ArrayType::ArrayType(TypePtr elementType, const size_t size):
    elementType(std::move(elementType)),
    arraySize(size) {
    auto len = MethodInfo::create("len",
                                  TypeFactory::makeFunction(TypeFactory::makePrimitiveType(TypeKind::Integer),
                                                            {},
                                                            false));
    methods.push_back(std::move(len));
}

bool ArrayType::operator==(const Type &other) const {
    if (const auto *const otherArray = dynamic_cast<const ArrayType *>(&other)) {
        return otherArray->size() == size() && *otherArray->getElementType() == *getElementType();
    }
    return false;
}

std::string ArrayType::getName() const {
    return std::format("[{};{}]", elementType->getName(), arraySize);
}

TypeKind ArrayType::getKind() const noexcept {
    return TypeKind::Array;
}

TypePtr ArrayType::getElementType() const {
    return elementType;
}

size_t ArrayType::size() const noexcept {
    return arraySize;
}

const std::vector<MethodInfoPtr> &ArrayType::getMethods() const {
    return methods;
}
