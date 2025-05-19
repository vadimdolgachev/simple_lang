//
// Created by vadim on 27.04.25.
//

#include "StrType.h"

#include "TypeFactory.h"

StrType::StrType():
    PrimitiveType(TypeKind::Str) {
    auto len = MethodInfo::create("len",
                                  TypeFactory::makeFunction(TypeFactory::makePrimitiveType(TypeKind::Integer),
                                                            {},
                                                            false));
    methods.push_back(std::move(len));
}

ResultType StrType::getResultTypeUnary(TokenType op) const {
    return std::unexpected("Unsupported operator for string type");
}

bool StrType::operator==(const Type &other) const {
    return dynamic_cast<const StrType *>(&other) != nullptr;
}

std::string StrType::getName() const {
    return "str";
}

const std::vector<MethodInfoPtr> &StrType::getMethods() const {
    return methods;
}
