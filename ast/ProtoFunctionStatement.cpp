#include <utility>

#include "ProtoFunctionStatement.h"
#include "type/TypeFactory.h"

ProtoFunctionStatement::ProtoFunctionStatement(std::string name,
                                               std::optional<TypePtr> returnType,
                                               std::vector<std::unique_ptr<DeclarationNode>> params,
                                               const bool isVarArgs) :
    name(std::move(name)),
    returnType(returnType ? std::move(returnType.value()) : TypeFactory::makePrimitiveType(TypeKind::Void)),
    params(std::move(params)),
    isVarArgs(isVarArgs) {}

ProtoFunctionStatement::ProtoFunctionStatement(const ProtoFunctionStatement &other):
    name(other.name),
    returnType(other.returnType),
    params(::clone<DeclarationNode>(other.params)),
    isVarArgs(other.isVarArgs) {}

std::string ProtoFunctionStatement::toString() const {
    return "proto func:" + name;
}

void ProtoFunctionStatement::visit(NodeVisitor *const visitor) {
    visitor->visit(this);
}

BaseNodePtr ProtoFunctionStatement::clone() const {
    return std::make_unique<ProtoFunctionStatement>(*this);
}
