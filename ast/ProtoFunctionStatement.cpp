#include "ProtoFunctionStatement.h"

#include <utility>

ProtoFunctionStatement::ProtoFunctionStatement(std::string name,
                                               TypeNode returnType,
                                               std::vector<DeclarationNode> params,
                                               const bool isVarArgs) :
    name(std::move(name)),
    returnType(std::move(returnType)),
    params(std::move(params)),
    isVarArgs(isVarArgs) {}

std::string ProtoFunctionStatement::toString() const {
    return "proto func:" + name;
}

void ProtoFunctionStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
