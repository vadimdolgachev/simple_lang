#include "ProtoFunctionStatement.h"

#include <utility>

#include "Util.h"

ProtoFunctionStatement::ProtoFunctionStatement(std::string name,
                                               TypeNode returnType,
                                               std::vector<std::unique_ptr<DeclarationNode>> params,
                                               const bool isVarArgs) :
    name(std::move(name)),
    returnType(std::move(returnType)),
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

void ProtoFunctionStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}

std::unique_ptr<BaseNode> ProtoFunctionStatement::clone() const {
    return std::make_unique<ProtoFunctionStatement>(*this);
}
