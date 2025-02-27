#include "ProtoFunctionStatement.h"

#include "IdentNode.h"

ProtoFunctionStatement::ProtoFunctionStatement(std::string name,
                                               std::vector<std::string> params) :
    name(std::move(name)),
    params(std::move(params)) {}

std::string ProtoFunctionStatement::toString() const {
    return "proto func:" + name;
}

void ProtoFunctionStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
