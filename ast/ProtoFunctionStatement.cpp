#include "ProtoFunctionStatement.h"

ProtoFunctionStatement::ProtoFunctionStatement(std::string name, std::vector<std::string> args)
    : name(std::move(name)),
      args(std::move(args)) {
}

std::string ProtoFunctionStatement::toString() const {
    return "proto func:" + name;
}

void ProtoFunctionStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
