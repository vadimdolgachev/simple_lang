#include "VariableDefinitionStatement.h"

VariableDefinitionStatement::VariableDefinitionStatement(
    std::string name, std::unique_ptr<ExpressionNode> rvalue)
    : name(std::move(name)),
      rvalue(std::move(rvalue)) {
}

std::string VariableDefinitionStatement::toString() const {
    return "var definition name=" + name + ", rvalue=" + rvalue->toString();
}

void VariableDefinitionStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
