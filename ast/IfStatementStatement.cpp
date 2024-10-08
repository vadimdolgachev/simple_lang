#include "IfStatementStatement.h"

IfStatementStatement::IfStatementStatement(
    std::unique_ptr<ExpressionNode> cond,
    std::list<std::unique_ptr<BaseNode> > thenBranch,
    std::optional<std::list<std::unique_ptr<BaseNode> > > elseBranch)
    : cond(std::move(cond)), thenBranch(std::move(thenBranch)),
      elseBranch(std::move(elseBranch)) {
}

std::string IfStatementStatement::toString() const { return "if expr"; }

void IfStatementStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
