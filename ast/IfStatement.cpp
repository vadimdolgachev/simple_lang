#include "IfStatement.h"

IfStatement::IfStatement(
    std::unique_ptr<ExpressionNode> cond,
    std::list<std::unique_ptr<BaseNode> > thenBranch,
    std::optional<std::list<std::unique_ptr<BaseNode> > > elseBranch)
    : cond(std::move(cond)), thenBranch(std::move(thenBranch)),
      elseBranch(std::move(elseBranch)) {
}

std::string IfStatement::toString() const { return "if expr"; }

void IfStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
