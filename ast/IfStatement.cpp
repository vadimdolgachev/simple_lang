#include "IfStatement.h"

IfStatement::IfStatement(
        CondBranch ifBranch,
        std::vector<CondBranch> elseIfBranches,
        std::optional<std::vector<std::unique_ptr<BaseNode>>> elseBranch) :
    ifBranch(std::move(ifBranch)),
    elseIfBranches(std::move(elseIfBranches)),
    elseBranch(std::move(elseBranch)) {}

std::string IfStatement::toString() const {
    return "if expr";
}

void IfStatement::visit(NodeVisitor *const visitor) const {
    visitor->visit(this);
}
