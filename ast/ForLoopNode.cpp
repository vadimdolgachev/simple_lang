#include "ForLoopNode.h"

ForLoopNode::ForLoopNode(std::unique_ptr<BaseNode> initExpr,
                         std::unique_ptr<ExpressionNode> nextExpr,
                         std::unique_ptr<ExpressionNode> endExpr,
                         std::vector<std::unique_ptr<BaseNode>> body) :
    init(std::move(initExpr)),
    next(std::move(nextExpr)),
    conditional(std::move(endExpr)),
    body(std::move(body)) {}

std::string ForLoopNode::toString() const {
    return "for loop";
}

void ForLoopNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
