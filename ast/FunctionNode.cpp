#include "FunctionNode.h"
#include "IdentNode.h"

FunctionNode::FunctionNode(std::unique_ptr<IdentNode> name,
                           std::vector<std::unique_ptr<IdentNode>> params,
                           std::unique_ptr<BlockNode> body) :
    name(std::move(name)),
    params(std::move(params)),
    body(std::move(body)) {}

std::string FunctionNode::toString() const {
    return name->toString();
}

void FunctionNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
