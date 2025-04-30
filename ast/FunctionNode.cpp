#include "FunctionNode.h"
#include "ProtoFunctionStatement.h"

FunctionNode::FunctionNode(std::shared_ptr<ProtoFunctionStatement> proto,
                           std::unique_ptr<BlockNode> body) :
    proto(std::move(proto)),
    body(std::move(body)) {}

std::string FunctionNode::toString() const {
    return proto->name;
}

void FunctionNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}
