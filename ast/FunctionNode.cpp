#include "FunctionNode.h"

FunctionNode::FunctionNode(std::unique_ptr<ProtoFunctionStatement> proto,
                           std::list<std::unique_ptr<BaseNode> > body) : proto(std::move(proto)),
                                                                         body(std::move(body)) {
}

std::string FunctionNode::toString() const {
    return proto->toString();
}

void FunctionNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}
