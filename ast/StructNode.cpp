#include "StructNode.h"

StructNode::StructNode(std::string name, std::vector<MemberNode> members) :
    name(std::move(name)),
    members(std::move(members)) {}

void StructNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string StructNode::toString() const {
    return "StructNode";
}
