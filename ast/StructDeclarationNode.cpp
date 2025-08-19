#include "StructDeclarationNode.h"

StructDeclarationNode::StructDeclarationNode(std::string name, std::vector<MemberNode> members) :
    name(std::move(name)),
    members(std::move(members)) {}

void StructDeclarationNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string StructDeclarationNode::toString() const {
    return "StructNode";
}
