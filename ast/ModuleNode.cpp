//
// Created by vadim on 15.04.25.
//

#include "ModuleNode.h"

void ModuleNode::visit(NodeVisitor *visitor) {
    visitor->visit(this);
}

std::string ModuleNode::toString() const {
    return "ModuleNode";
}
