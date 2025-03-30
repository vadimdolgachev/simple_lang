//
// Created by vadim on 29.03.25.
//

#include "CommentNode.h"

CommentNode::CommentNode(std::string text):
    text(std::move(text)) {}

void CommentNode::visit(NodeVisitor *visitor) const {
    visitor->visit(this);
}

std::string CommentNode::toString() const {
    return "Comment";
}
