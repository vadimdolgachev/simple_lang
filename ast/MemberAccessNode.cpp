//
// Created by vadim on 27.03.25.
//

#include "MemberAccessNode.h"

MemberAccessNode::MemberAccessNode(ExprNodePtr object):
    object(std::move(object)) {}

StructTypePtr MemberAccessNode::getObjectType() const {
    return std::dynamic_pointer_cast<StructType>(object->getType());
}