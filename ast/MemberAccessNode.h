//
// Created by vadim on 27.03.25.
//

#ifndef MEMBERACCESSNODE_H
#define MEMBERACCESSNODE_H

#include <memory>

#include "BaseNode.h"

class MemberAccessNode : public ExpressionNode {
public:
    explicit MemberAccessNode(std::unique_ptr<ExpressionNode> object):
        object(std::move(object)) {}

    const std::unique_ptr<ExpressionNode> object;
};

#endif //MEMBERACCESSNODE_H
