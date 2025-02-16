//
// Created by vadim on 16.02.25.
//

#ifndef STRING_NODE_H
#define STRING_NODE_H

#include "BaseNode.h"

class StringNode final : public ExpressionNode {
public:
    explicit StringNode(std::string v);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::string str;
};

#endif //STRING_NODE_H
