//
// Created by vadim on 16.02.25.
//

#ifndef STRING_NODE_H
#define STRING_NODE_H

#include "BaseNode.h"

class StringNode final : public ExpressionNode {
public:
    explicit StringNode(std::string text);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] TypePtr getType() const override;

    void setType(TypePtr type) override;

    const std::string text;
};

#endif //STRING_NODE_H
