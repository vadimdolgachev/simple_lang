//
// Created by vadim on 06.10.24.
//

#ifndef NUMBER_NODE_H
#define NUMBER_NODE_H

#include "BaseNode.h"

class NumberNode final : public ExpressionNode {
public:
    explicit NumberNode(double value, bool isFloat);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] TypePtr getType() const override;

    void setType(TypePtr type) override;

    const double value;
    const bool isFloat;

private:
    const TypePtr type;
};

#endif //NUMBER_NODE_H
