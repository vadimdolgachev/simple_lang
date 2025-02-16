//
// Created by vadim on 16.02.25.
//

#ifndef BOOL_NODE_H
#define BOOL_NODE_H

#include "BaseNode.h"

class BooleanNode final : public ExpressionNode {
public:
    explicit BooleanNode(bool v);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const bool value;
};

#endif //BOOL_NODE_H
