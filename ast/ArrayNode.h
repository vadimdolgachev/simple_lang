//
// Created by vadim on 14.05.2025.
//

#ifndef ARRAYNODE_H
#define ARRAYNODE_H

#include "BaseNode.h"

class ArrayNode final : public ExpressionNode {
public:
    explicit ArrayNode(std::vector<ExprNodePtr> elements);

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    [[nodiscard]] TypePtr getType() const override;

    void setType(TypePtr type) override;

    std::vector<ExprNodePtr> elements;

private:
    TypePtr arrayType;
};

#endif //ARRAYNODE_H
