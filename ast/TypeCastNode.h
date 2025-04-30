//
// Created by vadim on 20.04.25.
//

#ifndef TYPECASTNODE_H
#define TYPECASTNODE_H

#include "BaseNode.h"

class TypeCastNode final : public ExpressionNode {
public:
    TypeCastNode(ExprNodePtr expr, TypePtr targetType);

    void visit(NodeVisitor *visitor) override;
    [[nodiscard]] std::string toString() const override;
    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    ExprNodePtr expr;
    TypePtr targetType;
};

#endif //TYPECASTNODE_H
