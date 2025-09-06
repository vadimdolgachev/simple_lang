//
// Created by vadim on 06.10.24.
//

#ifndef ASSIGNMENT_NODE_H
#define ASSIGNMENT_NODE_H

#include <memory>

#include "BaseNode.h"

class AssignmentNode final : public ExpressionNode {
public:
    AssignmentNode(ExprNodePtr lvalue,
                   ExprNodePtr rvalue);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    ExprNodePtr lvalue;
    ExprNodePtr rvalue;

private:
    TypePtr type;
};

#endif //ASSIGNMENT_NODE_H
