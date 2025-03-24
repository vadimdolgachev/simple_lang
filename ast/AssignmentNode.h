//
// Created by vadim on 06.10.24.
//

#ifndef ASSIGNMENT_NODE_H
#define ASSIGNMENT_NODE_H

#include <memory>

#include "BaseNode.h"

class AssignmentNode final : public ExpressionNode {
public:
    AssignmentNode(std::unique_ptr<IdentNode> lvalue,
                   std::unique_ptr<ExpressionNode> rvalue);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<IdentNode> lvalue;
    const std::unique_ptr<ExpressionNode> rvalue;
};

#endif //ASSIGNMENT_NODE_H
