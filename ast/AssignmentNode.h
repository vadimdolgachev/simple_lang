//
// Created by vadim on 06.10.24.
//

#ifndef VARIABLEDEFINITIONAST_H
#define VARIABLEDEFINITIONAST_H

#include <memory>

#include "BaseNode.h"

class AssignmentNode final : public StatementNode {
public:
    AssignmentNode(std::string name,
                   std::unique_ptr<ExpressionNode> rvalue);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::string name;
    const std::unique_ptr<ExpressionNode> rvalue;
};

#endif //VARIABLEDEFINITIONAST_H
