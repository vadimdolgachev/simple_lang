//
// Created by vadim on 06.10.24.
//

#ifndef CALLFUNCTIONEXPR_H
#define CALLFUNCTIONEXPR_H

#include <memory>
#include <vector>

#include "BaseNode.h"
#include "IdentNode.h"

class FunctionCallNode final : public ExpressionNode {
public:
    FunctionCallNode(std::unique_ptr<IdentNode> ident,
                     std::vector<std::unique_ptr<ExpressionNode>> args);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<IdentNode> ident;
    const std::vector<std::unique_ptr<ExpressionNode>> args;
};

#endif // CALLFUNCTIONEXPR_H
