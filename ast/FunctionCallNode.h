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
                     std::vector<ExprNodePtr> args);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    const std::unique_ptr<IdentNode> ident;
    std::vector<ExprNodePtr> args;

private:
    TypePtr type;
};

#endif // CALLFUNCTIONEXPR_H
