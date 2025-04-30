//
// Created by vadim on 18.03.25.
//

#ifndef TERNARYOPERATORNODE_H
#define TERNARYOPERATORNODE_H

#include <memory>

#include "BaseNode.h"

class TernaryOperatorNode final : public ExpressionNode {
public:
    TernaryOperatorNode(std::shared_ptr<ExpressionNode> cond,
                        std::shared_ptr<ExpressionNode> trueExpr,
                        std::shared_ptr<ExpressionNode> falseExpr);

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    TypePtr getType() const override;
    void setType(TypePtr type) override;

    std::shared_ptr<ExpressionNode> cond;
    std::shared_ptr<ExpressionNode> trueExpr;
    std::shared_ptr<ExpressionNode> falseExpr;
};

#endif //TERNARYOPERATORNODE_H
