//
// Created by vadim on 18.03.25.
//

#ifndef TERNARYOPERATORNODE_H
#define TERNARYOPERATORNODE_H

#include <memory>

#include "BaseNode.h"

class TernaryOperatorNode final : public ExpressionNode {
public:
    TernaryOperatorNode(std::unique_ptr<ExpressionNode> cond,
                        std::unique_ptr<ExpressionNode> trueExpr,
                        std::unique_ptr<ExpressionNode> falseExpr);

    void visit(NodeVisitor *visitor) const override;

    [[nodiscard]] std::string toString() const override;

    const std::unique_ptr<ExpressionNode> cond;
    const std::unique_ptr<ExpressionNode> trueExpr;
    const std::unique_ptr<ExpressionNode> falseExpr;
};

#endif //TERNARYOPERATORNODE_H
