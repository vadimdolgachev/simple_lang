//
// Created by vadim on 18.03.25.
//

#ifndef TERNARYOPERATORNODE_H
#define TERNARYOPERATORNODE_H

#include <memory>

#include "BaseNode.h"

class TernaryOperatorNode final : public ExpressionNode {
public:
    TernaryOperatorNode(ExprNodePtr cond,
                        ExprNodePtr trueExpr,
                        ExprNodePtr falseExpr);

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    ExprNodePtr cond;
    ExprNodePtr trueExpr;
    ExprNodePtr falseExpr;
};

#endif //TERNARYOPERATORNODE_H
