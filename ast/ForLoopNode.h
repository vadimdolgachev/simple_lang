//
// Created by vadim on 06.10.24.
//

#ifndef FORLOOPSTATEMENT_H
#define FORLOOPSTATEMENT_H

#include <memory>

#include "DeclarationNode.h"
#include "BlockNode.h"

class ForLoopNode final : public StatementNode {
public:
    ForLoopNode(std::unique_ptr<DeclarationNode> initExpr,
                std::unique_ptr<ExpressionNode> nextExpr,
                std::unique_ptr<ExpressionNode> endExpr,
                std::unique_ptr<BlockNode> body);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<DeclarationNode> init;
    const std::unique_ptr<ExpressionNode> next;
    const std::unique_ptr<ExpressionNode> conditional;
    const std::unique_ptr<BlockNode> body;
};

#endif //FORLOOPSTATEMENT_H
