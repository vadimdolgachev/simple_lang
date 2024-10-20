//
// Created by vadim on 06.10.24.
//

#ifndef BINOPAST_H
#define BINOPAST_H

#include <memory>

#include "BaseNode.h"
#include "Lexer.h"

class BinOpNode final : public ExpressionNode {
public:
    BinOpNode(TokenType binOp, std::unique_ptr<ExpressionNode> lhs,
              std::unique_ptr<ExpressionNode> rhs);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const TokenType binOp;
    const std::unique_ptr<ExpressionNode> lhs;
    const std::unique_ptr<ExpressionNode> rhs;
};

#endif //BINOPAST_H
