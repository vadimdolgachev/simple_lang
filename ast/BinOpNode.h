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
    BinOpNode(TokenType binOp,
              ExprNodePtr lhs,
              ExprNodePtr rhs);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] TypePtr getType() const override;

    void setType(TypePtr type) override;

    const TokenType binOp;
    ExprNodePtr lhs;
    ExprNodePtr rhs;

private:
    TypePtr exprType;
};

#endif //BINOPAST_H
