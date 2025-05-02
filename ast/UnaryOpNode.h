//
// Created by vadim on 06.10.24.
//

#ifndef UNARYOPAST_H
#define UNARYOPAST_H

#include <memory>

#include "BaseNode.h"
#include "Lexer.h"

class UnaryOpNode final : public ExpressionNode {
public:
    enum class UnaryOpType : int8_t {
        Postfix,
        Prefix
    };

    UnaryOpNode(TokenType operatorType,
                UnaryOpType unaryPosType,
                ExprNodePtr expr);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    const TokenType operatorType;
    const UnaryOpType unaryPosType;
    ExprNodePtr expr;
};

#endif //UNARYOPAST_H
