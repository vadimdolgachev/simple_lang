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
                std::unique_ptr<ExpressionNode> expr);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const TokenType operatorType;
    const UnaryOpType unaryPosType;
    const std::unique_ptr<ExpressionNode> expr;
};

#endif //UNARYOPAST_H
