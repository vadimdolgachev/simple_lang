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
  UnaryOpNode(TokenType operatorType,
              std::unique_ptr<ExpressionNode> expr);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::unique_ptr<ExpressionNode> expr;
  const TokenType operatorType;
};

#endif //UNARYOPAST_H
