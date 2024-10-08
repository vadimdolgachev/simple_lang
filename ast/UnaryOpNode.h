//
// Created by vadim on 06.10.24.
//

#ifndef UNARYOPAST_H
#define UNARYOPAST_H

#include <memory>

#include "BaseNode.h"

class UnaryOpNode final : public ExpressionNode {
public:
  UnaryOpNode(OperatorType operatorType,
              std::unique_ptr<ExpressionNode> expr);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::unique_ptr<ExpressionNode> expr;
  const OperatorType operatorType;
};

#endif //UNARYOPAST_H
