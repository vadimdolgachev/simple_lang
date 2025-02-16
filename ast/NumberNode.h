//
// Created by vadim on 06.10.24.
//

#ifndef NUMBER_NODE_H
#define NUMBER_NODE_H

#include "BaseNode.h"

class NumberNode final : public ExpressionNode {
public:
  explicit NumberNode(double v);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  double value;
};

#endif //NUMBER_NODE_H
