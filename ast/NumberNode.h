//
// Created by vadim on 06.10.24.
//

#ifndef NUMBERAST_H
#define NUMBERAST_H

#include "BaseNode.h"

class NumberNode final : public ExpressionNode {
public:
  explicit NumberNode(double v);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  double value;
};

#endif //NUMBERAST_H
