//
// Created by vadim on 06.10.24.
//

#ifndef VARIABLEACCESSAST_H
#define VARIABLEACCESSAST_H

#include <string>

#include "BaseNode.h"

class IdentNode final : public ExpressionNode {
public:
  explicit IdentNode(std::string name);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::string name;
};

#endif //VARIABLEACCESSAST_H
