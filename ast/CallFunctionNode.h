//
// Created by vadim on 06.10.24.
//

#ifndef CALLFUNCTIONEXPR_H
#define CALLFUNCTIONEXPR_H

#include <memory>
#include <vector>

#include "BaseNode.h"

class CallFunctionNode final : public ExpressionNode {
public:
  CallFunctionNode(std::string callee,
                   std::vector<std::unique_ptr<ExpressionNode>> args)
      : callee(std::move(callee)), args(std::move(args)) {}

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::string callee;
  const std::vector<std::unique_ptr<ExpressionNode>> args;
};

#endif // CALLFUNCTIONEXPR_H
