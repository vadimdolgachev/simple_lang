//
// Created by vadim on 06.10.24.
//

#ifndef IFSTATEMENT_H
#define IFSTATEMENT_H

#include <list>
#include <memory>
#include <optional>

#include "BaseNode.h"

class IfStatement final : public StatementNode {
public:
  IfStatement(
      std::unique_ptr<ExpressionNode> cond,
      std::list<std::unique_ptr<BaseNode>> thenBranch,
      std::optional<std::list<std::unique_ptr<BaseNode>>> elseBranch);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::unique_ptr<ExpressionNode> cond;
  const std::list<std::unique_ptr<BaseNode>> thenBranch;
  const std::optional<std::list<std::unique_ptr<BaseNode>>> elseBranch;
};

#endif //IFSTATEMENT_H
