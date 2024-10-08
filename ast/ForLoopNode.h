//
// Created by vadim on 06.10.24.
//

#ifndef FORLOOPSTATEMENT_H
#define FORLOOPSTATEMENT_H

#include <list>
#include <memory>

#include "BaseNode.h"

class ForLoopNode final : public StatementNode {
public:
  ForLoopNode(std::unique_ptr<StatementNode> initExpr,
              std::unique_ptr<ExpressionNode> nextExpr,
              std::unique_ptr<ExpressionNode> endExpr,
              std::list<std::unique_ptr<BaseNode>> body);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::unique_ptr<BaseNode> init;
  const std::unique_ptr<ExpressionNode> next;
  const std::unique_ptr<ExpressionNode> conditional;
  const std::list<std::unique_ptr<BaseNode>> body;
};

#endif //FORLOOPSTATEMENT_H
