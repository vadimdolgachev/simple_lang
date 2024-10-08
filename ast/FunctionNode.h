//
// Created by vadim on 06.10.24.
//

#ifndef FUNCTIONAST_H
#define FUNCTIONAST_H

#include <list>
#include <memory>

#include "BaseNode.h"
#include "ProtoFunctionStatement.h"

class FunctionNode final : public StatementNode {
public:
  FunctionNode(std::unique_ptr<ProtoFunctionStatement> proto,
               std::list<std::unique_ptr<BaseNode>> body);

  [[nodiscard]] std::string toString() const override;

  void visit(NodeVisitor *visitor) const override;

  const std::unique_ptr<ProtoFunctionStatement> proto;
  const std::list<std::unique_ptr<BaseNode>> body;
};

#endif //FUNCTIONAST_H
