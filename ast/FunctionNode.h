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
    FunctionNode(std::unique_ptr<IdentNode> name,
                 std::vector<std::unique_ptr<IdentNode>> params,
                 std::vector<std::unique_ptr<BaseNode>> body);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<IdentNode> name;
    const std::vector<std::unique_ptr<IdentNode>> params;
    const std::vector<std::unique_ptr<BaseNode>> body;
};

#endif //FUNCTIONAST_H
