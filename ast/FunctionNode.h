//
// Created by vadim on 06.10.24.
//

#ifndef FUNCTIONAST_H
#define FUNCTIONAST_H

#include <memory>
#include <vector>

#include "BaseNode.h"
#include "BlockNode.h"

class FunctionNode final : public StatementNode {
public:
    FunctionNode(std::unique_ptr<IdentNode> name,
                 std::vector<std::unique_ptr<IdentNode>> params,
                 std::unique_ptr<BlockNode> body);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<IdentNode> name;
    const std::vector<std::unique_ptr<IdentNode>> params;
    const std::unique_ptr<BlockNode> body;
};

#endif //FUNCTIONAST_H
