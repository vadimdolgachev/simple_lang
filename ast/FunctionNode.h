//
// Created by vadim on 06.10.24.
//

#ifndef FUNCTIONAST_H
#define FUNCTIONAST_H

#include <memory>

#include "BaseNode.h"
#include "BlockNode.h"

class FunctionNode final : public StatementNode {
public:
    FunctionNode(std::shared_ptr<ProtoFunctionStatement> proto,
                 std::unique_ptr<BlockNode> body);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    std::shared_ptr<ProtoFunctionStatement> proto;
    std::unique_ptr<BlockNode> body;
};

#endif //FUNCTIONAST_H
