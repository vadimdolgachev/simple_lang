//
// Created by vadim on 22.02.2025.
//

#ifndef BLOCKNODE_H
#define BLOCKNODE_H

#include <vector>
#include <memory>

#include "BaseNode.h"

class BlockNode final : public StatementNode {
public:
    using Statements = std::vector<BaseNodePtr>;

    explicit BlockNode(Statements statements);

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    Statements statements;
};

#endif //BLOCKNODE_H
