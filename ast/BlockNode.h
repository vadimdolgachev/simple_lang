//
// Created by vadim on 22.02.2025.
//

#ifndef BLOCKNODE_H
#define BLOCKNODE_H

#include <vector>
#include <memory>

#include "BaseNode.h"

class BlockNode final : public BaseNode {
public:
    using BlockCode = std::vector<std::unique_ptr<BaseNode>>;

    explicit BlockNode(BlockCode statements);

    void visit(NodeVisitor *visitor) const override;

    [[nodiscard]] std::string toString() const override;

    BlockCode statements;
};

#endif //BLOCKNODE_H
