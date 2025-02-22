//
// Created by vadim on 16.02.25.
//

#ifndef LOOPCONDNODE_H
#define LOOPCONDNODE_H

#include <memory>

#include "BaseNode.h"
#include "BlockNode.h"

class LoopCondNode final : public StatementNode {
public:
    enum class LoopType : int8_t {
        While,
        DoWhile,
    };

    LoopCondNode(std::unique_ptr<ExpressionNode> conditional,
                 std::unique_ptr<BlockNode> body,
                 LoopType loopType);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<ExpressionNode> conditional;
    const std::unique_ptr<BlockNode> body;
    const LoopType loopType;
};

#endif //LOOPCONDNODE_H
