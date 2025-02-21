//
// Created by vadim on 16.02.25.
//

#ifndef LOOPCONDNODE_H
#define LOOPCONDNODE_H

#include <memory>
#include <vector>

#include "BaseNode.h"

class LoopCondNode final : public StatementNode {
public:
    enum class LoopType : int8_t {
        While,
        DoWhile,
    };

    LoopCondNode(std::unique_ptr<ExpressionNode> conditional,
                 std::vector<std::unique_ptr<BaseNode>> body,
                 LoopType loopType);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const std::unique_ptr<ExpressionNode> conditional;
    const std::vector<std::unique_ptr<BaseNode>> body;
    const LoopType loopType;
};

#endif //LOOPCONDNODE_H
