//
// Created by vadim on 16.02.25.
//

#ifndef LOOPCONDNODE_H
#define LOOPCONDNODE_H

#include <memory>
#include <optional>

#include "BaseNode.h"
#include "CondBranch.h"

class LoopCondNode final : public StatementNode {
public:
    enum class Type : int8_t {
        For,
        While,
        DoWhile,
    };

    LoopCondNode(Type loopType,
                 CondBranch condBranch,
                 std::optional<std::unique_ptr<BaseNode>> init = std::nullopt,
                 std::optional<std::unique_ptr<ExpressionNode>> increment = std::nullopt);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const Type loopType;
    const CondBranch condBranch;
    const std::optional<std::unique_ptr<BaseNode>> init;
    const std::optional<std::unique_ptr<ExpressionNode>> increment;
};

#endif //LOOPCONDNODE_H
