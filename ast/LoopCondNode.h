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
                 std::optional<BaseNodePtr> init = std::nullopt,
                 std::optional<ExprNodePtr> increment = std::nullopt);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    const Type loopType;
    CondBranch condBranch;
    std::optional<BaseNodePtr> init;
    std::optional<ExprNodePtr> increment;
};

#endif //LOOPCONDNODE_H
