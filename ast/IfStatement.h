//
// Created by vadim on 06.10.24.
//

#ifndef IFSTATEMENT_H
#define IFSTATEMENT_H

#include <memory>
#include <optional>
#include <vector>

#include "BaseNode.h"
#include "BlockNode.h"

class IfStatement final : public StatementNode {
public:
    struct CondBranch {
        std::unique_ptr<ExpressionNode> cond;
        std::unique_ptr<BlockNode> then;
    };

    IfStatement(
            CondBranch ifBranch,
            std::vector<CondBranch> elseIfBranches,
            std::optional<std::unique_ptr<BlockNode>> elseBranch);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    const CondBranch ifBranch;
    const std::vector<CondBranch> elseIfBranches;
    const std::optional<std::unique_ptr<BlockNode>> elseBranch;
};

#endif //IFSTATEMENT_H
