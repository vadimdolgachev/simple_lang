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
#include "CondBranch.h"

class IfStatement final : public StatementNode {
public:
    IfStatement(CondBranch ifBranch,
                std::vector<CondBranch> elseIfBranches,
                std::optional<std::unique_ptr<BlockNode>> elseBranch);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    CondBranch ifBranch;
    std::vector<CondBranch> elseIfBranches;
    std::optional<std::unique_ptr<BlockNode>> elseBranch;
};

#endif //IFSTATEMENT_H
