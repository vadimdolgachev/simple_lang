//
// Created by vadim on 15.04.25.
//

#ifndef MODULENODE_H
#define MODULENODE_H

#include <vector>

#include "BaseNode.h"

class ModuleNode final : public StatementNode {
public:
    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    std::vector<BaseNodePtr> statements;
};

#endif //MODULENODE_H
