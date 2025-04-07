//
// Created by vadim on 06.04.25.
//

#ifndef FIELDACCESSNODE_H
#define FIELDACCESSNODE_H

#include "MemberAccessNode.h"

class FieldAccessNode final : public MemberAccessNode {
public:
    explicit FieldAccessNode(std::unique_ptr<ExpressionNode> object,
                             std::unique_ptr<IdentNode> field);

    void visit(NodeVisitor *visitor) const override;
    [[nodiscard]] std::string toString() const override;

    const std::unique_ptr<IdentNode> field;
};

#endif //FIELDACCESSNODE_H
