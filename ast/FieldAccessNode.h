//
// Created by vadim on 06.04.25.
//

#ifndef FIELDACCESSNODE_H
#define FIELDACCESSNODE_H

#include "MemberAccessNode.h"

class FieldAccessNode final : public MemberAccessNode {
public:
    FieldAccessNode(ExprNodePtr object, std::unique_ptr<IdentNode> field);

    void visit(NodeVisitor *visitor) override;
    [[nodiscard]] std::string toString() const override;

    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    std::unique_ptr<IdentNode> field;

private:
    TypePtr type;
};

#endif //FIELDACCESSNODE_H
