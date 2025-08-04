//
// Created by vadim on 03.08.2025.
//

#ifndef STRUCTINITNODE_H
#define STRUCTINITNODE_H

#include "BaseNode.h"
#include "IdentNode.h"
#include "type/StructType.h"

class StructInitNode final : public ExpressionNode {
public:
    using Designator = std::vector<std::pair<NodePtr<IdentNode>, ExprNodePtr>>;

    StructInitNode(NodePtr<IdentNode> ident, Designator designator);

    void visit(NodeVisitor *visitor) override;
    [[nodiscard]] std::string toString() const override;
    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    NodePtr<IdentNode> ident;
    Designator designator;
    StructTypePtr type;
};

#endif //STRUCTINITNODE_H
