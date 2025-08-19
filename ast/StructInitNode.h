//
// Created by vadim on 03.08.2025.
//

#ifndef STRUCTINITNODE_H
#define STRUCTINITNODE_H

#include "BaseNode.h"
#include "type/StructType.h"

class StructInitNode final : public ExpressionNode {
public:
    StructInitNode(std::string ident, Designator designator);

    void visit(NodeVisitor *visitor) override;
    [[nodiscard]] std::string toString() const override;
    [[nodiscard]] TypePtr getType() const override;
    void setType(TypePtr type) override;

    const std::string ident;
    StructTypePtr type;
    Designator designator;
};

#endif //STRUCTINITNODE_H
