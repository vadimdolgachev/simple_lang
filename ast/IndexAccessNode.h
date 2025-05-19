//
// Created by vadim on 18.05.2025.
//

#ifndef INDEXACCESSNODE_H
#define INDEXACCESSNODE_H

#include "BaseNode.h"

class IndexAccessNode final : public ExpressionNode {
public:
    explicit IndexAccessNode(ExprNodePtr object, ExprNodePtr index);

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    [[nodiscard]] TypePtr getType() const override;

    void setType(TypePtr type) override;

    ExprNodePtr object;

    ExprNodePtr index;

private:
    TypePtr resultType;
};

#endif //INDEXACCESSNODE_H
