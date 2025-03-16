//
// Created by vadim on 16.03.25.
//

#ifndef RETURNNODE_H
#define RETURNNODE_H

#include <memory>

#include "BaseNode.h"

class ReturnNode final : public BaseNode {
public:
    explicit ReturnNode(std::unique_ptr<ExpressionNode> expr);

    void visit(NodeVisitor *visitor) const override;

    [[nodiscard]] std::string toString() const override;

    const std::unique_ptr<ExpressionNode> expr;
};


#endif //RETURNNODE_H
