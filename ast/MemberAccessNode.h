//
// Created by vadim on 27.03.25.
//

#ifndef METHODCALLNODE_H
#define METHODCALLNODE_H

#include <memory>

#include "BaseNode.h"

class MemberAccessNode final : public ExpressionNode {
public:
    explicit MemberAccessNode(std::unique_ptr<ExpressionNode> object,
                              std::unique_ptr<ExpressionNode> member);

    void visit(NodeVisitor *visitor) const override;

    [[nodiscard]] std::string toString() const override;

    const std::unique_ptr<ExpressionNode> object;
    const std::unique_ptr<ExpressionNode> member;
};

#endif //METHODCALLNODE_H
