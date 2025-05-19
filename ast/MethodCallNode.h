//
// Created by vadim on 06.04.25.
//

#ifndef METHODCALLNODE_H
#define METHODCALLNODE_H

#include "MemberAccessNode.h"
#include "FunctionCallNode.h"

class MethodCallNode final : public MemberAccessNode {
public:
    MethodCallNode(ExprNodePtr object,
                   std::unique_ptr<FunctionCallNode> method):
        MemberAccessNode(std::move(object)),
        method(std::move(method)) {}

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    [[nodiscard]] TypePtr getType() const override;

    void setType(TypePtr type) override;

    std::unique_ptr<FunctionCallNode> method;
};

#endif //METHODCALLNODE_H
