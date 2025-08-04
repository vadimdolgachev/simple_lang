#ifndef STRUCTNODE_H
#define STRUCTNODE_H

#include "ast/BaseNode.h"
#include "ast/DeclarationNode.h"

class StructNode final : public StatementNode {
public:
    using MemberNode = std::variant<NodePtr<DeclarationNode>, TypePtr>;

    StructNode(std::string name, std::vector<MemberNode> members);

    void visit(NodeVisitor *visitor) override;
    [[nodiscard]] std::string toString() const override;

    std::string name;
    std::vector<MemberNode> members;
};

#endif
