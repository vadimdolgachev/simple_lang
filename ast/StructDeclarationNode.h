#ifndef STRUCTDECLARATIONNODE_H
#define STRUCTDECLARATIONNODE_H

#include "ast/BaseNode.h"
#include "ast/DeclarationNode.h"

class StructDeclarationNode final : public StatementNode {
public:
    using MemberNode = std::variant<NodePtr<DeclarationNode>, TypePtr>;

    StructDeclarationNode(std::string name, std::vector<MemberNode> members);

    void visit(NodeVisitor *visitor) override;
    [[nodiscard]] std::string toString() const override;

    const std::string name;
    std::vector<MemberNode> members;
};

#endif // STRUCTDECLARATIONNODE_H
