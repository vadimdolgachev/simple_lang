//
// Created by vadim on 04.03.25.
//

#ifndef DECLARATIONNODE_H
#define DECLARATIONNODE_H

#include <memory>
#include <optional>

#include "BaseNode.h"
#include "IdentNode.h"
#include "TypeNode.h"

class DeclarationNode final : public BaseNode {
public:
    DeclarationNode(std::unique_ptr<IdentNode> ident,
                    TypeNode type,
                    std::optional<std::unique_ptr<ExpressionNode>> init,
                    bool isGlobal);

    DeclarationNode(const DeclarationNode &other);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    [[nodiscard]] std::unique_ptr<BaseNode> clone() const override;

    const std::unique_ptr<IdentNode> ident;
    const TypeNode type;
    const std::optional<std::unique_ptr<ExpressionNode>> init;
    const bool isGlobal;
};


#endif //DECLARATIONNODE_H
