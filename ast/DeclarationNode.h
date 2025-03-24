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
                    std::optional<std::unique_ptr<ExpressionNode>> init);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    std::unique_ptr<IdentNode> ident;
    TypeNode type;
    std::optional<std::unique_ptr<ExpressionNode>> init;
};


#endif //DECLARATIONNODE_H
