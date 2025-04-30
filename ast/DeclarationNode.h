//
// Created by vadim on 04.03.25.
//

#ifndef DECLARATIONNODE_H
#define DECLARATIONNODE_H

#include <memory>
#include <optional>

#include "BaseNode.h"
#include "IdentNode.h"
#include "../type/Type.h"

class DeclarationNode final : public StatementNode {
public:
    DeclarationNode(std::unique_ptr<IdentNode> ident,
                    TypePtr type,
                    std::optional<ExprNodePtr> init,
                    bool isConst,
                    bool isGlobal);

    DeclarationNode(const DeclarationNode &other);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] BaseNodePtr clone() const override;

    const std::unique_ptr<IdentNode> ident;
    const TypePtr type;
    std::optional<ExprNodePtr> init;
    const bool isConst;
    const bool isGlobal;
};


#endif //DECLARATIONNODE_H
