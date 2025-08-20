//
// Created by vadim on 06.10.24.
//

#ifndef PROTOFUNCTIONAST_H
#define PROTOFUNCTIONAST_H

#include <vector>

#include "BaseNode.h"
#include "DeclarationNode.h"
#include "../type/Type.h"

class ProtoFunctionStatement final : public StatementNode {
public:
    ProtoFunctionStatement(std::string name,
                           std::optional<TypePtr> returnType,
                           std::vector<std::unique_ptr<DeclarationNode>> params,
                           bool isVarArgs = false);

    ProtoFunctionStatement(const ProtoFunctionStatement &other);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] BaseNodePtr clone() const override;

    const std::string name;
    TypePtr returnType;
    std::vector<std::unique_ptr<DeclarationNode>> params;
    const bool isVarArgs;
};

#endif //PROTOFUNCTIONAST_H
