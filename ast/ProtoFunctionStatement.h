//
// Created by vadim on 06.10.24.
//

#ifndef PROTOFUNCTIONAST_H
#define PROTOFUNCTIONAST_H

#include <vector>

#include "BaseNode.h"
#include "DeclarationNode.h"
#include "TypeNode.h"

class ProtoFunctionStatement final : public StatementNode {
public:
    ProtoFunctionStatement(std::string name,
                           TypeNode returnType,
                           std::vector<DeclarationNode> params,
                           bool isVarArgs = false);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    std::string name;
    TypeNode returnType;
    std::vector<DeclarationNode> params;
    const bool isVarArgs;
};

#endif //PROTOFUNCTIONAST_H
