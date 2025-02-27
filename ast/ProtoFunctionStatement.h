//
// Created by vadim on 06.10.24.
//

#ifndef PROTOFUNCTIONAST_H
#define PROTOFUNCTIONAST_H

#include <vector>

#include "BaseNode.h"

class ProtoFunctionStatement final : public StatementNode {
public:
    ProtoFunctionStatement(std::string name,
                           std::vector<std::string> params,
                           bool isVarArgs = false);

    [[nodiscard]] std::string toString() const override;

    void visit(NodeVisitor *visitor) const override;

    std::string name;
    std::vector<std::string> params;
    const bool isVarArgs;
};

#endif //PROTOFUNCTIONAST_H
