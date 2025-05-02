//
// Created by vadim on 16.03.25.
//

#ifndef RETURNNODE_H
#define RETURNNODE_H

#include <memory>

#include "BaseNode.h"

class ReturnNode final : public StatementNode {
public:
    explicit ReturnNode(ExprNodePtr expr);

    void visit(NodeVisitor *visitor) override;

    [[nodiscard]] std::string toString() const override;

    ExprNodePtr expr;
};


#endif //RETURNNODE_H
