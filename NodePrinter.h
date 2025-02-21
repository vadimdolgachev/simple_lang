//
// Created by vadim on 03.11.24.
//

#ifndef NODEPRINTER_H
#define NODEPRINTER_H

#include <iostream>
#include "ast/BaseNode.h"

class NodePrinter final : public NodeVisitor {
public:
    explicit NodePrinter(std::ostream &os = std::cout);

    void visit(const IdentNode *node) override;

    void visit(const NumberNode *node) override;

    void visit(const StringNode *node) override;

    void visit(const BinOpNode *node) override;

    void visit(const BooleanNode *node) override;

    void visit(const FunctionNode *node) override;

    void visit(const ProtoFunctionStatement *node) override;

    void visit(const AssignmentNode *node) override;

    void visit(const FunctionCallNode *node) override;

    void visit(const IfStatement *node) override;

    void visit(const ForLoopNode *node) override;

    void visit(const UnaryOpNode *node) override;

    void visit(const LoopCondNode *node) override;
private:
    std::ostream &ostream;
};

#endif // NODEPRINTER_H
