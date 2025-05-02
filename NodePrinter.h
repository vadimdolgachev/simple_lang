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

    void visit(IdentNode *node) override;

    void visit(NumberNode *node) override;

    void visit(StringNode *node) override;

    void visit(BinOpNode *node) override;

    void visit(BooleanNode *node) override;

    void visit(FunctionNode *node) override;

    void visit(ProtoFunctionStatement *node) override;

    void visit(AssignmentNode *node) override;

    void visit(FunctionCallNode *node) override;

    void visit(IfStatement *node) override;

    void visit(UnaryOpNode *node) override;

    void visit(LoopCondNode *node) override;

    void visit(BlockNode *node) override;

    void visit(DeclarationNode *node) override;

    void visit(ReturnNode *node) override;

    void visit(TernaryOperatorNode *node) override;

    void visit(MethodCallNode *node) override;

    void visit(FieldAccessNode *node) override;

    void visit(CommentNode *node) override;

    void visit(ModuleNode *node) override;

    void visit(TypeCastNode *node) override;

private:
    void printIndent() const;
    [[nodiscard]] static bool needSemicolon(const std::unique_ptr<BaseNode> &node);

    size_t indent = 0;
    std::ostream &ostream;
};

#endif // NODEPRINTER_H
