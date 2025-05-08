//
// Created by vadim on 16.04.25.
//

#ifndef SEMAANALYZER_H
#define SEMAANALYZER_H

#include <utility>

#include "ast/BaseNode.h"

#include "SymbolTable.h"

class SemanticAnalyzer final : public NodeVisitor {
public:
    explicit SemanticAnalyzer(SymbolTable symbolTable):
        symbolTable(std::move(symbolTable)) {}

    void visit(IdentNode *node) override;
    void visit(NumberNode *node) override;
    void visit(StringNode *node) override;
    void visit(BooleanNode *node) override;
    void visit(BinOpNode *node) override;
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
    SymbolTable symbolTable;
    std::shared_ptr<const ProtoFunctionStatement> currentFunction;
};

#endif //SEMAANALYZER_H
