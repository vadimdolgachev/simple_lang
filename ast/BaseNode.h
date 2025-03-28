//
// Created by vadim on 06.10.24.
//


#ifndef BASEASTNODE_H
#define BASEASTNODE_H

#include <string>

class UnaryOpNode;
class IfStatement;
class FunctionCallNode;
class AssignmentNode;
class ProtoFunctionStatement;
class FunctionNode;
class BinOpNode;
class IdentNode;
class NumberNode;
class StringNode;
class BooleanNode;
class LoopCondNode;
class BlockNode;
class DeclarationNode;
class ReturnNode;
class TernaryOperatorNode;
class MethodCallNode;

class NodeVisitor {
public:
    virtual ~NodeVisitor() = default;

    virtual void visit(const IdentNode *node) = 0;

    virtual void visit(const NumberNode *node) = 0;

    virtual void visit(const StringNode *node) = 0;

    virtual void visit(const BooleanNode *node) = 0;

    virtual void visit(const BinOpNode *node) = 0;

    virtual void visit(const FunctionNode *node) = 0;

    virtual void visit(const ProtoFunctionStatement *node) = 0;

    virtual void visit(const AssignmentNode *node) = 0;

    virtual void visit(const FunctionCallNode *node) = 0;

    virtual void visit(const IfStatement *node) = 0;

    virtual void visit(const UnaryOpNode *node) = 0;

    virtual void visit(const LoopCondNode *node) = 0;

    virtual void visit(const BlockNode *node) = 0;

    virtual void visit(const DeclarationNode *node) = 0;

    virtual void visit(const ReturnNode *node) = 0;

    virtual void visit(const TernaryOperatorNode *node) = 0;

    virtual void visit(const MethodCallNode *node) = 0;
};

class BaseNode {
public:
    virtual ~BaseNode() = default;

    virtual void visit(NodeVisitor *visitor) const = 0;

    [[nodiscard]] virtual std::string toString() const = 0;
};

class StatementNode : public BaseNode {
public:
    ~StatementNode() override = default;
};

class ExpressionNode : public BaseNode {
public:
    ~ExpressionNode() override = default;
};


#endif //BASEASTNODE_H
