//
// Created by vadim on 06.10.24.
//


#ifndef BASEASTNODE_H
#define BASEASTNODE_H

#include <string>
#include <cstdint>

class UnaryOpNode;
class ForLoopNode;
class IfStatementStatement;
class CallFunctionNode;
class VariableDefinitionStatement;
class ProtoFunctionStatement;
class FunctionNode;
class BinOpNode;
class NumberNode;
class VariableAccessNode;

class NodeVisitor {
public:
    virtual ~NodeVisitor() = default;

    virtual void visit(const VariableAccessNode *node) = 0;

    virtual void visit(const NumberNode *node) = 0;

    virtual void visit(const BinOpNode *node) = 0;

    virtual void visit(const FunctionNode *node) = 0;

    virtual void visit(const ProtoFunctionStatement *node) = 0;

    virtual void visit(const VariableDefinitionStatement *node) = 0;

    virtual void visit(const CallFunctionNode *node) = 0;

    virtual void visit(const IfStatementStatement *node) = 0;

    virtual void visit(const ForLoopNode *node) = 0;

    virtual void visit(const UnaryOpNode *node) = 0;
};

enum class OperatorType : std::uint8_t {
    IncrementOperator,
    DecrementOperator,
    UnknownOperator,
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
