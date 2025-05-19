//
// Created by vadim on 06.10.24.
//


#ifndef BASEASTNODE_H
#define BASEASTNODE_H

#include <memory>
#include <string>

#include "../type/Type.h"

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
class MemberAccessNode;
class CommentNode;
class MethodCallNode;
class FieldAccessNode;
class ModuleNode;
class TypeCastNode;
class ArrayNode;
class IndexAccessNode;

class NodeVisitor {
public:
    virtual ~NodeVisitor() = default;

    virtual void visit(IdentNode *node) = 0;

    virtual void visit(NumberNode *node) = 0;

    virtual void visit(StringNode *node) = 0;

    virtual void visit(BooleanNode *node) = 0;

    virtual void visit(BinOpNode *node) = 0;

    virtual void visit(FunctionNode *node) = 0;

    virtual void visit(ProtoFunctionStatement *node) = 0;

    virtual void visit(AssignmentNode *node) = 0;

    virtual void visit(FunctionCallNode *node) = 0;

    virtual void visit(IfStatement *node) = 0;

    virtual void visit(UnaryOpNode *node) = 0;

    virtual void visit(LoopCondNode *node) = 0;

    virtual void visit(BlockNode *node) = 0;

    virtual void visit(DeclarationNode *node) = 0;

    virtual void visit(ReturnNode *node) = 0;

    virtual void visit(TernaryOperatorNode *node) = 0;

    virtual void visit(MethodCallNode *node) = 0;

    virtual void visit(FieldAccessNode *node) = 0;

    virtual void visit(CommentNode *node) = 0;

    virtual void visit(ModuleNode *node) = 0;

    virtual void visit(TypeCastNode *node) = 0;

    virtual void visit(ArrayNode *node) = 0;

    virtual void visit(IndexAccessNode *node) = 0;
};

class BaseNode;
using BaseNodePtr = std::unique_ptr<BaseNode>;

class BaseNode {
public:
    virtual ~BaseNode() = default;

    virtual void visit(NodeVisitor *visitor) = 0;

    [[nodiscard]] virtual std::string toString() const = 0;

    [[nodiscard]] virtual BaseNodePtr clone() const {
        throw std::logic_error("Not implemented");
    }
};

class StatementNode : public BaseNode {
public:
    ~StatementNode() override = default;
};

using StmtNodePtr = std::unique_ptr<StatementNode>;

class ExpressionNode : public BaseNode {
public:
    ~ExpressionNode() override = default;

    [[nodiscard]] virtual TypePtr getType() const = 0;

    virtual void setType(TypePtr type) = 0;
};

template<typename T>
using NodePtr = std::unique_ptr<T>;
using ExprNodePtr = NodePtr<ExpressionNode>;

template<typename T>
std::optional<T *> asNode(const BaseNode *node) {
    return dynamic_cast<const T *>(node);
}

#endif //BASEASTNODE_H
