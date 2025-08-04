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
class StructNode;
class StructInitNode;

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
    virtual void visit(StructNode *node) = 0;
    virtual void visit(StructInitNode *node) = 0;
};

class BaseNodeVisitor : public NodeVisitor {
public:
    void visit(IdentNode *node) override {}
    void visit(NumberNode *node) override {}
    void visit(StringNode *node) override {}
    void visit(BooleanNode *node) override {}
    void visit(BinOpNode *node) override {}
    void visit(FunctionNode *node) override {}
    void visit(ProtoFunctionStatement *node) override {}
    void visit(AssignmentNode *node) override {}
    void visit(FunctionCallNode *node) override {}
    void visit(IfStatement *node) override {}
    void visit(UnaryOpNode *node) override {}
    void visit(LoopCondNode *node) override {}
    void visit(BlockNode *node) override {}
    void visit(DeclarationNode *node) override {}
    void visit(ReturnNode *node) override {}
    void visit(TernaryOperatorNode *node) override {}
    void visit(MethodCallNode *node) override {}
    void visit(FieldAccessNode *node) override {}
    void visit(CommentNode *node) override {}
    void visit(ModuleNode *node) override {}
    void visit(TypeCastNode *node) override {}
    void visit(ArrayNode *node) override {}
    void visit(IndexAccessNode *node) override {}
    void visit(StructNode *node) override {}
    void visit(StructInitNode *node) override {}

protected:
    BaseNodeVisitor() = default;
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
std::optional<const T *> asNode(const BaseNode *node) {
    if (const auto *ptr = dynamic_cast<const T *>(node)) {
        return ptr;
    }
    return std::nullopt;
}

template<typename T>
bool isNode(const BaseNode *node) {
    return asNode<T>(node) != std::nullopt;
}

#endif //BASEASTNODE_H
