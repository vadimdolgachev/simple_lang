//
// Created by vadim on 23.03.25.
//

#ifndef TYPEFACTORY_H
#define TYPEFACTORY_H

#include "ModuleContext.h"
#include "StrIRType.h"

#include "ast/BaseNode.h"
#include "ast/AssignmentNode.h"
#include "ast/TypeNode.h"

class TypeFactory final : public NodeVisitor {
public:
    void visit(const IdentNode *node) override;

    void visit(const NumberNode *node) override;

    void visit(const StringNode *node) override;

    void visit(const BooleanNode *node) override;

    static bool isCompatible(const TypeNode &t1, const TypeNode &t2);

    static TypeNode getResultType(const TypeNode &t1, const TypeNode &t2);

    void visit(const BinOpNode *node) override;

    static bool isAssignable(const TypeNode &target, const TypeNode &source);

    void visit(const AssignmentNode *node) override;

    void visit(const UnaryOpNode *node) override;

    void visit(const TernaryOperatorNode *node) override;

    void visit(const FunctionCallNode *node) override;

    void visit(const FunctionNode *node) override;

    void visit(const MethodCallNode *node) override;

    void visit(const FieldAccessNode *node) override;

    void visit(const DeclarationNode *node) override;

    [[nodiscard]] std::unique_ptr<IRType> create() const;

    static std::unique_ptr<IRType> from(const ExpressionNode *node,
                                        const ModuleContext &mc);

    static std::unique_ptr<IRType> from(TypeKind kind, bool isPointer);

    static std::unique_ptr<IRType> from(const TypeNode &typeNode);

    void visit(const ProtoFunctionStatement *node) override {}
    void visit(const IfStatement *node) override {}
    void visit(const LoopCondNode *node) override {}
    void visit(const BlockNode *node) override {}
    void visit(const ReturnNode *node) override {}
    void visit(const CommentNode *node) override {}

private:
    explicit TypeFactory(const ModuleContext &mc);

    static TypeNode determine(const ExpressionNode *node,
                              const ModuleContext &mc);

    const ModuleContext &mc;
    std::optional<TypeNode> typeNode;
};

#endif //TYPEFACTORY_H
