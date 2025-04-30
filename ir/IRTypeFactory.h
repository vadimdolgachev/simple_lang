//
// Created by vadim on 23.03.25.
//

#ifndef IRTYPEFACTORY_H
#define IRTYPEFACTORY_H

#include "ModuleContext.h"
#include "StrIRType.h"

#include "ast/BaseNode.h"
#include "ast/AssignmentNode.h"
#include "../type/Type.h"

class IRTypeFactory final : public NodeVisitor {
public:
    void visit(IdentNode *node) override;

    void visit(NumberNode *node) override;

    void visit(StringNode *node) override;

    void visit(BooleanNode *node) override;

    void visit(BinOpNode *node) override;

    void visit(AssignmentNode *node) override;

    void visit(UnaryOpNode *node) override;

    void visit(TernaryOperatorNode *node) override;

    void visit(FunctionCallNode *node) override;

    void visit(FunctionNode *node) override;

    void visit(MethodCallNode *node) override;

    void visit(FieldAccessNode *node) override;

    void visit(DeclarationNode *node) override;

    void visit(TypeCastNode *node) override;

    [[nodiscard]] std::unique_ptr<IRType> create() const;

    static std::unique_ptr<IRType> from(ExpressionNode *node,
                                        const ModuleContext &mc);

    static std::unique_ptr<IRType> from(TypeKind kind, bool isPointer);

    void visit(ProtoFunctionStatement *node) override {}
    void visit(IfStatement *node) override {}
    void visit(LoopCondNode *node) override {}
    void visit(BlockNode *node) override {}
    void visit(ReturnNode *node) override {}
    void visit(CommentNode *node) override {}
    void visit(ModuleNode *node) override {}

private:
    explicit IRTypeFactory(const ModuleContext &mc);

    static TypePtr determine(ExpressionNode *node,
                             const ModuleContext &mc);

    const ModuleContext &mc;
    std::optional<TypePtr> typeNode;
};

#endif //IRTYPEFACTORY_H
