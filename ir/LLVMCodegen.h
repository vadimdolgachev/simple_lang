//
// Created by vadim on 06.10.24.
//

#ifndef LLVMCODEGEN_H
#define LLVMCODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "ModuleContext.h"
#include "ast/BaseNode.h"

class LLVMCodegen final : public NodeVisitor {
public:
    LLVMCodegen(const std::unique_ptr<llvm::IRBuilder<>> &builder,
                const std::unique_ptr<llvm::Module> &module,
                ModuleContext &mc,
                ExpressionNode *object = nullptr);

    void visit(IdentNode *node) override;

    void visit(FunctionNode *node) override;

    void visit(NumberNode *node) override;

    void visit(StringNode *node) override;

    void visit(BooleanNode *node) override;

    void visit(BinOpNode *node) override;

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

    [[nodiscard]] llvm::Value *value() const;

    static llvm::Value *generate(BaseNode *const node,
                                 const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
                                 const std::unique_ptr<llvm::Module> &llvmModule,
                                 ModuleContext &mc) {
        LLVMCodegen codegen(llvmIRBuilder, llvmModule, mc);
        node->visit(&codegen);
        return codegen.value();
    }

private:
    llvm::Value *value_ = nullptr;
    const std::unique_ptr<llvm::IRBuilder<>> &builder;
    const std::unique_ptr<llvm::Module> &module;
    ModuleContext &mc;
};

#endif //LLVMCODEGEN_H
