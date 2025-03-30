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
                ModuleContext &mc);

    void visit(const IdentNode *node) override;

    void visit(const FunctionNode *node) override;

    void visit(const NumberNode *node) override;

    void visit(const StringNode *node) override;

    void visit(const BooleanNode *node) override;

    void visit(const BinOpNode *node) override;

    void visit(const ProtoFunctionStatement *node) override;

    void visit(const AssignmentNode *node) override;

    void visit(const FunctionCallNode *node) override;

    void visit(const IfStatement *node) override;

    void visit(const UnaryOpNode *node) override;

    void visit(const LoopCondNode *node) override;

    void visit(const BlockNode *node) override;

    void visit(const DeclarationNode *node) override;

    void visit(const ReturnNode *node) override;

    void visit(const TernaryOperatorNode *node) override;

    void visit(const MethodCallNode *node) override;

    void visit(const CommentNode *node) override;

    [[nodiscard]] llvm::Value *value() const;

    static llvm::Value *generate(const BaseNode *const node,
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
