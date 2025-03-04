//
// Created by vadim on 06.10.24.
//

#ifndef LLVMCODEGEN_H
#define LLVMCODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "ast/BaseNode.h"

class LLVMCodegen final : public NodeVisitor {
public:
    LLVMCodegen(const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder,
                const std::unique_ptr<llvm::Module> &module,
                std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &functionProtos,
                std::unordered_map<std::string, llvm::AllocaInst *> &namedValues);

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

    void visit(const ForLoopNode *node) override;

    void visit(const UnaryOpNode *node) override;

    void visit(const LoopCondNode *node) override;

    void visit(const BlockNode *node) override;

    [[nodiscard]] llvm::Value *value() const;

    static llvm::Value *generate(const BaseNode *const node,
                                 const std::unique_ptr<llvm::IRBuilder<>> &llvmIRBuilder,
                                 const std::unique_ptr<llvm::Module> &llvmModule,
                                 std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues,
                                 std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &
                                 functionProtos,
                                 std::unordered_map<std::string, llvm::AllocaInst *> &namedValues) {
        LLVMCodegen codegen(llvmIRBuilder, llvmModule, globalValues, functionProtos, namedValues);
        node->visit(&codegen);
        return codegen.value();
    }

private:
    llvm::Value *value_ = nullptr;
    const std::unique_ptr<llvm::IRBuilder<>> &iRBuilder;
    const std::unique_ptr<llvm::Module> &module;
    std::unordered_map<std::string, llvm::GlobalVariable *> &globalValues;
    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &functionProtos;
    std::unordered_map<std::string, llvm::AllocaInst *> &namedValues;
};

#endif //LLVMCODEGEN_H
