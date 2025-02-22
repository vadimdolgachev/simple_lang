//
// Created by vadim on 06.10.24.
//

#ifndef IRCODEGEN_H
#define IRCODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "ast/BaseNode.h"

class FunctionCallNode;
class AssignmentNode;
class ProtoFunctionStatement;
class FunctionNode;
class BinOpNode;
class NumberNode;
class IdentNode;

class IRCodegen final : public NodeVisitor {
public:
    IRCodegen(const std::unique_ptr<llvm::LLVMContext> &llvmContext,
              const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder,
              const std::unique_ptr<llvm::Module> &llvmModule,
              std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &functionProtos,
              std::unordered_map<std::string, llvm::Value *> &namedValues);

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

private:
    llvm::Value * value_ = nullptr;
    const std::unique_ptr<llvm::LLVMContext> &llvmContext;
    const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder;
    const std::unique_ptr<llvm::Module> &llvmModule;
    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement> > &functionProtos;
    std::unordered_map<std::string, llvm::Value *> &namedValues;
};

inline llvm::Value *generateIR(const BaseNode *const node,
                               const std::unique_ptr<llvm::LLVMContext> &llvmContext,
                               const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder,
                               const std::unique_ptr<llvm::Module> &llvmModule,
                               std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement> > &functionProtos,
                               std::unordered_map<std::string, llvm::Value *> &namedValues) {
    IRCodegen codegen(llvmContext, llvmIRBuilder, llvmModule, functionProtos, namedValues);
    node->visit(&codegen);
    return codegen.value();
}

#endif //IRCODEGEN_H
