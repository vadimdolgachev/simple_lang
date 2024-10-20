//
// Created by vadim on 06.10.24.
//

#ifndef IRCODEGEN_H
#define IRCODEGEN_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "ast/BaseNode.h"

class CallFunctionNode;
class VariableDefinitionStatement;
class ProtoFunctionStatement;
class FunctionNode;
class BinOpNode;
class NumberNode;
class VariableAccessNode;

class IRCodegen final : public NodeVisitor {
public:
    IRCodegen(const std::unique_ptr<llvm::LLVMContext> &llvmContext,
              const std::unique_ptr<llvm::IRBuilder<> > &llvmIRBuilder,
              const std::unique_ptr<llvm::Module> &llvmModule,
              std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement>> &functionProtos,
              std::unordered_map<std::string, llvm::Value *> &namedValues);

    void visit(const VariableAccessNode *node) override;

    void visit(const FunctionNode *node) override;

    void visit(const NumberNode *node) override;

    void visit(const BinOpNode *node) override;

    void visit(const ProtoFunctionStatement *node) override;

    void visit(const VariableDefinitionStatement *node) override;

    void visit(const CallFunctionNode *node) override;

    void visit(const IfStatementStatement *node) override;

    void visit(const ForLoopNode *node) override;

    void visit(const UnaryOpNode *node) override;

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
