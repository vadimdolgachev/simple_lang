//
// Created by vadim on 06.10.24.
//

#ifndef LLVMCODEGEN_H
#define LLVMCODEGEN_H

#include <typeindex>

#include <llvm/IR/IRBuilder.h>

#include "ModuleContext.h"
#include "ast/BaseNode.h"
#include "ast/BlockNode.h"
#include "ir/IRGenerator.h"
#include "ir/IRValue.h"

llvm::Function *getModuleFunction(const std::string &name, const ModuleContext &moduleContext);
void generateBasicBlock(llvm::BasicBlock *basicBlock,
                        const BlockNode::Statements &statements,
                        ModuleContext &moduleContext,
                        const std::optional<std::function<void()>> &prologue = std::nullopt);
llvm::Value *tryCastValue(const std::unique_ptr<llvm::IRBuilder<>> &builder,
                          llvm::Value *value,
                          llvm::Type *destType);

class LLVMCodegen final : public NodeVisitor {
public:
    explicit LLVMCodegen(ModuleContext &moduleContext);

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

    void visit(ArrayNode *node) override;

    void visit(IndexAccessNode *node) override;

    [[nodiscard]] IRValueOpt value() const;

    static IRValueOpt generate(BaseNode *const node,
                               ModuleContext &mc) {
        LLVMCodegen codegen(mc);
        node->visit(&codegen);
        return codegen.value();
    }

private:
    template<typename T>
    IRValueOpt generateValue(T *const node, ModuleContext &moduleContext) {
        if (const auto &it = generators.find(std::type_index(typeid(T))); it != generators.end()) {
            return it->second->generate(node, moduleContext);
        }
        throw std::runtime_error("No generator found for node type: " + std::string(typeid(T).name()));
    }

    IRValueOpt res;
    ModuleContext &mc;
    std::unordered_map<std::type_index, std::unique_ptr<IRGenerator>> generators;
};

#endif //LLVMCODEGEN_H
