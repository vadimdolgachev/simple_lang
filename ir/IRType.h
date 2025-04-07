//
// Created by vadim on 19.03.25.
//

#ifndef IRTYPE_H
#define IRTYPE_H

#include <unordered_set>

#include <llvm/IR/Type.h>
#include <llvm/IR/IRBuilder.h>

#include "Lexer.h"
#include "ast/BaseNode.h"

class IRType {
public:
    using MethodLists = std::unordered_set<std::unique_ptr<ProtoFunctionStatement>>;

    explicit IRType(bool isPointer);

    virtual ~IRType() = default;

    [[nodiscard]] virtual bool isOperationSupported(TokenType op, const IRType *other) const = 0;

    virtual llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                        TokenType op,
                                        llvm::Value *lhs,
                                        llvm::Value *rhs,
                                        const std::string &name) const = 0;

    [[nodiscard]] virtual bool isUnaryOperationSupported(TokenType op) const = 0;

    virtual llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder,
                                       TokenType op,
                                       llvm::Value *operand,
                                       llvm::Value *storage,
                                       const std::string &name) const = 0;

    virtual llvm::Type *getLLVMType(llvm::LLVMContext &context) const = 0;

    void registerCustomOperation(TokenType op,
                                 llvm::Function *function);

    virtual llvm::Value *createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) = 0;

    [[nodiscard]] virtual const MethodLists &methodList() const;

    [[nodiscard]] virtual bool isMethodSupported(const std::string &method) const;

    [[nodiscard]] const ProtoFunctionStatement *findMethodByName(const std::string &methodName) const;

    [[nodiscard]] std::string findField(const std::string &fieldName) const;

    virtual llvm::Value *createMethodCall(llvm::IRBuilder<> &builder,
                                          const std::string &method,
                                          llvm::Value *object,
                                          const std::vector<llvm::Value *> &args,
                                          const std::string &name) const;

protected:
    bool isPointer;
};


#endif //IRTYPE_H
