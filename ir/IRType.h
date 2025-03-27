//
// Created by vadim on 19.03.25.
//

#ifndef IRTYPE_H
#define IRTYPE_H

#include <llvm/IR/Type.h>
#include <llvm/IR/IRBuilder.h>

#include "LLVMCodegen.h"
#include "Lexer.h"
#include "ast/BaseNode.h"

class IRType {
public:
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

protected:
    bool isPointer;
};


#endif //IRTYPE_H
