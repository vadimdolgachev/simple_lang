//
// Created by vadim on 25.03.25.
//

#ifndef STRIRTYPE_H
#define STRIRTYPE_H

#include <llvm/IR/Instructions.h>

#include "IRType.h"

class StrIRType final : public IRType {
public:
    explicit StrIRType(bool isPointer = true);

    llvm::Value *createBinaryOp(llvm::IRBuilder<> &builder,
                                TokenType op,
                                llvm::Value *lhs,
                                llvm::Value *rhs,
                                const std::string &name) const override;

    llvm::Value *createUnaryOp(llvm::IRBuilder<> &builder,
                               TokenType op,
                               llvm::Value *operand,
                               llvm::Value *storage,
                               const std::string &name) const override;

    llvm::Type *getLLVMType(llvm::LLVMContext &context) const override;

    llvm::Value *createValue(const BaseNode *node, llvm::IRBuilder<> &builder, llvm::Module &module) override;

    llvm::Value *createMethodCall(llvm::IRBuilder<> &builder,
                                  const MethodInfoPtr &methodInfo,
                                  llvm::Value *object,
                                  const std::vector<llvm::Value *> &args) const override;
private:
    llvm::Function *getOrDeclareStrcmp(llvm::Module *module) const;

    llvm::Function *getOrDeclareStrlen(llvm::Module *module) const;
};


#endif //STRIRTYPE_H
