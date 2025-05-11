//
// Created by vadim on 19.03.25.
//

#include "IRType.h"

IRType::IRType(const bool isPointer):
    isPointer(isPointer) {}

void IRType::registerCustomOperation(const TokenType op, llvm::Function *function) {}

llvm::Value *IRType::createMethodCall(llvm::IRBuilder<> &builder,
                                      const MethodInfoPtr &methodInfo,
                                      llvm::Value *object,
                                      const std::vector<llvm::Value *> &args) const {
    throw std::runtime_error("Method '" + methodInfo->name + "' not supported");
}
