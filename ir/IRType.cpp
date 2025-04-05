//
// Created by vadim on 19.03.25.
//

#include "IRType.h"
#include "ast/ProtoFunctionStatement.h"

IRType::IRType(const bool isPointer):
    isPointer(isPointer) {}

void IRType::registerCustomOperation(const TokenType op, llvm::Function *function) {}

const IRType::MethodLists &IRType::methodList() const {
    throw std::logic_error("Not supported");
}

bool IRType::isMethodSupported(const std::string &method) const {
    return findMethodByName(method) != nullptr;
}

const ProtoFunctionStatement *IRType::findMethodByName(const std::string &methodName) const {
    const auto it = std::ranges::find_if(methodList(), [methodName](const auto &method) {
        return method->name == methodName;
    });
    if (it != methodList().end()) {
        return it->get();
    }
    return nullptr;
}

llvm::Value *IRType::createMethodCall(llvm::IRBuilder<> &builder,
                                      const std::string &method,
                                      llvm::Value *object,
                                      const std::vector<llvm::Value *> &args,
                                      const std::string &name) const {
    throw std::runtime_error("Method '" + method + "' not supported");
}
