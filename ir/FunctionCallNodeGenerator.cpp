//
// Created by vadim on 13.06.2025.
//

#include "FunctionCallNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/FunctionCallNode.h"

IRValueOpt FunctionCallNodeGenerator::generateT(FunctionCallNode *node, ModuleContext &mc) const {
    auto *const calleeFunc = getModuleFunction(node->ident->name, mc);
    if (calleeFunc == nullptr) {
        throw std::runtime_error(std::format("Undefined reference: '{}'", node->ident->name));
    }

    std::vector<llvm::Value *> argsFunc;
    argsFunc.reserve(node->args.size());
    const auto *const funcType = calleeFunc->getFunctionType();
    for (size_t i = 0; i < node->args.size(); ++i) {
        const auto arg = LLVMCodegen::generate(node->args[i].get(), mc);
        if (!arg) {
            throw std::runtime_error("Error argument function generation");
        }
        const auto argType = IRTypeFactory::from(node->args[i]->getType(), mc.module->getContext());
        auto *argValue = arg.value().load(*mc.builder);
        if (i < funcType->getNumParams()) {
            argValue = tryCastValue(mc.builder, argValue, funcType->getParamType(i));
        }
        argsFunc.push_back(argValue);
    }

    auto irType = IRTypeFactory::from(node->getType(), mc.module->getContext());
    return IRValue::createConstant(mc.builder->CreateCall(calleeFunc, argsFunc), std::move(irType));
}
