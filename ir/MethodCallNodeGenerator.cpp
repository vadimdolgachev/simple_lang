//
// Created by vadim on 14.06.2025.
//

#include "MethodCallNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/MethodCallNode.h"
#include "type/FunctionType.h"

IRValueOpt MethodCallNodeGenerator::generateT(MethodCallNode *node, ModuleContext &mc) const {
    const auto objectType = IRTypeFactory::from(node->object->getType(), *mc.context);
    std::vector<llvm::Value *> args;
    args.reserve(node->method->args.size());
    for (const auto &arg: node->method->args) {
        args.push_back(LLVMCodegen::generate(arg.get(), mc).value().load(*mc.builder));
    }

    if (const auto fType = node->method->getType()->asFunction()) {
        const auto methodInfo = MethodInfo::create(node->method->ident->name,
                                                   fType.value());
        return IRValue::createConstant(objectType->createMethodCall(*mc.builder,
                                                                 methodInfo,
                                                                 LLVMCodegen::generate(node->object.get(), mc).value().
                                                                 getRawValue(),
                                                                 args), IRTypeFactory::from(
                                            fType.value()->returnType(), *mc.context));
    }
    throw std::runtime_error("Wrong method type");
}
