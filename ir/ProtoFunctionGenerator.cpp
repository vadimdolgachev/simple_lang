//
// Created by vadim on 13.06.2025.
//

#include "ProtoFunctionGenerator.h"

#include "IRTypeFactory.h"
#include "ast/ProtoFunctionStatement.h"
#include "type/TypeFactory.h"
#include "type/FunctionType.h"

IRValueOpt ProtoFunctionGenerator::generateT(ProtoFunctionStatement *node, ModuleContext &mc) const {
    std::vector<llvm::Type *> functionParams;
    functionParams.reserve(node->params.size());
    for (const auto &param: node->params) {
        functionParams.push_back(
            IRTypeFactory::from(param->type, mc.module->getContext())->getLLVMType(mc.module->getContext()));
    }

    auto *const functionType =
            llvm::FunctionType::get(IRTypeFactory::from(node->returnType, mc.module->getContext())
                                    ->getLLVMType(mc.module->getContext()),
                                    functionParams, node->isVarArgs);

    auto *const function = llvm::Function::Create(functionType,
                                                  llvm::Function::ExternalLinkage,
                                                  node->name,
                                                  mc.module.get());
    std::vector<TypePtr> params;
    std::ranges::transform(node->params,
                           std::back_inserter(params),
                           [](const auto &i) {
                               return i->type;
                           });
    mc.symTable.insertFunction(node->name,
                               std::make_shared<SymbolInfo>(TypeFactory::makeFunction(node->returnType, params)));
    // function->addFnAttr(llvm::Attribute::NoUnwind);
    // function->addRetAttr(llvm::Attribute::ZExt);
    for (auto [index, arg]: llvm::enumerate(function->args())) {
        arg.setName(node->params[index]->ident->name);
    }

    return std::nullopt;
}
