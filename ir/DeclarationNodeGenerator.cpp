//
// Created by vadim on 13.06.2025.
//

#include "DeclarationNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/DeclarationNode.h"

namespace {
    IRValue genGlobalDeclaration(const DeclarationNode *node,
                                 llvm::Type *type,
                                 llvm::Value *init,
                                 ModuleContext &mc) {
        llvm::Constant *constInit = nullptr;
        if (init) {
            constInit = llvm::dyn_cast<llvm::Constant>(init);
            if (!constInit) {
                throw std::logic_error(
                        "Global variable initializer must be constant: " + node->ident->name);
            }
        }

        auto *gVar = new llvm::GlobalVariable(*mc.module,
                                              type,
                                              true,
                                              llvm::GlobalValue::InternalLinkage,
                                              constInit,
                                              node->ident->name);

        gVar->setAlignment(llvm::MaybeAlign(8));
        gVar->setDSOLocal(true);

        mc.symTable.insert(node->ident->name, std::make_shared<GlobalSymbolInfo>(node->type, gVar));
        return IRValue::createGlobal(gVar,
                                     IRTypeFactory::from(node->type, mc.module->getContext()),
                                     node->ident->name);
    }

    IRValue genLocalDeclaration(const DeclarationNode *node,
                                llvm::Type *type,
                                llvm::Value *init,
                                ModuleContext &mc) {
        auto *alloca = mc.builder->CreateAlloca(type, nullptr, node->ident->name);

        if (init) {
            auto *const casted = tryCastValue(mc.builder, init, type);
            if (!casted) {
                throw std::logic_error("Type mismatch in initialization of: " + node->ident->name);
            }
            mc.builder->CreateStore(casted, alloca);
        }

        if (mc.symTable.lookup(node->ident->name)) {
            throw std::logic_error("Redeclaration of variable: " + node->ident->name);
        }

        mc.symTable.insert(node->ident->name,
                           std::make_shared<AllocaInstSymbolInfo>(node->type,
                                                                  alloca));
        return IRValue::createAlloca(alloca, IRTypeFactory::from(node->type, mc.module->getContext()),
                                     node->ident->name);
    }
}

IRValueOpt DeclarationNodeGenerator::generateT(DeclarationNode *node, ModuleContext &mc) const {
    const auto irType = IRTypeFactory::from(node->type, mc.module->getContext());
    auto *const llvmType = irType->getLLVMType(mc.module->getContext());
    if (llvmType == nullptr) {
        throw std::logic_error("Unknown type for variable: " + node->ident->name);
    }

    llvm::Value *initValue = nullptr;
    if (node->init.has_value()) {
        const auto valueHandler = LLVMCodegen::generate(node->init.value().get(), mc);
        initValue = valueHandler.value().getRawValue();
        if (node->init.value()->getType()->isArray()
            && isNode<IdentNode>(node->init.value().get())) {
            initValue = valueHandler.value().createLoad(*mc.builder);
        }
        if (!initValue) {
            throw std::logic_error("Failed to generate initializer for: " + node->ident->name);
        }
    } else {
        if (llvmType->isAggregateType()) {
            initValue = llvm::ConstantAggregateZero::get(llvmType);
        } else {
            initValue = llvm::Constant::getNullValue(llvmType);
        }
    }

    if (node->isGlobal) {
        genGlobalDeclaration(node, llvmType, initValue, mc);
    } else {
        genLocalDeclaration(node, llvmType, initValue, mc);
    }

    return std::nullopt;
}
