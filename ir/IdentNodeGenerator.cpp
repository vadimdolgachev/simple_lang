//
// Created by vadim on 01.06.2025.
//

#include "IdentNodeGenerator.h"
#include "ast/IdentNode.h"
#include "./IRTypeFactory.h"

IRValue IdentNodeGenerator::generate(BaseNode *node, ModuleContext &mc) const {
    auto *const identNode = dynamic_cast<IdentNode *>(node);
    if (identNode == nullptr) {
        throw std::runtime_error("Expected IdentNode");
    }

    if (const auto symbol = mc.symTable.lookup(identNode->name)) {
        if (const auto &global = std::dynamic_pointer_cast<const GlobalSymbolInfo>(symbol.value())) {
            return IRValue::createGlobal(global->var,
                                        IRTypeFactory::from(global->type, mc.module->getContext()),
                                        identNode->name + ".global");
        }
        if (const auto &alloca = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(symbol.value())) {
            if (alloca->inst == nullptr) {
                throw std::runtime_error(std::format("Unknown variable name: {}", identNode->name));
            }

            return IRValue::createAlloca(alloca->inst,
                                        IRTypeFactory::from(alloca->type, mc.module->getContext()),
                                        identNode->name + ".local");
        }
    }

    throw std::runtime_error(std::format("Undefined variable: {}", identNode->name));
}

