//
// Created by vadim on 01.06.2025.
//

#include "IdentNodeGenerator.h"
#include "ast/IdentNode.h"
#include "./IRTypeFactory.h"

IRValueOpt IdentNodeGenerator::generateT(IdentNode *node, ModuleContext &mc) const {
    if (const auto symbol = mc.symTable.lookup(node->name)) {
        if (const auto &global = std::dynamic_pointer_cast<const GlobalSymbolInfo>(symbol.value())) {
            return IRValue::createMemory(global->var,
                                        IRTypeFactory::from(global->type, mc.module->getContext()),
                                        node->name + ".global");
        }
        if (const auto &alloca = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(symbol.value())) {
            if (alloca->inst == nullptr) {
                throw std::runtime_error(std::format("Unknown variable name: {}", node->name));
            }

            return IRValue::createMemory(alloca->inst,
                                        IRTypeFactory::from(alloca->type, mc.module->getContext()),
                                        node->name + ".local");
        }
    }

    throw std::runtime_error(std::format("Undefined variable: {}", node->name));
}

