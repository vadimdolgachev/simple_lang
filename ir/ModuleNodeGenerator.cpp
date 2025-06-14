//
// Created by vadim on 14.06.2025.
//

#include "ModuleNodeGenerator.h"

#include "LLVMCodegen.h"
#include "ast/ModuleNode.h"

IRValueOpt ModuleNodeGenerator::generateT(ModuleNode *node, ModuleContext &mc) const {
    mc.symTable.enterScope();
    for (const auto &statement: node->statements) {
        LLVMCodegen::generate(statement.get(), mc);
    }
    mc.symTable.exitScope();

    return std::nullopt;
}
