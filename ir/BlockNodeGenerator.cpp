//
// Created by vadim on 14.06.2025.
//

#include "BlockNodeGenerator.h"

#include "LLVMCodegen.h"

void BlockNodeGenerator::generateT(BlockNode *node, ModuleContext &mc) const {
    if (!mc.builder->GetInsertBlock()) {
        throw std::logic_error("Block generation outside of function context");
    }

    generateBasicBlock(mc.builder->GetInsertBlock(), node->statements, mc);
}
