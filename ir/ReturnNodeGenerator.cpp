//
// Created by vadim on 14.06.2025.
//

#include "ReturnNodeGenerator.h"

#include "LLVMCodegen.h"
#include "ast/ReturnNode.h"

void ReturnNodeGenerator::generateT(ReturnNode *node, ModuleContext &mc) const {
    if (node->expr != nullptr) {
        mc.builder->CreateRet(LLVMCodegen::generate(node->expr.get(), mc).value().load(*mc.builder));
    } else {
        mc.builder->CreateRetVoid();
    }
}
