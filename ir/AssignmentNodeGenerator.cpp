//
// Created by vadim on 13.06.2025.
//

#include "AssignmentNodeGenerator.h"

#include "LLVMCodegen.h"
#include "ast/AssignmentNode.h"

IRValueOpt AssignmentNodeGenerator::generateT(AssignmentNode *node, ModuleContext &mc) const {
    LLVMCodegen::generate(node->rvalue.get(), mc).value().store(*mc.builder,
                                                                LLVMCodegen::generate(node->lvalue.get(), mc).value().
                                                                getRawValue());
    return std::nullopt;
}
