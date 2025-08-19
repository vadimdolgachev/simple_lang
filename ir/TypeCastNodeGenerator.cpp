//
// Created by vadim on 14.06.2025.
//

#include "TypeCastNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/TypeCastNode.h"

IRValueOpt TypeCastNodeGenerator::generateT(TypeCastNode *node, ModuleContext &mc) const {
    const auto value = LLVMCodegen::generate(node->expr.get(), mc).value();
    const auto irType = IRTypeFactory::from(node->targetType, mc.module->getContext());
    return IRValue::createValue(
            tryCastValue(mc.builder, value.createLoad(*mc.builder), irType->getLLVMType(mc.module->getContext())),
            irType);
}
