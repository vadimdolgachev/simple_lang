//
// Created by vadim on 15.06.2025.
//

#include "BooleanNodeGenerator.h"

#include "IRTypeFactory.h"

IRValueOpt BooleanNodeGenerator::generateT(BooleanNode *node, ModuleContext &mc) const {
    const auto type = IRTypeFactory::from(node->getType(), mc.module->getContext());
    return IRValue::createValue(type->createConstant(node, *mc.builder, *mc.module), type);
}
