//
// Created by vadim on 14.06.2025.
//

#include "ArrayNodeGenerator.h"

#include "IRTypeFactory.h"
#include "ast/ArrayNode.h"

IRValueOpt ArrayNodeGenerator::generateT(ArrayNode *node, ModuleContext &mc) const {
    const auto arrayType = IRTypeFactory::from(node->getType(), mc.module->getContext());
    return IRValue::createValue(arrayType->createConstant(node, *mc.builder, *mc.module), arrayType);
}
