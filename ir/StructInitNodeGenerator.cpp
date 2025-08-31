//
// Created by vadim on 09.08.2025.
//

#include "StructInitNodeGenerator.h"
#include "IRTypeFactory.h"

IRValueOpt StructInitNodeGenerator::generateT(StructInitNode *node, ModuleContext &mc) const {
    const auto type = IRTypeFactory::from(node->getType(), *mc.context);
    return IRValue::createConstant(type->createUndef(node, mc), type);
}
