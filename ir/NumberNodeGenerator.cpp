//
// Created by vadim on 15.06.2025.
//

#include "NumberNodeGenerator.h"

#include "IRTypeFactory.h"

IRValueOpt NumberNodeGenerator::generateT(NumberNode *node, ModuleContext &mc) const {
    const auto type = IRTypeFactory::from(node->getType(), *mc.context);
    return IRValue::createConstant(type->createConstant(node, mc), type);
}
