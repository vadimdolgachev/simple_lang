//
// Created by vadim on 15.06.2025.
//

#include "StringNodeGenerator.h"

#include "IRTypeFactory.h"

IRValueOpt StringNodeGenerator::generateT(StringNode *node, ModuleContext &mc) const {
    const auto type = IRTypeFactory::from(node->getType(), mc.module->getContext());
    return IRValue::createValue(type->createConstant(node, mc),
                                type,
                                node->text + ".str");
}
