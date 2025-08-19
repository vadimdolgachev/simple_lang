//
// Created by vadim on 17.08.2025.
//

#include "FieldAccessNodeGenerator.h"

#include "IRTypeFactory.h"
#include "ast/IdentNode.h"

IRValueOpt FieldAccessNodeGenerator::generateT(FieldAccessNode *node, ModuleContext &mc) const {
    const auto fieldIRType = IRTypeFactory::from(node->getType(), mc.module->getContext());
    const auto structIRType = IRTypeFactory::from(node->object->getType(), mc.module->getContext());
    if (const auto identNode = asNode<IdentNode>(node->object.get())) {
        if (const auto si = mc.symTable.lookup(identNode.value()->name)) {
            if (const auto alloca = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(si.value())) {
                const auto fieldIndex = node->object->getType()->asStruct().value()->findFieldIndex(node->field->name);
                return IRValue::createValue(
                        mc.builder->CreateStructGEP(
                                structIRType->getLLVMType(mc.builder->getContext()),
                                alloca->inst,
                                fieldIndex.value(),
                                node->field->name + ".ptr"),
                        fieldIRType);
            }
        }
    }
    return {};
}
