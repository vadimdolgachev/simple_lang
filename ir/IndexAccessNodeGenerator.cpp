//
// Created by vadim on 14.06.2025.
//

#include "IndexAccessNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/IndexAccessNode.h"
#include "type/ArrayType.h"

IRValueOpt IndexAccessNodeGenerator::generateT(IndexAccessNode *node, ModuleContext &mc) const {
    const auto object = LLVMCodegen::generate(node->object.get(), mc);
    const auto index = LLVMCodegen::generate(node->index.get(), mc);

    if (const auto arrayType = node->object->getType()->asArray()) {
        const auto elementType = arrayType.value()->getElementType();
        const auto arrayIrType = IRTypeFactory::from(arrayType.value(), *mc.context);
        auto *const arrayLlvmType = llvm::dyn_cast<llvm::ArrayType>(arrayIrType->getLLVMType(*mc.context));
        auto elementIrType = IRTypeFactory::from(arrayType.value()->getElementType(), *mc.context);
        const std::vector<llvm::Value *> indices = {
                mc.builder->getInt64(0),
                index.value().load(*mc.builder)
        };
        auto *const elementLlvmValue = mc.builder->CreateLoad(arrayIrType->getLLVMElementType(*mc.context),
                                                              mc.builder->CreateInBoundsGEP(
                                                                      arrayLlvmType,
                                                                      object.value().getRawValue(),
                                                                      indices,
                                                                      "elem_ptr"));
        return IRValue::createConstant(elementLlvmValue, std::move(elementIrType));
    }
    throw std::runtime_error("Unsupported index access type");
}
