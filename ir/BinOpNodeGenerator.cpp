//
// Created by vadim on 13.06.2025.
//

#include "BinOpNodeGenerator.h"

#include "IRTypeFactory.h"
#include "ast/BinOpNode.h"

#include "LLVMCodegen.h"

IRValueOpt BinOpNodeGenerator::generateT(BinOpNode *node, ModuleContext &mc) const {
    auto *const lhsValue = LLVMCodegen::generate(node->lhs.get(), mc).value().createLoad(*mc.builder);
    auto *const rhsValue = LLVMCodegen::generate(node->rhs.get(), mc).value().createLoad(*mc.builder);
    if (lhsValue == nullptr || rhsValue == nullptr) {
        throw std::logic_error("Unexpected expression");
    }
    if (lhsValue->getType()->isPointerTy() || rhsValue->getType()->isPointerTy()) {
        throw std::logic_error("Unsupported operation");
    }
    const auto category = getOperationCategory(node->binOp);
    if (category == OperationCategory::Comparison) {
        const auto type = IRTypeFactory::from(node->lhs->getType(), mc.module->getContext());
        return IRValue::createValue(type->createBinaryOp(*mc.builder, node->binOp, lhsValue, rhsValue, "binOp"), type);
    }
    if (category == OperationCategory::Arithmetic) {
        const auto resultTypeNode = IRTypeFactory::from(node->getType(), mc.module->getContext());
        return IRValue::createValue(
                resultTypeNode->createBinaryOp(*mc.builder, node->binOp, lhsValue, rhsValue, "binOp"),
                resultTypeNode);
    }
    throw std::logic_error("Unsupported operation");
}
