//
// Created by vadim on 14.06.2025.
//

#include "TernaryOperatorNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/TernaryOperatorNode.h"

IRValueOpt TernaryOperatorNodeGenerator::generateT(TernaryOperatorNode *node, ModuleContext &mc) const {
    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();

    auto *const thenBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_then", parentFunc);
    auto *const elseBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_else");
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "tern_merge");

    mc.builder->CreateCondBr(LLVMCodegen::generate(node->cond.get(), mc).value().getRawValue(), thenBB, elseBB);

    mc.builder->SetInsertPoint(thenBB);
    const auto trueValueHandler = LLVMCodegen::generate(node->trueExpr.get(), mc);
    auto *const trueVal = trueValueHandler.value().createLoad(*mc.builder);
    mc.builder->CreateBr(mergeBB);

    elseBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(elseBB);
    auto falseValueHandler = LLVMCodegen::generate(node->falseExpr.get(), mc);
    auto *const falseVal = trueValueHandler.value().createLoad(*mc.builder);
    mc.builder->CreateBr(mergeBB);

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);

    if (trueVal->getType() != falseVal->getType()) {
        throw std::logic_error("Ternary expressions must be of the same type");
    }

    auto *const phi = mc.builder->CreatePHI(trueVal->getType(), 2, "tern_result");
    phi->addIncoming(trueVal, thenBB);
    phi->addIncoming(falseVal, elseBB);
    return IRValue::createValue(phi, IRTypeFactory::from(node->getType(), mc.module->getContext()));
}
