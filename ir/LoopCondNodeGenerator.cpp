//
// Created by vadim on 13.06.2025.
//

#include "LoopCondNodeGenerator.h"

#include "LLVMCodegen.h"
#include "ast/LoopCondNode.h"

void LoopCondNodeGenerator::generateT(LoopCondNode *node, ModuleContext &mc) const {
    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();
    llvm::BasicBlock *condBB = nullptr;
    auto *const loopBB = llvm::BasicBlock::Create(mc.module->getContext(), "loop", parentFunc);
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "merge");

    if (node->loopType == LoopCondNode::Type::For) {
        if (node->init) {
            LLVMCodegen::generate(node->init->get(), mc);
        }
        condBB = llvm::BasicBlock::Create(mc.module->getContext(), "for.cond", parentFunc);
        mc.builder->CreateBr(condBB);
    }

    switch (node->loopType) {
        case LoopCondNode::Type::For: {
            mc.builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(mc.builder,
                                            LLVMCodegen::generate(node->condBranch.cond.get(), mc).value().getRawValue(),
                                            mc.builder->getInt1Ty());
            mc.builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::While: {
            condBB = llvm::BasicBlock::Create(mc.module->getContext(), "while.cond", parentFunc);
            mc.builder->CreateBr(condBB);
            mc.builder->SetInsertPoint(condBB);
            auto *const cond = tryCastValue(mc.builder,
                                            LLVMCodegen::generate(node->condBranch.cond.get(), mc).value().getRawValue(),
                                            mc.builder->getInt1Ty());
            mc.builder->CreateCondBr(cond, loopBB, mergeBB);
            break;
        }
        case LoopCondNode::Type::DoWhile: {
            mc.builder->CreateBr(loopBB);
            break;
        }
    }

    mc.builder->SetInsertPoint(loopBB);
    LLVMCodegen::generate(node->condBranch.then.get(), mc);

    switch (node->loopType) {
        case LoopCondNode::Type::For: {
            if (node->increment) {
                LLVMCodegen::generate(node->increment->get(), mc);
            }
            mc.builder->CreateBr(condBB);
            break;
        }
        case LoopCondNode::Type::While: {
            mc.builder->CreateBr(condBB);
            break;
        }
        case LoopCondNode::Type::DoWhile: {
            const auto cond = LLVMCodegen::generate(node->condBranch.cond.get(), mc);
            mc.builder->CreateCondBr(cond.value().getRawValue(), loopBB, mergeBB);
            break;
        }
    }

    if (!loopBB->getTerminator()) {
        mc.builder->CreateBr(mergeBB);
    }

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);
}
