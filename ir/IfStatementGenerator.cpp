//
// Created by vadim on 13.06.2025.
//

#include "IfStatementGenerator.h"

#include "LLVMCodegen.h"
#include "ast/IfStatement.h"

void IfStatementGenerator::generateT(IfStatement *node, ModuleContext &mc) const {
    auto *const firstCV = tryCastValue(mc.builder,
                                       LLVMCodegen::generate(node->ifBranch.cond.get(), mc).value().getRawValue(),
                                       mc.builder->getInt1Ty());
    if (!firstCV) {
        throw std::logic_error("Condition must be boolean type");
    }

    auto *const parentFunc = mc.builder->GetInsertBlock()->getParent();

    auto *const firstIfBB = llvm::BasicBlock::Create(mc.module->getContext(), "if", parentFunc);
    auto *lastElseBB = llvm::BasicBlock::Create(mc.module->getContext(), "else");
    auto *const mergeBB = llvm::BasicBlock::Create(mc.module->getContext(), "merge_if");

    mc.builder->CreateCondBr(firstCV, firstIfBB, lastElseBB);

    mc.builder->SetInsertPoint(firstIfBB);
    LLVMCodegen::generate(node->ifBranch.then.get(), mc);

    if (!mc.builder->GetInsertBlock()->getTerminator()) {
        mc.builder->CreateBr(mergeBB);
    }

    for (size_t i = 0; i < node->elseIfBranches.size(); ++i) {
        lastElseBB->insertInto(parentFunc);
        mc.builder->SetInsertPoint(lastElseBB);

        const auto &[cond, then] = node->elseIfBranches[i];
        auto *const value = tryCastValue(mc.builder,
                                         LLVMCodegen::generate(cond.get(), mc).value().getRawValue(),
                                         mc.builder->getInt1Ty());
        if (!value) {
            throw std::logic_error("Condition must be boolean type");
        }
        auto *const ifBB = llvm::BasicBlock::Create(mc.module->getContext(),
                                                    "elif_" + std::to_string(i), parentFunc);
        lastElseBB = llvm::BasicBlock::Create(mc.module->getContext(), "else_" + std::to_string(i));
        mc.builder->CreateCondBr(value, ifBB, lastElseBB);

        mc.builder->SetInsertPoint(ifBB);
        LLVMCodegen::generate(then.get(), mc);

        if (!mc.builder->GetInsertBlock()->getTerminator()) {
            mc.builder->CreateBr(mergeBB);
        }
    }

    lastElseBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(lastElseBB);
    if (node->elseBranch.has_value()) {
        LLVMCodegen::generate(node->elseBranch.value().get(), mc);
    }
    if (!mc.builder->GetInsertBlock()->getTerminator()) {
        mc.builder->CreateBr(mergeBB);
    }

    mergeBB->insertInto(parentFunc);
    mc.builder->SetInsertPoint(mergeBB);
}
