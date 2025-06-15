#include "FunctionNodeGenerator.h"
#include "ast/FunctionNode.h"
#include "ast/ProtoFunctionStatement.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include "./LLVMCodegen.h"
#include "IRTypeFactory.h"

namespace {
    void processFunctionParameters(llvm::Function *func,
                                llvm::BasicBlock *basicBlock,
                                const FunctionNode *node,
                                ModuleContext &mc) {
        mc.builder->SetInsertPoint(basicBlock);

        for (auto &arg: func->args()) {
            const auto &paramType = node->proto->params[arg.getArgNo()]->type;
            auto *const alloca = mc.builder->CreateAlloca(
                    IRTypeFactory::from(paramType, mc.module->getContext())->getLLVMType(mc.module->getContext()), nullptr,
                    arg.getName());

            mc.builder->CreateStore(&arg, alloca);
            mc.symTable.insert(std::string(arg.getName()), std::make_shared<AllocaInstSymbolInfo>(paramType, alloca));
        }
    }
}

void FunctionNodeGenerator::generateT(FunctionNode *node, ModuleContext &mc) const {
    auto *const func = getModuleFunction(node->proto->name, mc);
    if (!func) {
        throw std::logic_error("Function prototype generation failed for: " + node->proto->name);
    }
    auto *const basicBlock = llvm::BasicBlock::Create(mc.module->getContext(),
                                                      "entry",
                                                      func);

    generateBasicBlock(basicBlock,
                       node->body->statements,
                       mc,
                       [&]() {
                           processFunctionParameters(func, basicBlock, node, mc);
                       });

    if (node->proto->returnType->isVoid()) {
        mc.builder->CreateRetVoid();
    }

    std::string verifyError;
    llvm::raw_string_ostream os(verifyError);
    if (verifyFunction(*func, &os)) {
        throw std::logic_error("Function verification failed:\n" + os.str());
    }
}
