#include "FunctionNodeGenerator.h"
#include "ast/FunctionNode.h"
#include "ast/ProtoFunctionStatement.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>

#include "./LLVMCodegen.h"

IRValueOpt FunctionNodeGenerator::generate(BaseNode *node, ModuleContext &mc) const {
    const auto *const functionNode = dynamic_cast<FunctionNode *>(node);
    auto *const func = getModuleFunction(functionNode->proto->name, mc);
    if (!func) {
        throw std::logic_error("Function prototype generation failed for: " + functionNode->proto->name);
    }
    auto *const basicBlock = llvm::BasicBlock::Create(mc.module->getContext(),
                                                      "entry",
                                                      func);

    generateBasicBlock(basicBlock,
                       functionNode->body->statements,
                       mc,
                       [&]() {
                           processFunctionParameters(func, basicBlock, functionNode, mc);
                       });

    if (functionNode->proto->returnType->isVoid()) {
        mc.builder->CreateRetVoid();
    }

    std::string verifyError;
    llvm::raw_string_ostream os(verifyError);
    if (verifyFunction(*func, &os)) {
        throw std::logic_error("Function verification failed:\n" + os.str());
    }
    return std::nullopt;
}
