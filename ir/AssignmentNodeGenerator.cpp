//
// Created by vadim on 13.06.2025.
//

#include "AssignmentNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/AssignmentNode.h"
#include "ast/IdentNode.h"

IRValueOpt AssignmentNodeGenerator::generateT(AssignmentNode *node, ModuleContext &mc) const {
    const auto init = LLVMCodegen::generate(node->rvalue.get(), mc);
    if (!init) {
        throw std::runtime_error("Error init value generation");
    }
    if (mc.builder->GetInsertBlock() != nullptr) {
        if (const auto var = mc.symTable.lookup(node->lvalue->name)) {
            if (const auto si = std::dynamic_pointer_cast<const AllocaInstSymbolInfo>(var.value())) {
                init.value().createStore(*mc.builder, si->inst);
                auto irType = IRTypeFactory::from(si->type, mc.module->getContext());
                return IRValue::createAlloca(si->inst, std::move(irType));
            }
        } else if (const auto gVar = mc.symTable.lookupGlobal(node->lvalue->name)) {
            if (const auto &sig = std::dynamic_pointer_cast<const GlobalSymbolInfo>(gVar.value())) {
                if (sig->var->isConstant()) {
                    throw std::logic_error("Variable: " + node->lvalue->name + " is constant");
                }
                init.value().createStore(*mc.builder, sig->var);
                auto irType = IRTypeFactory::from(sig->type, mc.module->getContext());
                return IRValue::createGlobal(sig->var, std::move(irType));
            }
        } else {
            throw std::logic_error("Undefined variable: " + node->lvalue->name);
        }
    }
    return std::nullopt;
}
