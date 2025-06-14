//
// Created by vadim on 13.06.2025.
//

#include "UnaryOpNodeGenerator.h"

#include "IRTypeFactory.h"
#include "LLVMCodegen.h"
#include "ast/UnaryOpNode.h"
#include "ast/IdentNode.h"

IRValueOpt UnaryOpNodeGenerator::generateT(UnaryOpNode *node, ModuleContext &mc) const {
    if (node->operatorType == TokenType::Increment
        || node->operatorType == TokenType::Decrement
        || node->operatorType == TokenType::Plus
        || node->operatorType == TokenType::Minus) {
        const auto irType = IRTypeFactory::from(node->getType(), mc.module->getContext());
        if (!isNode<IdentNode>(node->expr.get())
            && (node->operatorType == TokenType::Increment || node->operatorType ==
                TokenType::Decrement)) {
            throw std::logic_error("Increment/decrement requires lvalue variable");
        }
        const auto irValue = LLVMCodegen::generate(node->expr.get(), mc);

        return IRValue::createValue(irValue.value().getType()->createUnaryOp(*mc.builder,
                                                                             node->operatorType,
                                                                             irValue.value().createLoad(*mc.builder),
                                                                             irValue.value().getRawValue(),
                                                                             "incdec"),
                                    irValue.value().getType());
    }

    throw std::logic_error("Unsupported unary operator");
}
