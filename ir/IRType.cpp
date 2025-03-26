//
// Created by vadim on 19.03.25.
//

#include "IRType.h"

IRType::IRType(const bool isPointer) :
    isPointer(isPointer) {}

void IRType::registerCustomOperation(const TokenType op, llvm::Function *function) {}
