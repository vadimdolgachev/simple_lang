//
// Created by vadim on 23.03.25.
//

#ifndef IRTYPEFACTORY_H
#define IRTYPEFACTORY_H


#include "../type/Type.h"
#include "ir/IRType.h"

class IRTypeFactory final {
public:
    static std::shared_ptr<IRType> from(const TypePtr &type, llvm::LLVMContext &context);
};

#endif //IRTYPEFACTORY_H
