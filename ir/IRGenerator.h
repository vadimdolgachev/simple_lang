//
// Created by vadim on 01.06.2025.
//

#ifndef IRGENERATOR_H
#define IRGENERATOR_H

#include "ast/BaseNode.h"
#include "ir/IRValue.h"
#include "ModuleContext.h"

class IRGenerator {
public:
    virtual ~IRGenerator() = default;

    virtual IRValueOpt generate(BaseNode *node, ModuleContext &mc) const = 0;
};

#endif //IRGENERATOR_H
