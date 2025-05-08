//
// Created by vadim on 23.03.25.
//

#ifndef IRTYPEFACTORY_H
#define IRTYPEFACTORY_H

#include "ModuleContext.h"
#include "StrIRType.h"

#include "ast/BaseNode.h"
#include "ast/AssignmentNode.h"
#include "../type/Type.h"

class IRTypeFactory final {
public:
    static std::shared_ptr<IRType> from(const TypePtr &type);
};

#endif //IRTYPEFACTORY_H
