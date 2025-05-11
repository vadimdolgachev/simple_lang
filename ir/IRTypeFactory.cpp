//
// Created by vadim on 09.04.25.
//

#include "IRTypeFactory.h"

#include "BooleanIRType.h"
#include "DoubleIRType.h"
#include "IntIRType.h"
#include "StrIRType.h"
#include "VoidIRType.h"

#include "../type/Type.h"

std::shared_ptr<IRType> IRTypeFactory::from(const TypePtr &type) {
    if (type->isStr()) {
        return std::make_shared<StrIRType>();
    }
    if (type->isVoid()) {
        return std::make_shared<VoidIRType>();
    }
    if (type->isDouble()) {
        return std::make_shared<DoubleIRType>();
    }
    if (type->isInteger()) {
        return std::make_shared<IntIRType>();
    }
    if (type->isBoolean()) {
        return std::make_shared<BooleanIRType>();
    }

    throw std::logic_error("Unknown type: " + type->getName());
}
