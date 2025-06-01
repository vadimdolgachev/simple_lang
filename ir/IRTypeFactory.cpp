//
// Created by vadim on 09.04.25.
//

#include "IRTypeFactory.h"

#include "type/BooleanIRType.h"
#include "type/DoubleIRType.h"
#include "type/IntIRType.h"
#include "type/StrIRType.h"
#include "type/VoidIRType.h"
#include "type/ArrayIRType.h"
#include "../type/Type.h"
#include "../type/ArrayType.h"

std::shared_ptr<IRType> IRTypeFactory::from(const TypePtr &type, llvm::LLVMContext &context) {
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
    if (const auto arrayType = std::dynamic_pointer_cast<const ArrayType>(type)) {
        return std::make_shared<ArrayIRType>(from(arrayType->getElementType(), context)->getLLVMType(context),
                                             arrayType->size());
    }

    throw std::logic_error("Unknown type: " + type->getName());
}
