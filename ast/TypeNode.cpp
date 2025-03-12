//
// Created by vadim on 04.03.25.
//

#include "TypeNode.h"

PrimitiveType::PrimitiveType(const PrimitiveTypeKind fundamentalType,
                             const bool isPointer):
    type(fundamentalType),
    isPointer(isPointer) {}
