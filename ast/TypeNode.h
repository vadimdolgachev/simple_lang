//
// Created by vadim on 04.03.25.
//

#ifndef TYPENODE_H
#define TYPENODE_H

#include <cstdint>

enum class PrimitiveTypeKind : std::uint8_t {
    Boolean,
    Byte,
    Char,
    Double,
    Integer,
    Void,
    Str
};

class PrimitiveType final {
public:
    PrimitiveType(PrimitiveTypeKind fundamentalType, bool isPointer);

    PrimitiveTypeKind type;
    bool isPointer;
};

#endif //TYPENODE_H
