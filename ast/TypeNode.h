//
// Created by vadim on 04.03.25.
//

#ifndef TYPENODE_H
#define TYPENODE_H

#include <cstdint>
#include <optional>
#include <string>

enum class TypeKind : std::uint8_t {
    Boolean,
    Byte,
    Char,
    Double,
    Integer,
    Void,
    Str,
    Custom
};

struct TypeNode final {
    TypeNode(TypeKind type,
             bool isPointer,
             std::optional<std::string> typeName);

    TypeKind kind;
    bool isPointer;
    std::optional<std::string> typeName;

    [[nodiscard]] bool isNumeric() const;

    [[nodiscard]] bool isVoid() const;

    [[nodiscard]] TypeNode dereference() const;

    bool operator==(const TypeNode &other) const;

    static TypeNode makePrimitive(TypeKind kind, bool isPtr = false);

    static TypeNode makeCustom(const std::string &name, bool isPtr = false);
};

#endif //TYPENODE_H
