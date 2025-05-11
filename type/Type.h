//
// Created by vadim on 04.03.25.
//

#ifndef TYPE_H
#define TYPE_H

#include <memory>
#include <optional>
#include <vector>

#include "Lexer.h"
#include "Util.h"

enum class TypeKind : std::uint8_t {
    Boolean,
    Byte,
    Char,
    Double,
    Integer,
    Void,
    Str,
    Pointer,
    Function,
    Custom
};

class Type;
using TypePtr = std::shared_ptr<const Type>;

using OptType = std::optional<TypePtr>;

using ResultType = Result<TypePtr>;

using BoolResult = std::expected<bool, std::string>;

struct CallableInfo;
using CallableInfoPtr = std::shared_ptr<CallableInfo>;

class FunctionType;
using FunctionTypePtr = std::shared_ptr<const FunctionType>;

enum class OperationCategory: std::uint8_t {
    Arithmetic,
    Comparison,
    Logical,
    Other
};

OperationCategory getOperationCategory(TokenType op);

class Type : public std::enable_shared_from_this<Type> {
public:
    enum class CastMode: std::uint8_t {
        Implicit,
        Explicit
    };

    Type() = default;
    Type(const Type &) = delete;
    Type &operator=(const Type &) = delete;
    virtual ~Type() = default;

    virtual bool operator==(const Type &other) const = 0;

    [[nodiscard]] virtual BoolResult canCastTo(const TypePtr &target, CastMode mode) const;

    virtual ResultType getResultTypeUnary([[maybe_unused]] TokenType op) const;

    virtual ResultType getCommonType(const TypePtr &other) const;

    [[nodiscard]] virtual std::string getName() const = 0;

    [[nodiscard]] virtual TypeKind getKind() const noexcept = 0;

    [[nodiscard]] virtual std::vector<TypePtr> getFieldTypes() const;

    [[nodiscard]] virtual const std::vector<CallableInfoPtr> &getMethodTypes() const;

    [[nodiscard]] virtual TypePtr getElementType() const;

    virtual ResultType getComparableType(const TypePtr &type) const;

    bool isBoolean() const noexcept;

    bool isNumeric() const noexcept;

    bool isVoid() const noexcept;

    bool isDouble() const noexcept;

    bool isInteger() const noexcept;

    bool isStr() const noexcept;

    std::optional<FunctionTypePtr> asFunction() const;
};

class PrimitiveType : public Type {
public:
    explicit PrimitiveType(TypeKind kind, bool isConst = false);

    [[nodiscard]] TypeKind getKind() const noexcept override;

    bool operator==(const Type &other) const override;

    std::string getName() const override;

protected:
    const TypeKind kind;
    const bool isConst;
};

struct CallableInfo {
    std::string name;
    FunctionTypePtr type;

    virtual ~CallableInfo() = default;
};

struct FunctionInfo final : CallableInfo {
    bool isBuiltin = false;

    static std::shared_ptr<FunctionInfo> create(std::string name, FunctionTypePtr type, bool isBuiltin = false) {
        auto info = std::make_shared<FunctionInfo>();
        info->name = std::move(name);
        info->type = std::move(type);
        info->isBuiltin = isBuiltin;
        return info;
    }
};

struct MethodInfo final : CallableInfo {
    bool isStatic = false;

    static std::shared_ptr<MethodInfo> create(std::string name, FunctionTypePtr type, bool isStatic = false) {
        auto info = std::make_shared<MethodInfo>();
        info->name = std::move(name);
        info->type = std::move(type);
        info->isStatic = isStatic;
        return info;
    }
};
using MethodInfoPtr = std::shared_ptr<MethodInfo>;

#endif //TYPE_H
