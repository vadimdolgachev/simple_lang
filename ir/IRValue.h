//
// Created by vadim on 26.05.2025.
//

#ifndef IRVALUE_H
#define IRVALUE_H

#include <llvm/IR/IRBuilder.h>

class IRType;

class IRValue final {
public:
    enum class ValueKind: std::uint8_t {
        Local,
        Global,
        Value
    };

    IRValue(llvm::Value *value,
            std::shared_ptr<IRType> type,
            ValueKind valueKind,
            std::string name = "");

    IRValue(const IRValue &other) = default;
    IRValue &operator=(const IRValue &other) = default;

    [[nodiscard]] llvm::Value *getRawValue() const;
    [[nodiscard]] std::shared_ptr<IRType> getType() const;
    [[nodiscard]] llvm::Value *createLoad(llvm::IRBuilder<> &builder) const;
    void createStore(llvm::IRBuilder<> &builder, llvm::Value *ptr) const;
    [[nodiscard]] ValueKind getKind() const;

    static IRValue createValue(llvm::Value *value,
                               std::shared_ptr<IRType> type,
                               const std::string &name = "");

    static IRValue createAlloca(llvm::Value *value,
                                std::shared_ptr<IRType> type,
                                const std::string &name = "");

    static IRValue createGlobal(llvm::Value *value,
                                std::shared_ptr<IRType> type,
                                const std::string &name = "");

private:
    llvm::Value *value;
    std::shared_ptr<IRType> type;
    ValueKind valueKind;
    std::string name;
};

using IRValueOpt = std::optional<IRValue>;

#endif //IRVALUE_H
