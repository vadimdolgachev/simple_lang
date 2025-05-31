//
// Created by vadim on 26.05.2025.
//

#ifndef IRVALUE_H
#define IRVALUE_H

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>

class IRType;

class IRValue final {
public:
    explicit IRValue(llvm::Value *value,
                     std::shared_ptr<IRType> type,
                     std::string name = "");

    IRValue(const IRValue &other) = default;
    IRValue &operator=(const IRValue &other) = default;

    [[nodiscard]] llvm::Value *getRawValue() const;

    [[nodiscard]] llvm::Value *createLoad(llvm::IRBuilder<> &builder) const;

    void createStore(llvm::IRBuilder<> &builder, llvm::Value *ptr) const;

    static IRValue create(llvm::Value *value,
                          std::shared_ptr<IRType> type,
                          const std::string &name = "");

private:
    llvm::Value *value;
    std::shared_ptr<IRType> type;
    std::string name;
};

#endif //IRVALUE_H
