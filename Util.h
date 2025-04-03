//
// Created by vadim on 15.12.24.
//

#ifndef UTIL_H
#define UTIL_H

#include <memory>
#include <vector>

template<typename>
struct deduce_arg_type;

template<typename Return, typename X, typename ArgType>
struct deduce_arg_type<Return(X::*)(ArgType) const> {
    using type = ArgType;
};

template<typename Function>
using HandlerType = typename deduce_arg_type<decltype(&Function::operator())>::type;

template<typename Base, typename Del, typename... Fs>
void visit(std::unique_ptr<Base, Del> ptr, Fs &&... fs) {
    const auto attempt = [&]<typename FType>(FType &&f) {
        if (auto *const rawPtr = dynamic_cast<typename HandlerType<FType>::pointer>(ptr.get())) {
            std::forward<FType>(f)(
                    std::unique_ptr<typename HandlerType<FType>::element_type>(
                            static_cast<typename HandlerType<FType>::pointer>(ptr.release()))
                    );
        }
    };
    (attempt(std::forward<Fs>(fs)), ...);
}

template<typename To, typename From>
std::tuple<std::unique_ptr<To>, std::unique_ptr<From>> tryCast(std::unique_ptr<From> fromPtr) {
    static_assert(std::is_same_v<std::default_delete<From>, std::decay_t<decltype(fromPtr.get_deleter())>>,
                  "From type must have default deleter");
    std::tuple<std::unique_ptr<To>, std::unique_ptr<From>> res;
    visit(std::move(fromPtr), [&res](std::unique_ptr<To> node) {
              res = {std::move(node), nullptr};
          }, [&res](std::unique_ptr<From> node) {
              res = {nullptr, std::move(node)};
          });
    return res;
}

template<typename DerivedType, typename BaseType>
std::unique_ptr<DerivedType> dynCast(std::unique_ptr<BaseType> &&basePtr) {
    if (auto *derived = dynamic_cast<DerivedType *>(basePtr.get())) {
        basePtr.release();
        return std::unique_ptr<DerivedType>(derived);
    }
    return nullptr;
}

template<class T>
std::vector<std::unique_ptr<T>> makeVectorUnique() {
    return {};
}

template<class T, class... Args>
std::vector<std::unique_ptr<T>> makeVector(Args &&... args) {
    std::vector<std::unique_ptr<T>> vector;
    (vector.emplace_back(std::forward<Args>(args)), ...);
    return vector;
}

template<typename T>
std::vector<std::unique_ptr<T>> clone(const std::vector<std::unique_ptr<T>> &source) {
    std::vector<std::unique_ptr<T>> dest;
    dest.reserve(source.size());
    for (const auto &item: source) {
        dest.emplace_back(dynCast<T>(item->clone()));
    }
    return dest;
}

#endif //UTIL_H
