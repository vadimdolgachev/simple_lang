//
// Created by vadim on 20.08.2025.
//

#ifndef TYPEFWD_H
#define TYPEFWD_H

#include <memory>

class Type;
using TypePtr = std::shared_ptr<Type>;

struct CallableInfo;
using CallableInfoPtr = std::shared_ptr<CallableInfo>;

class FunctionType;
using FunctionTypePtr = std::shared_ptr<FunctionType>;

class StructType;
using StructTypePtr = std::shared_ptr<StructType>;

class ArrayType;
using ArrayTypePtr = std::shared_ptr<ArrayType>;

#endif //TYPEFWD_H