//
// Created by vadim on 04.05.25.
//

#include "BuiltinSymbols.h"
#include "type/TypeFactory.h"

BuiltinSymbols::BuiltinSymbols() {
    const auto printType = TypeFactory::makeFunction(TypeFactory::makePrimitiveType(TypeKind::Void),
                                                     std::vector{TypeFactory::makePrimitiveType(TypeKind::Str)},
                                                     true);
    builtinFunctions["println"].push_back(std::make_shared<SymbolInfo>(printType));
    builtinFunctions["print"].push_back(std::make_shared<SymbolInfo>(printType));
}

const std::unordered_map<std::string, std::vector<SymbolInfoPtr>> &BuiltinSymbols::getFunctions() const {
    return builtinFunctions;
}

const BuiltinSymbols &BuiltinSymbols::getInstance() {
    static BuiltinSymbols builtinSymbols;
    return builtinSymbols;
}
