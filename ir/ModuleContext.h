//
// Created by vadim on 23.03.25.
//

#ifndef MODULECONTEXT_H
#define MODULECONTEXT_H

#include "../SymbolTable.h"

// template<typename SymbolType, typename BoundType>
// struct SymbolInfo final {
//     SymbolType type;
//     bool isConst = false;
//     BoundType bound = {};
// };


struct ModuleContext final {
    ModuleContext() = default;

    ModuleContext(const ModuleContext &) = delete;
    ModuleContext &operator=(ModuleContext &) = delete;

    SymbolTable symTable;
};

#endif //MODULECONTEXT_H
