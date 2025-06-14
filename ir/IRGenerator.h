//
// Created by vadim on 01.06.2025.
//

#ifndef IRGENERATOR_H
#define IRGENERATOR_H

#include "ast/BaseNode.h"
#include "ir/IRValue.h"
#include "ModuleContext.h"

class IRGenerator {
public:
    virtual ~IRGenerator() = default;

    virtual IRValueOpt generate(BaseNode *node, ModuleContext &mc) const = 0;
};

template<typename NodeT>
class IRGeneratorT : public IRGenerator {
public:
    virtual IRValueOpt generateT(NodeT *node, ModuleContext &mc) const = 0;

private:
    IRValueOpt generate(BaseNode *const node, ModuleContext &mc) const override {
        if (auto *const casted = dynamic_cast<NodeT *>(node)) {
            return generateT(casted, mc);
        }
        throw std::runtime_error("Unexpected node type: " + std::string(typeid(NodeT).name()));
    }
};

#endif //IRGENERATOR_H
