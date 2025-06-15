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

template<typename NodeT, typename Enable = void>
class IRGeneratorT;

template<typename NodeT>
class IRGeneratorT<NodeT, std::enable_if_t<std::is_base_of_v<StatementNode, NodeT>>> : public IRGenerator {
public:
    virtual void generateT(NodeT *node, ModuleContext &mc) const = 0;

private:
    IRValueOpt generate(BaseNode *node, ModuleContext &mc) const override {
        generateT(static_cast<NodeT *>(node), mc);
        return std::nullopt;
    }
};

template<typename NodeT>
class IRGeneratorT<NodeT, std::enable_if_t<std::is_base_of_v<ExpressionNode, NodeT>>> : public IRGenerator {
public:
    virtual IRValueOpt generateT(NodeT *node, ModuleContext &mc) const = 0;

private:
    IRValueOpt generate(BaseNode *node, ModuleContext &mc) const override {
        return generateT(static_cast<NodeT *>(node), mc);
    }
};

#endif //IRGENERATOR_H
