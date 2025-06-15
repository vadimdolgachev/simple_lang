//
// Created by vadim on 14.06.2025.
//

#ifndef TYPECASTNODEGENERATOR_H
#define TYPECASTNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/TypeCastNode.h"
class TypeCastNodeGenerator final : public IRGeneratorT<TypeCastNode> {
public:
    IRValueOpt generateT(TypeCastNode *node, ModuleContext &mc) const override;
};

#endif //TYPECASTNODEGENERATOR_H