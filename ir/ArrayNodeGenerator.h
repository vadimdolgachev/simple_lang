//
// Created by vadim on 14.06.2025.
//

#ifndef ARRAYNODEGENERATOR_H
#define ARRAYNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/ArrayNode.h"

class ArrayNodeGenerator final : public IRGeneratorT<ArrayNode> {
public:
    IRValueOpt generateT(ArrayNode *node, ModuleContext &mc) const override;
};

#endif //ARRAYNODEGENERATOR_H