//
// Created by vadim on 17.08.2025.
//

#ifndef FIELDACCESSNODEGENERATOR_H
#define FIELDACCESSNODEGENERATOR_H

#include "IRGenerator.h"
#include "../ast/FieldAccessNode.h"

class FieldAccessNodeGenerator final : public IRGeneratorT<FieldAccessNode> {
public:
    IRValueOpt generateT(FieldAccessNode *node, ModuleContext &mc) const override;
};


#endif //FIELDACCESSNODEGENERATOR_H