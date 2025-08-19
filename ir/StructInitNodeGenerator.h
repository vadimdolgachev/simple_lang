//
// Created by vadim on 09.08.2025.
//

#ifndef STRUCTINITNODEGENERATOR_H
#define STRUCTINITNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/StructInitNode.h"

class StructInitNodeGenerator final : public IRGeneratorT<StructInitNode> {
public:
    IRValueOpt generateT(StructInitNode *node, ModuleContext &mc) const override;
};

#endif //STRUCTINITNODEGENERATOR_H