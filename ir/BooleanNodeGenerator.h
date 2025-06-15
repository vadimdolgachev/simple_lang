//
// Created by vadim on 15.06.2025.
//

#ifndef BOOLEANNODEGENERATOR_H
#define BOOLEANNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/BooleanNode.h"

class BooleanNodeGenerator final : public IRGeneratorT<BooleanNode> {
public:
    IRValueOpt generateT(BooleanNode *node, ModuleContext &mc) const override;
};

#endif //BOOLEANNODEGENERATOR_H