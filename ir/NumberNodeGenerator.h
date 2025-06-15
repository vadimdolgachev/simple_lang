//
// Created by vadim on 15.06.2025.
//

#ifndef NUMBERNODEGENERATOR_H
#define NUMBERNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/NumberNode.h"

class NumberNodeGenerator final : public IRGeneratorT<NumberNode> {
public:
    IRValueOpt generateT(NumberNode *node, ModuleContext &mc) const override;
};

#endif //NUMBERNODEGENERATOR_H