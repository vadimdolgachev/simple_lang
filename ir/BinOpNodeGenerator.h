//
// Created by vadim on 13.06.2025.
//

#ifndef BINOPNODEGENERATOR_H
#define BINOPNODEGENERATOR_H

#include "IRGenerator.h"

class BinOpNodeGenerator final : public IRGeneratorT<BinOpNode> {
public:
    IRValueOpt generateT(BinOpNode *node, ModuleContext &mc) const override;
};

#endif //BINOPNODEGENERATOR_H