//
// Created by vadim on 14.06.2025.
//

#ifndef BLOCKNODEGENERATOR_H
#define BLOCKNODEGENERATOR_H

#include "IRGenerator.h"

class BlockNodeGenerator final : public IRGeneratorT<BlockNode> {
public:
    IRValueOpt generateT(BlockNode *node, ModuleContext &mc) const override;
};

#endif //BLOCKNODEGENERATOR_H