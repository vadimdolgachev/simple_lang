//
// Created by vadim on 13.06.2025.
//

#ifndef LOOPCONDNODEGENERATOR_H
#define LOOPCONDNODEGENERATOR_H

#include "IRGenerator.h"

class LoopCondNodeGenerator final : public IRGeneratorT<LoopCondNode> {
public:
    IRValueOpt generateT(LoopCondNode *node, ModuleContext &mc) const override;
};

#endif //LOOPCONDNODEGENERATOR_H