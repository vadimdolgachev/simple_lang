//
// Created by vadim on 13.06.2025.
//

#ifndef LOOPCONDNODEGENERATOR_H
#define LOOPCONDNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/LoopCondNode.h"
class LoopCondNodeGenerator final : public IRGeneratorT<LoopCondNode> {
public:
    void generateT(LoopCondNode *node, ModuleContext &mc) const override;
};

#endif //LOOPCONDNODEGENERATOR_H