//
// Created by vadim on 14.06.2025.
//

#ifndef TERNARYOPERATORNODEGENERATOR_H
#define TERNARYOPERATORNODEGENERATOR_H

#include "IRGenerator.h"

class TernaryOperatorNodeGenerator final : public IRGeneratorT<TernaryOperatorNode> {
public:
    IRValueOpt generateT(TernaryOperatorNode *node, ModuleContext &mc) const override;
};

#endif //TERNARYOPERATORNODEGENERATOR_H