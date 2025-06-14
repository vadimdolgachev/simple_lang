//
// Created by vadim on 13.06.2025.
//

#ifndef UNARYOPNODEGENERATOR_H
#define UNARYOPNODEGENERATOR_H

#include "IRGenerator.h"

class UnaryOpNodeGenerator final : public IRGeneratorT<UnaryOpNode> {
public:
    IRValueOpt generateT(UnaryOpNode *node, ModuleContext &mc) const override;
};

#endif //UNARYOPNODEGENERATOR_H