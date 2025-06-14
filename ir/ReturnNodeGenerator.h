//
// Created by vadim on 14.06.2025.
//

#ifndef RETURNNODEGENERATOR_H
#define RETURNNODEGENERATOR_H

#include "IRGenerator.h"

class ReturnNodeGenerator final : public IRGeneratorT<ReturnNode> {
public:
    IRValueOpt generateT(ReturnNode *node, ModuleContext &mc) const override;
};

#endif //RETURNNODEGENERATOR_H