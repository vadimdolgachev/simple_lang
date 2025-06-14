//
// Created by vadim on 01.06.2025.
//

#ifndef IDENTNODEGENERATOR_H
#define IDENTNODEGENERATOR_H

#include "IRGenerator.h"

class IdentNodeGenerator final : public IRGeneratorT<IdentNode> {
public:
    ~IdentNodeGenerator() override = default;

    IRValueOpt generateT(IdentNode *node, ModuleContext &mc) const override;
};

#endif //IDENTNODEGENERATOR_H