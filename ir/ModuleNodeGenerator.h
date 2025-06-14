//
// Created by vadim on 14.06.2025.
//

#ifndef MODULENODEGENERATOR_H
#define MODULENODEGENERATOR_H

#include "IRGenerator.h"

class ModuleNodeGenerator final : public IRGeneratorT<ModuleNode> {
public:
    IRValueOpt generateT(ModuleNode *node, ModuleContext &mc) const override;
};

#endif //MODULENODEGENERATOR_H