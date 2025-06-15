//
// Created by vadim on 14.06.2025.
//

#ifndef MODULENODEGENERATOR_H
#define MODULENODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/ModuleNode.h"

class ModuleNodeGenerator final : public IRGeneratorT<ModuleNode> {
public:
    void generateT(ModuleNode *node, ModuleContext &mc) const override;
};

#endif //MODULENODEGENERATOR_H