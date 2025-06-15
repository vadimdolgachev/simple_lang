//
// Created by vadim on 13.06.2025.
//

#ifndef DECLARATIONNODEGENERATOR_H
#define DECLARATIONNODEGENERATOR_H

#include "IRGenerator.h"

#include "ast/DeclarationNode.h"

class DeclarationNodeGenerator final : public IRGeneratorT<DeclarationNode> {
public:
    void generateT(DeclarationNode *node, ModuleContext &mc) const override;
};

#endif //DECLARATIONNODEGENERATOR_H
