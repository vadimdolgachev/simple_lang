//
// Created by vadim on 13.06.2025.
//

#ifndef ASSIGNMENTNODEGENERATOR_H
#define ASSIGNMENTNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/AssignmentNode.h"

class AssignmentNodeGenerator final : public IRGeneratorT<AssignmentNode> {
public:
    IRValueOpt generateT(AssignmentNode *node, ModuleContext &mc) const override;
};

#endif //ASSIGNMENTNODEGENERATOR_H