//
// Created by vadim on 13.06.2025.
//

#ifndef IFSTATEMENTGENERATOR_H
#define IFSTATEMENTGENERATOR_H

#include "IRGenerator.h"

class IfStatementGenerator final : public IRGeneratorT<IfStatement> {
public:
    IRValueOpt generateT(IfStatement *node, ModuleContext &mc) const override;
};

#endif //IFSTATEMENTGENERATOR_H