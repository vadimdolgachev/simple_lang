#ifndef FUNCTIONGENERATOR_H
#define FUNCTIONGENERATOR_H

#include "ir/IRGenerator.h"

class FunctionNodeGenerator final : public IRGeneratorT<FunctionNode> {
public:
    ~FunctionNodeGenerator() override = default;

    IRValueOpt generateT(FunctionNode *node, ModuleContext &mc) const override;
};

#endif
