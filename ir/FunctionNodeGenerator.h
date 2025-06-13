#ifndef FUNCTIONGENERATOR_H
#define FUNCTIONGENERATOR_H

#include "ir/IRGenerator.h"

class FunctionNodeGenerator final : public IRGenerator {
public:
    ~FunctionNodeGenerator() override = default;

    IRValueOpt generate(BaseNode *node, ModuleContext &mc) const override;
};

#endif
