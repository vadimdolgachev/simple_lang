#ifndef FUNCTIONGENERATOR_H
#define FUNCTIONGENERATOR_H

#include "ir/IRGenerator.h"
#include "ast/FunctionNode.h"

class FunctionNodeGenerator final : public IRGeneratorT<FunctionNode> {
public:
    ~FunctionNodeGenerator() override = default;

    void generateT(FunctionNode *node, ModuleContext &mc) const override;
};

#endif
