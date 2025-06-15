//
// Created by vadim on 13.06.2025.
//

#ifndef FUNCTIONCALLNODEGENERATOR_H
#define FUNCTIONCALLNODEGENERATOR_H

#include "ir/IRGenerator.h"
#include "ast/FunctionCallNode.h"

class FunctionCallNodeGenerator final : public IRGeneratorT<FunctionCallNode> {
public:
    IRValueOpt generateT(FunctionCallNode *node, ModuleContext &mc) const override;
};

#endif //FUNCTIONCALLNODEGENERATOR_H