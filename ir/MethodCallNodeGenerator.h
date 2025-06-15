//
// Created by vadim on 14.06.2025.
//

#ifndef METHODCALLNODEGENERATOR_H
#define METHODCALLNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/MethodCallNode.h"

class MethodCallNodeGenerator final : public IRGeneratorT<MethodCallNode> {
public:
    IRValueOpt generateT(MethodCallNode *node, ModuleContext &mc) const override;
};

#endif //METHODCALLNODEGENERATOR_H