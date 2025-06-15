//
// Created by vadim on 15.06.2025.
//

#ifndef STRINGNODEGENERATOR_H
#define STRINGNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/StringNode.h"

class StringNodeGenerator final : public IRGeneratorT<StringNode> {
public:
    IRValueOpt generateT(StringNode *node, ModuleContext &mc) const override;
};

#endif //STRINGNODEGENERATOR_H