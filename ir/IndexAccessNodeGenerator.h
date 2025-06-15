//
// Created by vadim on 14.06.2025.
//

#ifndef INDEXACCESSNODEGENERATOR_H
#define INDEXACCESSNODEGENERATOR_H

#include "IRGenerator.h"
#include "ast/IndexAccessNode.h"

class IndexAccessNodeGenerator final : public IRGeneratorT<IndexAccessNode> {
public:
    IRValueOpt generateT(IndexAccessNode *node, ModuleContext &mc) const override;
};

#endif //INDEXACCESSNODEGENERATOR_H
