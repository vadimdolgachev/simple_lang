//
// Created by vadim on 13.06.2025.
//

#ifndef PROTOFUNCTIONGENERATOR_H
#define PROTOFUNCTIONGENERATOR_H

#include "IRGenerator.h"

class ProtoFunctionGenerator final : public IRGeneratorT<ProtoFunctionStatement> {
public:
    IRValueOpt generateT(ProtoFunctionStatement *node, ModuleContext &mc) const override;
};

#endif //PROTOFUNCTIONGENERATOR_H
