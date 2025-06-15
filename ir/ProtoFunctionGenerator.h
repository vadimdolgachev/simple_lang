//
// Created by vadim on 13.06.2025.
//

#ifndef PROTOFUNCTIONGENERATOR_H
#define PROTOFUNCTIONGENERATOR_H

#include "IRGenerator.h"
#include "ast/ProtoFunctionStatement.h"

class ProtoFunctionGenerator final : public IRGeneratorT<ProtoFunctionStatement> {
public:
    void generateT(ProtoFunctionStatement *node, ModuleContext &mc) const override;
};

#endif //PROTOFUNCTIONGENERATOR_H
