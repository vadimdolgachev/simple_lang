//
// Created by vadim on 10.07.2025.
//

#ifndef DECLARATIONCOLLECTOR_H
#define DECLARATIONCOLLECTOR_H

#include "ast/BaseNode.h"

class DeclarationCollector final : public BaseNodeVisitor {
public:
    [[nodiscard]] std::vector<TypePtr> getTypeDeclarations() const;

private:
    void visit(StructNode *node) override;
    void visit(ModuleNode *node) override;

    std::vector<TypePtr> declarations;
};

#endif //DECLARATIONCOLLECTOR_H