//
// Created by vadim on 10.07.2025.
//

#include "DeclarationCollector.h"
#include "ast/ModuleNode.h"
#include "ast/StructDeclarationNode.h"
#include "type/StructType.h"

void DeclarationCollector::visit(StructDeclarationNode *node) {
    std::vector<StructField> fields;
    for (auto &member: node->members) {
        const auto visitor = FuncOverloads{
            [&fields](const NodePtr<DeclarationNode> &decl) {
                if (decl->type) {
                    fields.emplace_back(decl->ident->name, decl->type);
                } else {
                    throw std::runtime_error("Member type is null");
                }
            },
            []([[maybe_unused]] TypePtr &type) {
                throw std::runtime_error("Not implemented");
            }
        };
        std::visit(visitor, member);
    }
    declarations.emplace_back(std::make_unique<StructType>(node->name, StructKind::Named, std::move(fields)));
}

void DeclarationCollector::visit(ModuleNode *node) {
    for (const auto &statement: node->statements) {
        statement->visit(this);
    }
}

std::vector<TypePtr> DeclarationCollector::getTypeDeclarations() const {
    return declarations;
}
