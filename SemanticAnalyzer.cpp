//
// Created by vadim on 16.04.25.
//

#include "SemanticAnalyzer.h"

#include <cassert>
#include <iostream>

#include "ast/AssignmentNode.h"
#include "ast/BinOpNode.h"
#include "ast/BlockNode.h"
#include "ast/DeclarationNode.h"
#include "ast/FieldAccessNode.h"
#include "ast/FunctionNode.h"
#include "ast/IfStatement.h"
#include "ast/LoopCondNode.h"
#include "ast/MethodCallNode.h"
#include "ast/ModuleNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/ReturnNode.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/TypeCastNode.h"
#include "ast/UnaryOpNode.h"
#include "type/FunctionType.h"
#include "type/TypeFactory.h"

namespace {
    class SemanticError final : public std::runtime_error {
    public:
        explicit SemanticError(const std::string &msg):
            std::runtime_error(msg) {}
    };

    ExprNodePtr castIfNeeded(TypePtr targetType, ExprNodePtr expr) {
        const auto exprType = expr->getType();
        if (*exprType == *targetType) {
            return expr;
        }
        if (exprType->canCastTo(targetType, Type::CastMode::Implicit)) {
            return std::make_unique<TypeCastNode>(std::move(expr), std::move(targetType));
        }
        throw SemanticError("Cannot convert " + exprType->getName() + " to " + targetType->getName());
    }
} // namespace

void SemanticAnalyzer::visit(IdentNode *node) {
    if (const auto &si = symbolTable.lookup(node->name)) {
        node->setType(si.value()->type);
    } else if (const auto gi = symbolTable.lookupGlobal(node->name)) {
        node->setType(gi.value()->type);
    } else {
        throw SemanticError("Symbol " + node->name + " does not exist");
    }
}

void SemanticAnalyzer::visit(NumberNode *node) {}
void SemanticAnalyzer::visit(StringNode *node) {}
void SemanticAnalyzer::visit(BooleanNode *node) {}

void SemanticAnalyzer::visit(BinOpNode *node) {
    node->lhs->visit(this);
    node->rhs->visit(this);

    const auto category = getOperationCategory(node->binOp);
    TypePtr resultType;
    if (category == OperationCategory::Arithmetic) {
        auto commonType = node->lhs->getType()->getCommonType(node->rhs->getType());
        if (commonType) {
            throw SemanticError(std::format("Type mismatch error: {}", commonType.error()));
        }
        node->lhs = castIfNeeded(commonType.value(), std::move(node->lhs));
        node->rhs = castIfNeeded(commonType.value(), std::move(node->rhs));
        resultType = *commonType;
    } else if (category == OperationCategory::Comparison) {
        auto commonType = node->lhs->getType()->getComparableType(node->rhs->getType());
        if (!commonType) {
            throw SemanticError(std::format("Types are not comparable: {}", commonType.error()));
        }
        node->lhs = castIfNeeded(commonType.value(), std::move(node->lhs));
        node->rhs = castIfNeeded(commonType.value(), std::move(node->rhs));
        resultType = TypeFactory::makePrimitiveType(TypeKind::Boolean);
    } else if (category == OperationCategory::Logical) {
        if (!node->lhs->getType()->isBoolean() || !node->rhs->getType()->isBoolean()) {
            throw SemanticError("Logical operations require boolean operands");
        }
        resultType = TypeFactory::makePrimitiveType(TypeKind::Boolean);
    } else {
        throw SemanticError("Unsupported binary operation");
    }
    node->setType(resultType);
}

void SemanticAnalyzer::visit(FunctionNode *node) {
    node->proto->visit(this);
    symbolTable.enterScope();
    currentFunction = node->proto;

    for (const auto &param: node->proto->params) {
        symbolTable.insertGlobal(param->ident->name,
                                 std::make_shared<SymbolInfo>(param->type));
    }
    for (const auto &stmt: node->body->statements) {
        stmt->visit(this);
    }

    symbolTable.exitScope();
    currentFunction = nullptr;
}

void SemanticAnalyzer::visit(ProtoFunctionStatement *node) {
    std::vector<TypePtr> params;
    for (const auto &param: node->params) {
        params.push_back(param->type);
    }
    symbolTable.insertGlobal(node->name,
                             std::make_shared<SymbolInfo>(
                                     TypeFactory::makeFunction(node->returnType, params)));
}

void SemanticAnalyzer::visit(AssignmentNode *node) {
    node->lvalue->visit(this);
    node->rvalue->visit(this);
    node->rvalue = castIfNeeded(node->lvalue->getType(), std::move(node->rvalue));
}

void SemanticAnalyzer::visit(FunctionCallNode *node) {
    const auto optProto = symbolTable.lookupGlobal(node->ident->name);
    if (!optProto.has_value()) {
        throw SemanticError("Function not defined");
    }
    const auto functionType = std::dynamic_pointer_cast<const FunctionType>(optProto.value()->type);
    for (size_t i = 0; i < node->args.size(); ++i) {
        node->args[i]->visit(this);
        node->args[i] = castIfNeeded(functionType->parametersType()[i], std::move(node->args[i]));
    }
    node->setType(functionType->returnType());
}

void SemanticAnalyzer::visit(IfStatement *node) {
    node->ifBranch.cond->visit(this);
    const auto boolType = TypeFactory::makePrimitiveType(TypeKind::Boolean);
    if (const auto res = node->ifBranch.cond->getType()->canCastTo(boolType,
                                                                   Type::CastMode::Implicit);
        !res) {
        throw SemanticError(std::format("If condition type mismatch error: {}", res.error()));
    }
    node->ifBranch.cond = castIfNeeded(boolType, std::move(node->ifBranch.cond));
    node->ifBranch.then->visit(this);
}

void SemanticAnalyzer::visit(UnaryOpNode *node) {
    node->expr->visit(this);
    if (const auto type = node->getType()->getResultTypeUnary(node->operatorType); !type) {
        throw SemanticError("Unary operation not supported");
    }
}

void SemanticAnalyzer::visit(LoopCondNode *node) {
    if (node->init.has_value()) {
        node->init.value()->visit(this);
    }
}

void SemanticAnalyzer::visit(BlockNode *node) {
    symbolTable.enterScope();
    for (const auto &stmt: node->statements) {
        stmt->visit(this);
    }
    symbolTable.exitScope();
}

void SemanticAnalyzer::visit(DeclarationNode *node) {
    if (node->isGlobal) {
        symbolTable.insertGlobal(node->ident->name, std::make_shared<SymbolInfo>(node->type));
    } else {
        symbolTable.insert(node->ident->name, std::make_shared<SymbolInfo>(node->type));
    }
    if (node->init) {
        node->init.value()->visit(this);
        node->init = castIfNeeded(node->type, std::move(node->init.value()));
    }
}

void SemanticAnalyzer::visit(ReturnNode *node) {
    node->expr->visit(this);
    if (currentFunction == nullptr) {
        throw SemanticError("Return statement outside of a function");
    }
    const auto functionRetType = currentFunction->returnType;
    if (const auto res = node->expr->getType()->canCastTo(functionRetType, Type::CastMode::Implicit); !res) {
        throw SemanticError(std::format("Mismatch between return value and function types: {}", res.error()));
    }
    node->expr = castIfNeeded(functionRetType, std::move(node->expr));
}

void SemanticAnalyzer::visit(TernaryOperatorNode *node) {
    node->cond->visit(this);
    node->trueExpr->visit(this);
    node->falseExpr->visit(this);
}

void SemanticAnalyzer::visit(MethodCallNode *node) {
    node->object->visit(this);
    node->method->visit(this);
}

void SemanticAnalyzer::visit(FieldAccessNode *node) {
    node->object->visit(this);
    // TODO: check method in type
}

void SemanticAnalyzer::visit(CommentNode *node) {}

void SemanticAnalyzer::visit(ModuleNode *node) {
    for (const auto &statement: node->statements) {
        statement->visit(this);
    }
}

void SemanticAnalyzer::visit(TypeCastNode *node) {
    node->expr->visit(this);
}
