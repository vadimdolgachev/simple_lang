//
// Created by vadim on 16.04.25.
//

#include "SemanticAnalyzer.h"

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
#include "ast/ArrayNode.h"
#include "ast/IndexAccessNode.h"
#include "ast/StructInitNode.h"
#include "type/FunctionType.h"
#include "type/TypeFactory.h"
#include "ast/NumberNode.h"

namespace {
    class SemanticError final : public std::runtime_error {
    public:
        explicit SemanticError(const std::string &msg) :
            std::runtime_error(msg) {
        }
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

SemanticAnalyzer::SemanticAnalyzer(SymbolTable symbolTable, std::vector<TypePtr> declarations) :
    symbolTable(std::move(symbolTable)),
    declarations(std::move(declarations)) {
}

void SemanticAnalyzer::visit(IdentNode *node) {
    if (const auto &si = symbolTable.lookup(node->name)) {
        node->setType(si.value()->type);
    } else {
        throw SemanticError("Symbol " + node->name + " does not exist");
    }
}

void SemanticAnalyzer::visit(NumberNode *node) {
}

void SemanticAnalyzer::visit(StringNode *node) {
}

void SemanticAnalyzer::visit(BooleanNode *node) {
}

void SemanticAnalyzer::visit(BinOpNode *node) {
    node->lhs->visit(this);
    node->rhs->visit(this);

    const auto category = getOperationCategory(node->binOp);
    TypePtr resultType;
    if (category == OperationCategory::Arithmetic) {
        auto commonType = node->lhs->getType()->getCommonType(node->rhs->getType());
        if (!commonType) {
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
    symbolTable.enterScope();
    node->proto->visit(this);
    currentFunction = node->proto;

    for (const auto &param: node->proto->params) {
        symbolTable.insertFunction(param->ident->name,
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
    params.reserve(node->params.size());
    for (const auto &param: node->params) {
        param->type = resolveTypeIfNeeded(param->type);
        params.push_back(param->type);
        symbolTable.insert(param->ident->name, std::make_shared<SymbolInfo>(param->type));
    }

    node->returnType = resolveTypeIfNeeded(node->returnType);
    symbolTable.insertFunction(node->name,
                               std::make_shared<SymbolInfo>(
                                       TypeFactory::makeFunction(node->returnType, params, node->isVarArgs)));
}

void SemanticAnalyzer::visit(AssignmentNode *node) {
    node->lvalue->visit(this);
    node->rvalue->visit(this);
    node->rvalue = castIfNeeded(node->lvalue->getType(), std::move(node->rvalue));
    if (*node->rvalue->getType() != *node->lvalue->getType()) {
        throw SemanticError("Type mismatch after conversion");
    }
    node->setType(node->lvalue->getType());
}

void SemanticAnalyzer::visit(FunctionCallNode *node) {
    const auto signatures = symbolTable.lookupFunction(node->ident->name);
    if (signatures.empty()) {
        throw SemanticError("Function not defined");
    }
    if (signatures.size() > 1) {
        throw SemanticError("Function overloads are not supported yet");
    }
    if (const auto functionType = signatures[0]->type->asFunction()) {
        const auto paramsType = functionType.value()->parametersType();
        for (size_t i = 0; i < node->args.size(); ++i) {
            node->args[i]->visit(this);
            if (i < paramsType.size()) {
                node->args[i] = castIfNeeded(paramsType[i], std::move(node->args[i]));
            } else if (!functionType.value()->isVariadic()) {
                throw SemanticError("Wrong number of arguments");
            }
        }
        node->setType(functionType.value()->returnType());
    }
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
    for (auto &[cond, then]: node->elseIfBranches) {
        cond->visit(this);
        cond = castIfNeeded(boolType, std::move(cond));
        then->visit(this);
    }
    if (node->elseBranch) {
        node->elseBranch.value()->visit(this);
    }
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
        node->condBranch.cond->visit(this);
        node->condBranch.cond = castIfNeeded(TypeFactory::makePrimitiveType(TypeKind::Boolean),
                                             std::move(node->condBranch.cond));
        if (node->increment) {
            node->increment.value()->visit(this);
        }
        node->condBranch.then->visit(this);
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
    node->type = resolveTypeIfNeeded(node->type);
    symbolTable.insert(node->ident->name, std::make_shared<SymbolInfo>(node->type));
    if (node->init) {
        node->init.value()->visit(this);
        node->init = castIfNeeded(node->type, std::move(node->init.value()));
    } else if (!node->isConst) {
        throw SemanticError("Const declaration must be initialized");
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
    if (!node->cond->getType()->isBoolean()) {
        throw SemanticError("Condition in ternary operator must be boolean, got: " +
                            node->cond->getType()->getName());
    }

    node->trueExpr->visit(this);
    node->falseExpr->visit(this);
    const auto trueType = node->trueExpr->getType();
    const auto falseType = node->falseExpr->getType();

    const auto commonType = trueType->getCommonType(falseType);
    if (!commonType) {
        throw SemanticError("Incompatible types in ternary operator: " +
                            trueType->getName() + " and " +
                            falseType->getName());
    }

    node->trueExpr = castIfNeeded(*commonType, std::move(node->trueExpr));
    node->falseExpr = castIfNeeded(*commonType, std::move(node->falseExpr));

    node->setType(*commonType);
}

void SemanticAnalyzer::visit(MethodCallNode *node) {
    node->object->visit(this);
    std::vector<TypePtr> signature;
    signature.reserve(node->method->args.size());
    for (const auto &arg: node->method->args) {
        arg->visit(this);
        signature.push_back(arg->getType());
    }
    if (const auto method = node->object->getType()->findMethod(node->method->ident->name, signature)) {
        node->setType((*method)->type);
    } else {
        throw SemanticError(
                "Method '" + node->object->getType()->getName() + ":" + node->method->ident->name + "' does not exist");
    }
}

void SemanticAnalyzer::visit(FieldAccessNode *node) {
    node->object->visit(this);
    if (const auto fieldType = node->getObjectType()->findFieldType(node->field->name)) {
        node->field->setType(*fieldType);
        node->setType(*fieldType);
    } else {
        throw SemanticError(std::format("Unknown field: {}:{}", node->object->getType()->asStruct().value()->getName(),
                                        node->field->name));
    }
}

void SemanticAnalyzer::visit(CommentNode *node) {
}

void SemanticAnalyzer::visit(ModuleNode *node) {
    symbolTable.enterScope();
    for (const auto &statement: node->statements) {
        statement->visit(this);
    }
    symbolTable.exitScope();
}

void SemanticAnalyzer::visit(TypeCastNode *node) {
    node->expr->visit(this);
}

void SemanticAnalyzer::visit(ArrayNode *node) {
    std::optional<TypePtr> elementType;
    for (const auto &element: node->elements) {
        element->visit(this);
        if (elementType && *element->getType() != *elementType.value()) {
            throw SemanticError("Array elements must be of the same type");
        }
        if (!elementType) {
            elementType = element->getType();
        }
    }
    node->setType(TypeFactory::makeArrayType(elementType.value(), node->elements.size()));
}

void SemanticAnalyzer::visit(IndexAccessNode *node) {
    node->object->visit(this);
    node->index->visit(this);

    if (const auto arrayType = node->object->getType()->asArray()) {
        if (!node->index->getType()->isInteger()) {
            throw SemanticError("Index type must be integer");
        }
        if (const auto numberNode = asNode<NumberNode>(node->index.get())) {
            if (const auto index = numberNode.value()->value;
                index < 0 || index >= arrayType.value()->size()) {
                throw SemanticError(std::format("Index value: {} is out of array bounds: {}", index, arrayType.value()->getName()));
            }
        }
        // TODO: node->index->getType() should it be unsigned int?
        node->setType(arrayType.value()->getElementType());
    } else {
        throw SemanticError("Index not supported for type: " + node->object->getType()->getName());
    }
}

void SemanticAnalyzer::visit(StructDeclarationNode *node) {
    // skip struct declaration
}

void SemanticAnalyzer::visit(StructInitNode *node) {
    if (const auto type = resolveTypeRef(node->ident)) {
        const auto structType = type.value()->asStruct().value();
        for (size_t i = 0; i < structType->getFieldSize(); ++i) {
            auto &[fieldName, fieldType] = structType->getFieldByIndex(i);
            fieldType = resolveTypeIfNeeded(std::move(fieldType));
        }
        node->setType(structType);
    } else {
        throw SemanticError(std::format("Type definition for '{}' not found", node->ident));
    }

    auto designatorIt = std::begin(node->designator);
    for (const auto &[name, type]: node->type->getFields()) {
        if (designatorIt == std::end(node->designator)) {
            break;
        }
        if (designatorIt->first == name) {
            designatorIt->second->visit(this);
            designatorIt->second = castIfNeeded(type, std::move(designatorIt->second));
            ++designatorIt;
        }
    }
}

std::optional<TypePtr> SemanticAnalyzer::resolveTypeRef(const std::string &typeName) const {
    if (const auto type = TypeFactory::findPrimitiveType(typeName)) {
        return type;
    }
    const auto it = std::ranges::find_if(declarations, [typeName](const auto &type) {
        return type->getName() == typeName;
    });
    if (it != declarations.end()) {
        return *it;
    }
    return std::nullopt;
}

TypePtr SemanticAnalyzer::resolveTypeIfNeeded(TypePtr type) const {
    if (type->getKind() == TypeKind::Unresolved) {
        if (auto resolvedType = resolveTypeRef(type->getName())) {
            return *resolvedType;
        }
    } else if (type->getKind() == TypeKind::Array) {
        const auto arrayType = std::dynamic_pointer_cast<ArrayType>(type);
        arrayType->setElementType(resolveTypeIfNeeded(arrayType->getElementType()));
    }
    return type;
}
