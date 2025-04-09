//
// Created by vadim on 09.04.25.
//

#include "TypeFactory.h"
#include "BooleanIRType.h"
#include "ByteIRType.h"
#include "CharIRType.h"
#include "DoubleIRType.h"
#include "IntIRType.h"
#include "ModuleContext.h"
#include "StrIRType.h"
#include "VoidIRType.h"

#include "ast/BaseNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/NumberNode.h"
#include "ast/BinOpNode.h"
#include "ast/FieldAccessNode.h"
#include "ast/MemberAccessNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/TypeNode.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/MethodCallNode.h"

TypeFactory::TypeFactory(const ModuleContext &mc):
    mc(mc) {}

void TypeFactory::visit(const IdentNode *node) {
    if (const auto alloc = mc.symTable.lookup(node->name)) {
        typeNode = alloc.value().type;
    } else if (const auto &func = mc.symTable.lookupFunction(node->name); func) {
        typeNode = func.value()->returnType;
    } else if (const auto gVal = mc.symTable.lookupGlobal(node->name)) {
        typeNode = gVal->type;
    }
}

void TypeFactory::visit(const NumberNode *node) {
    typeNode = TypeNode::makePrimitive(node->isFloat ? TypeKind::Double : TypeKind::Integer,
                                       false);
}

void TypeFactory::visit(const StringNode * /*node*/) {
    typeNode = TypeNode::makePrimitive(TypeKind::Str, true);
}

void TypeFactory::visit(const BooleanNode * /*node*/) {
    typeNode = TypeNode::makePrimitive(TypeKind::Boolean, false);
}

bool TypeFactory::isCompatible(const TypeNode &t1, const TypeNode &t2) {
    if (t1.kind == TypeKind::Custom || t2.kind == TypeKind::Custom) {
        return t1.typeName == t2.typeName;
    }
    return t1.kind == t2.kind || (t1.isNumeric() && t2.isNumeric());
}

TypeNode TypeFactory::getResultType(const TypeNode &t1, const TypeNode &t2) {
    if (t1.kind == TypeKind::Double || t2.kind == TypeKind::Double) {
        return TypeNode::makePrimitive(TypeKind::Double);
    }
    return t1;
}

void TypeFactory::visit(const BinOpNode *node) {
    const auto lhsType = determine(node->lhs.get(), mc);
    const auto rhsType = determine(node->rhs.get(), mc);

    if (!isCompatible(lhsType, rhsType)) {
        throw std::logic_error("Incompatible types in binary operation");
    }

    typeNode = getResultType(lhsType, rhsType);
}

bool TypeFactory::isAssignable(const TypeNode &target, const TypeNode &source) {
    if (target.isPointer) {
        return source.isPointer && isCompatible(target.dereference(), source.dereference());
    }
    return isCompatible(target, source);
}

void TypeFactory::visit(const AssignmentNode *node) {
    const auto lhsType = determine(node->lvalue.get(), mc);
    if (const auto rhsType = determine(node->rvalue.get(), mc);
        !isAssignable(lhsType, rhsType)) {
        throw std::logic_error("Assignment type mismatch");
    }

    typeNode = lhsType;
}

void TypeFactory::visit(const UnaryOpNode *node) {
    typeNode = determine(node->expr.get(), mc);
}

void TypeFactory::visit(const TernaryOperatorNode *node) {
    const auto trueType = determine(node->trueExpr.get(), mc);

    if (const auto falseType = determine(node->falseExpr.get(), mc);
        !isCompatible(trueType, falseType)) {
        throw std::logic_error("Ternary operator type mismatch");
    }

    typeNode = trueType;
}

void TypeFactory::visit(const FunctionCallNode *node) {
    if (const auto &func = mc.symTable.lookupFunction(node->ident->name);
        func.has_value()) {
        typeNode = func.value()->returnType;
    } else {
        throw std::logic_error("Undefined function: " + node->ident->name);
    }
}

void TypeFactory::visit(const FunctionNode *node) {
    typeNode = node->proto->returnType;
}

void TypeFactory::visit(const MethodCallNode *node) {
    const auto objectType = from(node->object.get(), mc);
    if (const auto *method = objectType->findMethodByName(node->method->ident->name)) {
        typeNode = method->returnType;
    }
}

void TypeFactory::visit(const FieldAccessNode *node) {
    const auto objType = from(node->object.get(), mc);
    [[maybe_unused]] auto fieldType = objType->findField(node->field->name);
    throw std::logic_error("Not implemented");
}

void TypeFactory::visit(const DeclarationNode *node) {
    typeNode = node->type;
}

std::unique_ptr<IRType> TypeFactory::create() const {
    return from(typeNode.value());
}

std::unique_ptr<IRType> TypeFactory::from(const ExpressionNode *const node, const ModuleContext &mc) {
    return from(determine(node, mc));
}

std::unique_ptr<IRType> TypeFactory::from(const TypeKind kind, const bool isPointer) {
    std::unique_ptr<IRType> type = nullptr;
    switch (kind) {
            using enum TypeKind;
        case Boolean:
            type = std::make_unique<BooleanIRType>(isPointer);
            break;
        case Byte:
            type = std::make_unique<ByteIRType>(isPointer);
            break;
        case Char:
            type = std::make_unique<CharIRType>(isPointer);
            break;
        case Double:
            type = std::make_unique<DoubleIRType>(isPointer);
            break;
        case Integer:
            type = std::make_unique<IntIRType>(isPointer);
            break;
        case Void:
            type = std::make_unique<VoidIRType>();
            break;
        case Str:
            type = std::make_unique<StrIRType>();
            break;
        case Custom:
            // type = std::make_unique<CustomType>(typeNode.typeName, typeNode.isPointer);
            break;
        default:
            throw std::logic_error("Unknown type kind");
    }
    return type;
}

std::unique_ptr<IRType> TypeFactory::from(const TypeNode &typeNode) {
    return from(typeNode.kind, typeNode.isPointer);
}

TypeNode TypeFactory::determine(const ExpressionNode *const node, const ModuleContext &mc) {
    TypeFactory visitor(mc);
    node->visit(&visitor);
    return visitor.typeNode.value();
}
