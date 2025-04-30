//
// Created by vadim on 09.04.25.
//

#include "IRTypeFactory.h"
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
#include "../type/Type.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/MethodCallNode.h"
#include "type/FunctionType.h"
#include "type/TypeFactory.h"

IRTypeFactory::IRTypeFactory(const ModuleContext &mc):
    mc(mc) {}

void IRTypeFactory::visit(IdentNode *node) {
    if (const auto alloc = mc.symTable.lookup(node->name)) {
        typeNode = alloc.value()->type;
    } else if (const auto gVal = mc.symTable.lookupGlobal(node->name)) {
        if (const auto &f = std::dynamic_pointer_cast<const FunctionType>(gVal.value()->type)) {
            typeNode = f->returnType();
        } else {
            typeNode = gVal.value()->type;
        }
    }
}

void IRTypeFactory::visit(NumberNode *node) {
    typeNode = TypeFactory::makePrimitiveType(node->isFloat ? TypeKind::Double : TypeKind::Integer);
}

void IRTypeFactory::visit(StringNode * /*node*/) {
    typeNode = TypeFactory::makePrimitiveType(TypeKind::Str);
}

void IRTypeFactory::visit(BooleanNode * /*node*/) {
    typeNode = TypeFactory::makePrimitiveType(TypeKind::Boolean);
}

void IRTypeFactory::visit(BinOpNode *node) {
    const auto lhsType = determine(node->lhs.get(), mc);
    const auto rhsType = determine(node->rhs.get(), mc);

    // if (!isCompatible(lhsType, rhsType)) {
    //     throw std::logic_error("Incompatible types in binary operation");
    // }
    throw std::logic_error("Not implemented");

    // typeNode = getResultType(lhsType, rhsType);
}

void IRTypeFactory::visit(AssignmentNode *node) {
    // const auto lhsType = determine(node->lvalue.get(), mc);
    // if (const auto rhsType = determine(node->rvalue.get(), mc);
    //     !isAssignable(lhsType, rhsType)) {
    //     throw std::logic_error("Assignment type mismatch");
    // }
    throw std::logic_error("Not implemented");

    // typeNode = lhsType;
}

void IRTypeFactory::visit(UnaryOpNode *node) {
    typeNode = determine(node->expr.get(), mc);
}

void IRTypeFactory::visit(TernaryOperatorNode *node) {
    const auto trueType = determine(node->trueExpr.get(), mc);

    // if (const auto falseType = determine(node->falseExpr.get(), mc);
    //     !isCompatible(trueType, falseType)) {
    //     throw std::logic_error("Ternary operator type mismatch");
    // }

    throw std::logic_error("Not implemented");

    typeNode = trueType;
}

void IRTypeFactory::visit(FunctionCallNode *node) {
    if (const auto &func = mc.symTable.lookup(node->ident->name);
        func.has_value()) {
        if (const auto &f = std::dynamic_pointer_cast<const FunctionType>(func.value()->type)) {
            typeNode = f->returnType();
        }
    } else {
        throw std::logic_error("Undefined function: " + node->ident->name);
    }
}

void IRTypeFactory::visit(FunctionNode *node) {
    typeNode = node->proto->returnType;
}

void IRTypeFactory::visit(MethodCallNode *node) {
    const auto objectType = from(node->object.get(), mc);
    if (const auto *method = objectType->findMethodByName(node->method->ident->name)) {
        typeNode = method->returnType;
    }
}

void IRTypeFactory::visit(FieldAccessNode *node) {
    const auto objType = from(node->object.get(), mc);
    [[maybe_unused]] auto fieldType = objType->findField(node->field->name);
    throw std::logic_error("Not implemented");
}

void IRTypeFactory::visit(DeclarationNode *node) {
    typeNode = node->type;
}

void IRTypeFactory::visit(TypeCastNode *node) {

}

std::unique_ptr<IRType> IRTypeFactory::create() const {
    // return from(typeNode.value());
    throw std::logic_error("Not implemented");
}

std::unique_ptr<IRType> IRTypeFactory::from(ExpressionNode *const node, const ModuleContext &mc) {
    // return from(determine(node, mc));
    throw std::logic_error("Not implemented");
}

std::unique_ptr<IRType> IRTypeFactory::from(const TypeKind kind, const bool isPointer) {
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

TypePtr IRTypeFactory::determine(ExpressionNode *const node, const ModuleContext &mc) {
    IRTypeFactory visitor(mc);
    node->visit(&visitor);
    return visitor.typeNode.value();
}
