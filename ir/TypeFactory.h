//
// Created by vadim on 23.03.25.
//

#ifndef TYPEFACTORY_H
#define TYPEFACTORY_H

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
#include "ast/MethodCallNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/TypeNode.h"
#include "ast/TernaryOperatorNode.h"

class TypeFactory final : public NodeVisitor {
public:
    void visit(const IdentNode *node) override {
        if (const auto symbolInfo = mc.symTable.lookup(node->name)) {
            typeNode = symbolInfo.value().type;
        } else if (auto *const func = mc.symTable.lookupFunction(node->name)) {
            typeNode = func->returnType;
        } else if (const auto gVal = mc.symTable.lookupGlobal(node->name)) {
            typeNode = gVal->type;
        }
    }

    void visit(const NumberNode *node) override {
        typeNode = TypeNode::makePrimitive(node->isFloat ? TypeKind::Double : TypeKind::Integer,
                                           false);
    }

    void visit(const StringNode * /*node*/) override {
        typeNode = TypeNode::makePrimitive(TypeKind::Str, true);
    }

    void visit(const BooleanNode * /*node*/) override {
        typeNode = TypeNode::makePrimitive(TypeKind::Boolean, false);
    }

    static bool isCompatible(const TypeNode &t1, const TypeNode &t2) {
        if (t1.kind == TypeKind::Custom || t2.kind == TypeKind::Custom) {
            return t1.typeName == t2.typeName;
        }
        return t1.kind == t2.kind ||
               (t1.isNumeric() && t2.isNumeric());
    }

    static TypeNode getResultType(const TypeNode &t1, const TypeNode &t2) {
        if (t1.kind == TypeKind::Double || t2.kind == TypeKind::Double) {
            return TypeNode::makePrimitive(TypeKind::Double);
        }
        return t1;
    }

    void visit(const BinOpNode *node) override {
        const auto lhsType = determine(node->lhs.get(), mc);
        const auto rhsType = determine(node->rhs.get(), mc);

        if (!isCompatible(lhsType, rhsType)) {
            throw std::logic_error("Incompatible types in binary operation");
        }

        typeNode = getResultType(lhsType, rhsType);
    }

    static bool isAssignable(const TypeNode &target, const TypeNode &source) {
        if (target.isPointer) {
            return source.isPointer && isCompatible(target.dereference(), source.dereference());
        }
        return isCompatible(target, source);
    }

    void visit(const AssignmentNode *node) override {
        const auto lhsType = determine(node->lvalue.get(), mc);
        if (const auto rhsType = determine(node->rvalue.get(), mc);
            !isAssignable(lhsType, rhsType)) {
            throw std::logic_error("Assignment type mismatch");
        }

        typeNode = lhsType;
    }

    void visit(const UnaryOpNode *node) override {
        typeNode = determine(node->expr.get(), mc);
    }

    void visit(const TernaryOperatorNode *node) override {
        const auto trueType = determine(node->trueExpr.get(), mc);

        if (const auto falseType = determine(node->falseExpr.get(), mc);
            !isCompatible(trueType, falseType)) {
            throw std::logic_error("Ternary operator type mismatch");
        }

        typeNode = trueType;
    }

    void visit(const FunctionCallNode *node) override {
        if (const auto *func = mc.symTable.lookupFunction(node->ident->name);
            func != nullptr) {
            typeNode = func->returnType;
        } else {
            throw std::logic_error("Undefined function: " + node->ident->name);
        }
    }

    void visit(const FunctionNode *node) override {
        typeNode = node->proto->returnType;
    }

    void visit(const MethodCallNode *node) override {
        const auto objectType = from(node->object.get(), mc);
        if (const auto *method = objectType->findMethodByName(node->method->ident->name)) {
            typeNode = method->returnType;
        }
    }

    void visit(const DeclarationNode *node) override {
        typeNode = node->type;
    }

    [[nodiscard]] std::unique_ptr<IRType> create() const {
        return from(typeNode.value());
    }

    static std::unique_ptr<IRType> from(const ExpressionNode *const node,
                                        const ModuleContext &mc) {
        return from(determine(node, mc));
    }

    static std::unique_ptr<IRType> from(const TypeKind kind, const bool isPointer) {
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
                type = std::make_unique<StrIRType>(isPointer);
                break;
            case Custom:
                // type = std::make_unique<CustomType>(typeNode.typeName, typeNode.isPointer);
                break;
            default:
                throw std::logic_error("Unknown type kind");
        }
        return type;
    }

    static std::unique_ptr<IRType> from(const TypeNode &typeNode) {
        return from(typeNode.kind, typeNode.isPointer);
    }

    void visit(const ProtoFunctionStatement *node) override {}
    void visit(const IfStatement *node) override {}
    void visit(const LoopCondNode *node) override {}
    void visit(const BlockNode *node) override {}
    void visit(const ReturnNode *node) override {}
    void visit(const CommentNode *node) override {}

private:
    explicit TypeFactory(const ModuleContext &mc):
        mc(mc) {}

    static TypeNode determine(const ExpressionNode *const node,
                              const ModuleContext &mc) {
        TypeFactory visitor(mc);
        node->visit(&visitor);
        return visitor.typeNode.value();
    }

    const ModuleContext &mc;
    std::optional<TypeNode> typeNode;
};

#endif //TYPEFACTORY_H
