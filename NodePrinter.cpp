//
// Created by vadim on 04.11.24.
//

#include "NodePrinter.h"
#include "ast/BinOpNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/NumberNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/BooleanNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/StringNode.h"

namespace {
    std::ostream &operator<<(std::ostream &os, const TokenType token) {
        switch (token) {
            case TokenType::Plus:
                os << "+";
                break;
            case TokenType::Minus:
                os << "-";
                break;
            case TokenType::Star:
                os << "*";
                break;
            case TokenType::Slash:
                os << "/";
                break;
            case TokenType::IncrementOperator:
                os << "++";
                break;
            case TokenType::DecrementOperator:
                os << "--";
                break;
            default:
                os << "unknown token";
        }
        return os;
    }
} // namespace

NodePrinter::NodePrinter(std::ostream &os)
    : ostream(os) {
}

void NodePrinter::visit(const IdentNode *node) {
    ostream << "VariableAccess: name=" << node->name << ", ";
}

void NodePrinter::visit(const NumberNode *node) {
    ostream << "Number value=" << node->value;
}

void NodePrinter::visit(const StringNode *node) {
    ostream << "String value=" << node->str;
}

void NodePrinter::visit(const BinOpNode *node) {
    ostream << "BinOp: op=" << node->binOp;
    ostream << ", lhs=(";
    node->lhs->visit(this);
    ostream << ")";
    ostream << ", rhs=(";
    node->rhs->visit(this);
    ostream << ")";
}

void NodePrinter::visit(const BooleanNode *node) {
    ostream << "Boolean value=" << node->value;
}

void NodePrinter::visit(const FunctionNode *node) {
    ostream << "Function: name=" << node->proto->toString();
}

void NodePrinter::visit(const ProtoFunctionStatement *node) {
    ostream << "ProtoFunction: name=" << node->name;
}

void NodePrinter::visit(const AssignmentNode *node) {
    ostream << "VariableDefinition: var=" << node->name;
}

void NodePrinter::visit(const FunctionCallNode *node) {
    ostream << "CallFunctionNode: name=" << node->ident;
}

void NodePrinter::visit(const IfStatement *node) {
    ostream << "IfStatement";
}

void NodePrinter::visit(const ForLoopNode *node) {
    ostream << "ForLoop";
}

void NodePrinter::visit(const UnaryOpNode *node) {
    ostream << "UnaryOp: name=" << node->operatorType;
}

void NodePrinter::visit(const LoopCondNode *node) {
    ostream << "WhileLoop";
}

void NodePrinter::visit(const BlockNode *node) {
    ostream << "Block";
}