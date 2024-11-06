//
// Created by vadim on 04.11.24.
//

#include "NodePrinter.h"
#include "ast/BinOpNode.h"
#include "ast/CallFunctionNode.h"
#include "ast/FunctionNode.h"
#include "ast/NumberNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/VariableAccessNode.h"
#include "ast/VariableDefinitionStatement.h"

namespace {
    std::ostream &operator<<(std::ostream &os, const TokenType token) {
        switch (token) {
            case TokenType::PlusToken:
                os << "+";
                break;
            case TokenType::MinusToken:
                os << "-";
                break;
            case TokenType::MultiplyToken:
                os << "*";
                break;
            case TokenType::DivideToken:
                os << "/";
                break;
            case TokenType::IncrementOperatorToken:
                os << "++";
                break;
            case TokenType::DecrementOperatorToken:
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

void NodePrinter::visit(const VariableAccessNode *node) {
    ostream << "VariableAccess: name=" << node->name << ", ";
}

void NodePrinter::visit(const NumberNode *node) {
    ostream << node->value;
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

void NodePrinter::visit(const FunctionNode *node) {
    ostream << "Function: name=" << node->proto->name;
}

void NodePrinter::visit(const ProtoFunctionStatement *node) {
    ostream << "ProtoFunction: name=" << node->name;
}

void NodePrinter::visit(const VariableDefinitionStatement *node) {
    ostream << "VariableDefinition: var=" << node->name;
}

void NodePrinter::visit(const CallFunctionNode *node) {
    ostream << "CallFunctionNode: name=" << node->callee;
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
