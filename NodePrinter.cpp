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
#include "ast/ModuleNode.h"
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
            case TokenType::PlusPlus:
                os << "++";
                break;
            case TokenType::MinusMinus:
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

void NodePrinter::visit(IdentNode *node) {
    ostream << "VariableAccess: name=" << node->name << ", ";
}

void NodePrinter::visit(NumberNode *node) {
    ostream << "Number value=" << node->value;
}

void NodePrinter::visit(StringNode *node) {
    ostream << "String value=" << node->str;
}

void NodePrinter::visit(BinOpNode *node) {
    ostream << "BinOp: op=" << node->binOp;
    ostream << ", lhs=(";
    node->lhs->visit(this);
    ostream << ")";
    ostream << ", rhs=(";
    node->rhs->visit(this);
    ostream << ")";
}

void NodePrinter::visit(BooleanNode *node) {
    ostream << "Boolean value=" << node->value;
}

void NodePrinter::visit(FunctionNode *node) {
    ostream << "Function: name=" << node->proto->toString();
}

void NodePrinter::visit(ProtoFunctionStatement *node) {
    ostream << "ProtoFunction: name=" << node->name;
}

void NodePrinter::visit(AssignmentNode *node) {
    ostream << "VariableDefinition: var=" << node->lvalue;
}

void NodePrinter::visit(FunctionCallNode *node) {
    ostream << "CallFunctionNode: name=" << node->ident->name;
}

void NodePrinter::visit(IfStatement *node) {
    ostream << "IfStatement";
}

void NodePrinter::visit(UnaryOpNode *node) {
    ostream << "UnaryOp: name=" << node->operatorType;
}

void NodePrinter::visit(LoopCondNode *node) {
    ostream << "WhileLoop";
}

void NodePrinter::visit(BlockNode *node) {
    ostream << "Block";
}

void NodePrinter::visit(DeclarationNode *node) {
    ostream << "DeclarationNode";
}

void NodePrinter::visit(ReturnNode *node) {
    ostream << "ReturnNode";
}

void NodePrinter::visit(TernaryOperatorNode *node) {
    ostream << "TernaryOperatorNode";
}

void NodePrinter::visit(MethodCallNode *node) {
    ostream << "MethodCallNode";
}

void NodePrinter::visit(FieldAccessNode *node) {
    ostream << "FieldAccessNode";
}

void NodePrinter::visit(CommentNode *node) {

}

void NodePrinter::visit(ModuleNode *node) {

}

void NodePrinter::visit(TypeCastNode *node) {
    ostream << "TypeCastNode";
}
