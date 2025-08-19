//
// Created by vadim on 04.11.24.
//

#include "NodePrinter.h"

#include <algorithm>
#include <iterator>

#include "ast/BinOpNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/NumberNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/BooleanNode.h"
#include "ast/CommentNode.h"
#include "ast/IfStatement.h"
#include "ast/LoopCondNode.h"
#include "ast/ModuleNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/ReturnNode.h"
#include "ast/StringNode.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/TypeCastNode.h"
#include "ast/ArrayNode.h"
#include "ast/IndexAccessNode.h"
#include "ast/MethodCallNode.h"
#include "ast/StructInitNode.h"
#include "ast/StructDeclarationNode.h"

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
            case TokenType::Increment:
                os << "++";
                break;
            case TokenType::Decrement:
                os << "--";
                break;
            case TokenType::Less:
                os << "<";
                break;
            case TokenType::LessEqual:
                os << "<=";
                break;
            case TokenType::Greater:
                os << ">";
                break;
            case TokenType::GreaterEqual:
                os << ">=";
                break;
            case TokenType::Equal:
                os << "==";
                break;
            case TokenType::NotEqual:
                os << "!=";
                break;
            case TokenType::LogicalAnd:
                os << "&&";
                break;
            case TokenType::LogicalOr:
                os << "||";
                break;
            default:
                os << "unknown token";
        }
        return os;
    }
} // namespace

NodePrinter::NodePrinter(std::ostream &os) :
    ostream(os) {}

void NodePrinter::visit(IdentNode *node) {
    ostream << node->name;
}

void NodePrinter::visit(NumberNode *node) {
    ostream << node->value;
}

void NodePrinter::visit(StringNode *node) {
    ostream << "\"" << node->text << "\"";
}

void NodePrinter::visit(BinOpNode *node) {
    node->lhs->visit(this);
    ostream << node->binOp;
    node->rhs->visit(this);
}

void NodePrinter::visit(BooleanNode *node) {
    ostream << std::boolalpha << node->value;
}

void NodePrinter::visit(FunctionNode *node) {
    node->proto->visit(this);
    node->body->visit(this);
}

void NodePrinter::visit(ProtoFunctionStatement *node) {
    ostream << "fn " << node->name << "(";
    for (size_t i = 0; i < node->params.size(); i++) {
        node->params[i]->visit(this);
        ostream << (i < node->params.size() - 1 ? ", " : "");
    }
    ostream << "):" << node->returnType->getName();
}

void NodePrinter::visit(AssignmentNode *node) {
    node->lvalue->visit(this);
    ostream << "=";
    node->rvalue->visit(this);
}

void NodePrinter::visit(FunctionCallNode *node) {
    node->ident->visit(this);
    ostream << "(";
    for (size_t i = 0; i < node->args.size(); i++) {
        node->args[i]->visit(this);
        ostream << (i < node->args.size() - 1 ? ", " : "");
    }
    ostream << ")";
}

void NodePrinter::visit(IfStatement *node) {
    ostream << "if (";
    node->ifBranch.cond->visit(this);
    ostream << ") ";
    node->ifBranch.then->visit(this);
    for (const auto &[cond, then]: node->elseIfBranches) {
        ostream << " else if (";
        cond->visit(this);
        ostream << ") ";
        then->visit(this);
    }
    if (node->elseBranch) {
        ostream << " else ";
        node->elseBranch.value()->visit(this);
    }
}

void NodePrinter::visit(UnaryOpNode *node) {
    if (node->unaryPosType == UnaryOpNode::UnaryOpType::Prefix) {
        ostream << node->operatorType;
    }
    node->expr->visit(this);
    if (node->unaryPosType == UnaryOpNode::UnaryOpType::Postfix) {
        ostream << node->operatorType;
    }
}

void NodePrinter::visit(LoopCondNode *node) {
    if (node->loopType == LoopCondNode::Type::For) {
        ostream << "for(";
        if (node->init) {
            node->init.value()->visit(this);
        }
        ostream << ";";
        node->condBranch.cond->visit(this);
        ostream << ";";
        if (node->increment) {
            node->increment.value()->visit(this);
        }
        ostream << ") ";
        node->condBranch.then->visit(this);
    }
}

void NodePrinter::visit(BlockNode *node) {
    ostream << "{";
    if (!node->statements.empty()) {
        ostream << "\n";
        indent += 2;
        for (const auto &statement: node->statements) {
            printIndent();
            statement->visit(this);
            if (needSemicolon(statement)) {
                ostream << ";";
            }
            ostream << "\n";
        }
        indent -= 2;
        printIndent();
    }
    ostream << "}";
}

void NodePrinter::visit(DeclarationNode *node) {
    node->ident->visit(this);
    ostream << ":" << node->type->getName();
    if (node->init) {
        ostream << "=";
        node->init.value()->visit(this);
    }
}

void NodePrinter::visit(ReturnNode *node) {
    ostream << "return ";
    node->expr->visit(this);
}

void NodePrinter::visit(TernaryOperatorNode *node) {
    node->cond->visit(this);
    ostream << "?";
    node->trueExpr->visit(this);
    ostream << ":";
    node->falseExpr->visit(this);
}

void NodePrinter::visit(MethodCallNode *node) {
    node->object->visit(this);
    ostream << ".";
    node->method->visit(this);
}

void NodePrinter::visit(FieldAccessNode *node) {
    ostream << "FieldAccessNode";
}

void NodePrinter::visit(CommentNode *node) {
    ostream << "//" << node->text;
}

void NodePrinter::visit(ModuleNode *node) {
    ostream << "module:\n";
    for (const auto &stmt: node->statements) {
        stmt->visit(this);
        ostream << "\n";
    }
}

void NodePrinter::visit(TypeCastNode *node) {
    ostream << "(" << node->targetType->getName() << ")";
    node->expr->visit(this);
}

void NodePrinter::visit(ArrayNode *node) {
    ostream << "[";
    for (size_t i = 0; i < node->elements.size(); ++i) {
        ostream << node->elements[i]->toString() << (i < node->elements.size() - 1 ? "," : "");
    }
    ostream << "]";
}

void NodePrinter::visit(IndexAccessNode *node) {
    ostream << node->object->toString() << "[" << node->index->toString() << "]";
}

void NodePrinter::visit(StructDeclarationNode *node) {
    ostream << "struct " << node->name;
}

void NodePrinter::visit(StructInitNode *node) {
    ostream << "StructInitNode " << node->ident;
}

void NodePrinter::printIndent() const {
    std::fill_n(std::ostream_iterator<char>(ostream), indent, ' ');
}

bool NodePrinter::needSemicolon(const std::unique_ptr<BaseNode> &node) {
    return dynamic_cast<ExpressionNode *>(node.get())
           || dynamic_cast<DeclarationNode *>(node.get())
           || dynamic_cast<ReturnNode *>(node.get());
}
