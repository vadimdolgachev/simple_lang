//
// Created by vadim on 14.12.24.
//

#include <functional>
#include <memory>
#include <sstream>

#include "Lexer.h"
#include "Parser.h"
#include "ast/BinOpNode.h"
#include "ast/BaseNode.h"
#include "ast/VariableDefinitionStatement.h"
#include "ast/IdentNode.h"
#include "ast/UnaryOpNode.h"
#include "Util.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"

namespace {
    std::string makeTestFailMsg(const std::uint32_t line) {
        return std::string("test failed, line=").append(std::to_string(line));
    }

    void testVarDefinition() {
        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("varName=2*(1-2);")));
            auto ident = parser->parseNextNode();
            if (auto [varExprAst, orig] = tryCast<VariableDefinitionStatement>(std::move(ident));
                varExprAst != nullptr) {
                if (varExprAst == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const var = (varExprAst.get());
                if (var->name != "varName") {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const binOp = dynamic_cast<BinOpNode *>(var->rvalue.get());
                if (binOp == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
        }
        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("varName=1;")));
            auto ident = parser->parseNextNode();
            if (auto [varExprAst, orig] = tryCast<VariableDefinitionStatement>(std::move(ident));
                varExprAst != nullptr) {
                if (varExprAst == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const var = (varExprAst.get());
                if (var->name != "varName") {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const value = dynamic_cast<NumberNode *>(var->rvalue.get());
                if (value == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
        }
    }

    void testBinExpressions() {
        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("-1-21.2;")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto [binOp, orig] = tryCast<BinOpNode>(std::move(node)); binOp != nullptr) {
                if (binOp == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (binOp->binOp != TokenType::Minus) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const lhsNumber = dynamic_cast<NumberNode *>(binOp->lhs.get());
                if (lhsNumber == nullptr || lhsNumber->value != -1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const rhsNumber = dynamic_cast<NumberNode *>(binOp->rhs.get());
                if (rhsNumber == nullptr || rhsNumber->value != 21.2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
        }
        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("-1.123")));
            const auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const value = dynamic_cast<NumberNode *>(node.get());
                value == nullptr || value->value != -1.123) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("(2*(1+2));")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }

            if (auto [binOp, orig] = tryCast<BinOpNode>(std::move(node)); binOp != nullptr) {
                const auto *const lhsNumber = dynamic_cast<NumberNode *>(binOp->lhs.get());
                if (lhsNumber == nullptr || lhsNumber->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const binOpRhs = dynamic_cast<BinOpNode *>(binOp->rhs.get());
                if (binOpRhs == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const lhs = dynamic_cast<NumberNode *>(binOpRhs->lhs.get());
                if (lhs == nullptr || lhs->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const rhs = dynamic_cast<NumberNode *>(binOpRhs->rhs.get());
                if (rhs == nullptr || rhs->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>("+1 *  (   2    +3.0);")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }

            if (auto [binOp, orig] = tryCast<BinOpNode>(std::move(node)); binOp != nullptr) {
                {
                    const auto *const lhsNumber = dynamic_cast<NumberNode *>(binOp->lhs.get());
                    if (lhsNumber == nullptr || lhsNumber->value != 1) {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                }
                {
                    const auto *const binOpRhs = dynamic_cast<BinOpNode *>(binOp->rhs.get());
                    if (binOpRhs == nullptr) {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                    const auto *const lhs = dynamic_cast<NumberNode *>(binOpRhs->lhs.get());
                    if (lhs == nullptr || lhs->value != 2) {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                    const auto *const rhs = dynamic_cast<NumberNode *>(binOpRhs->rhs.get());
                    if (rhs == nullptr || rhs->value != 3.0) {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                }
            }
        }

        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("1+2+3;")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto [binOp, orig] = tryCast<BinOpNode>(std::move(node)); binOp != nullptr) {
                if (binOp == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const rhs = dynamic_cast<NumberNode *>(binOp->rhs.get());
                    rhs != nullptr && rhs->value != 3) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const binOpNode = dynamic_cast<BinOpNode *>(binOp->lhs.get());
                if (binOpNode == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const lhs = dynamic_cast<NumberNode *>(binOpNode->lhs.get());
                    lhs == nullptr || lhs->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const rhs = dynamic_cast<NumberNode *>(binOpNode->rhs.get());
                    rhs == nullptr || rhs->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
        }
    }

    void testIdentifiers() {
        {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>("v+1;")));
            const auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpNode *>(node.get());
            if (binOp == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const lhs = dynamic_cast<IdentNode *>(binOp->lhs.get());
                lhs == nullptr || lhs->name != "v") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const rhs = dynamic_cast<NumberNode *>(binOp->rhs.get());
                rhs == nullptr || rhs->value != 1.0) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }

        {
            const auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(
                    std::make_unique<std::istringstream>("var")));
            const auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const value = dynamic_cast<IdentNode *>(node.get());
                value == nullptr || value->name != "var") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
    }

    void testIncrementOperators() {
        {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>("var++")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto [unOp, orig] = tryCast<UnaryOpNode>(std::move(node)); unOp != nullptr) {
                if (unOp->operatorType != TokenType::IncrementOperator) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->unaryPosType != UnaryOpNode::UnaryOpType::Postfix) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->expr == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const var = dynamic_cast<IdentNode *>(unOp->expr.get())) {
                    if (var->name != "var") {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>("++var")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto [unOp, orig] = tryCast<UnaryOpNode>(std::move(node)); unOp != nullptr) {
                if (unOp->operatorType != TokenType::IncrementOperator) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->unaryPosType != UnaryOpNode::UnaryOpType::Prefix) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->expr == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const var = dynamic_cast<IdentNode *>(unOp->expr.get())) {
                    if (var->name != "var") {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>("var--")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto [unOp, orig] = tryCast<UnaryOpNode>(std::move(node)); unOp != nullptr) {
                if (unOp->operatorType != TokenType::DecrementOperator) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->unaryPosType != UnaryOpNode::UnaryOpType::Postfix) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->expr == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const var = dynamic_cast<IdentNode *>(unOp->expr.get())) {
                    if (var->name != "var") {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>("--var")));
            auto node = parser->parseNextNode();
            if (node == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto [unOp, orig] = tryCast<UnaryOpNode>(std::move(node)); unOp != nullptr) {
                if (unOp->operatorType != TokenType::DecrementOperator) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->unaryPosType != UnaryOpNode::UnaryOpType::Prefix) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (unOp->expr == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const var = dynamic_cast<IdentNode *>(unOp->expr.get())) {
                    if (var->name != "var") {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
    }

    void testFunctionCalls() {
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>("foo(1, 2.1, var, 1 + 2)")));
        auto node = parser->parseNextNode();
        if (node == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        if (auto [fnCall, orig] = tryCast<FunctionCallNode>(std::move(node)); fnCall != nullptr) {
            if (fnCall->ident->name != "foo") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (fnCall->args.size() != 4) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const var = dynamic_cast<NumberNode *>(fnCall->args[0].get())) {
                if (var->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const var = dynamic_cast<NumberNode *>(fnCall->args[1].get())) {
                if (var->value != 2.1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const var = dynamic_cast<IdentNode *>(fnCall->args[2].get())) {
                if (var->name != "var") {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const binOp = dynamic_cast<BinOpNode *>(fnCall->args[3].get())) {
                if (const auto *const lhs = dynamic_cast<NumberNode *>(binOp->lhs.get())) {
                    if (lhs->value != 1) {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (const auto *const rhs = dynamic_cast<NumberNode *>(binOp->rhs.get())) {
                    if (rhs->value != 2) {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } else {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
    }

    void testFunctionDefs() {
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(
                        std::make_unique<std::istringstream>(R"(
        fn foo(arg1, arg2, arg3, arg4) {
            v = 1
            ++v
        }
        v=2
        )")));
        auto node = parser->parseNextNode();
        if (node == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        if (auto [fn, orig] = tryCast<FunctionNode>(std::move(node)); fn != nullptr) {
            if (fn->name->name != "foo") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (fn->params.size() != 4) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (fn->params[0]->name != "arg1") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (fn->params[1]->name != "arg2") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (fn->params[2]->name != "arg3") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (fn->params[3]->name != "arg4") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (const auto *const statement = dynamic_cast<IdentNode *>(fn->body[0].get())) {
                if (statement->name != "v") {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            if (const auto *const statement = dynamic_cast<UnaryOpNode *>(fn->body[1].get())) {
                if (const auto *const var = dynamic_cast<IdentNode *>(statement->expr.get())) {
                    if (var->name != "v") {
                        throw std::logic_error(makeTestFailMsg(__LINE__));
                    }
                } else {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (statement->operatorType != TokenType::IncrementOperator) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                if (statement->unaryPosType != UnaryOpNode::UnaryOpType::Prefix) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
        } else {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
    }
} // namespace


int main(int argc, const char *argv[]) {
    testVarDefinition();
    testBinExpressions();
    testIdentifiers();
    testIncrementOperators();
    testFunctionCalls();
    testFunctionDefs();
    return 0;
}
