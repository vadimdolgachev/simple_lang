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
#include "ast/VariableAccessNode.h"
#include "Util.h"

namespace {
    std::string makeTestFailMsg(const std::uint32_t line) {
        return std::string("test failed, line=").append(std::to_string(line));
    }

    void testVarDefinition() {
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

    void testParseBinExpression() {
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
                if (binOp->binOp != TokenType::MinusToken) {
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
    }

    void parseIdentifiers() {
        auto parser = std::make_unique<Parser>(std::make_unique<Lexer>(std::make_unique<std::istringstream>("v+1;")));
        auto node = parser->parseNextNode();
        if (node == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const binOp = dynamic_cast<BinOpNode *>(node.get());
        if (binOp == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const lhs = dynamic_cast<VariableAccessNode *>(binOp->lhs.get());
        if (lhs == nullptr || lhs->name != "v") {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const rhs = dynamic_cast<NumberNode *>(binOp->rhs.get());
        if (rhs == nullptr || rhs->value != 1.0) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
    }
} // namespace


int main(int argc, const char *argv[]) {
    testVarDefinition();
    testParseBinExpression();
    parseIdentifiers();
    return 0;
}
