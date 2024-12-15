#include "Parser.h"

namespace {
    int getInfixOpPrecedence(const TokenType token) {
        if (token == TokenType::PlusToken || token == TokenType::MinusToken) {
            return 1;
        }
        if (token == TokenType::MultiplyToken || token == TokenType::DivideToken) {
            return 2;
        }
        return 0;
    }
} // namespace

Parser::Parser(std::unique_ptr<Lexer> lexer) : lexer(std::move(lexer)) {}

Parser::operator bool() const { return lexer->hasNextToken(); }

std::unique_ptr<BaseNode> Parser::parseNextNode() {
    const auto token = lexer->readNextToken();

    if (token == TokenType::NumberToken
        || token == TokenType::LeftParenthesisToken) {
        return parseExpr();
    }

    return nullptr;
}

static bool isEndOfExpr(const TokenType token) {
    return token == TokenType::EosToken || token == TokenType::RightParenthesisToken;
}

std::unique_ptr<ExpressionNode> Parser::parseExpr(const int operatorPrecedence) {
    if (lexer->getCurrentToken() == TokenType::NumberToken) {
        std::unique_ptr<ExpressionNode> resultExpr = parseNumberExpr();
        const auto token = lexer->readNextToken(true);
        // parsing of binary expression
        if (Lexer::isArithmeticOp(token)) {
            while (operatorPrecedence < getInfixOpPrecedence(token)
                && !isEndOfExpr(lexer->getCurrentToken())) {
                resultExpr = parseBinExpr(std::move(resultExpr));
            }
        }
        return resultExpr;
    }
    if (lexer->getCurrentToken() == TokenType::LeftParenthesisToken) {
        lexer->readNextToken(); // eat "("
        auto expr = parseExpr();
        lexer->readNextToken(); // eat ")"
        return expr;
    }
    return nullptr;
}

std::unique_ptr<NumberNode> Parser::parseNumberExpr() const {
    auto number = std::make_unique<NumberNode>(strtod(lexer->getNumberValue().c_str(), nullptr));
    return number;
}

std::unique_ptr<BinOpNode> Parser::parseBinExpr(std::unique_ptr<ExpressionNode> lhsOp) {
    const auto binOp = lexer->getCurrentToken();
    lexer->readNextToken(true);
    auto rhsOp = parseExpr(getInfixOpPrecedence(binOp));
    auto binExpr = std::make_unique<BinOpNode>(binOp, std::move(lhsOp), std::move(rhsOp));
    return binExpr;
}
