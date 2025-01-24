#include "Parser.h"
#include "ast/VariableAccessNode.h"
#include "ast/VariableDefinitionStatement.h"

// Declaration -> Identifier Initialization
// Identifier -> [a-zA-Z_][a-zA-Z0-9_]*
// Initialization -> "=" Expression | ε

// Declaration -> Identifier "(" Parameters ")" Block
// Parameters -> Parameter "," Parameters | Parameter | ε
// Parameter -> Identifier
// Block -> "{" Declarations "}"

// Assignment -> Identifier "=" Expression ";"
// Expression -> Literal | Identifier | Expression
// Literal -> Integer
// Integer -> [0-9]+

// Expression -> Term (( '+' | '-' ) Term)*
// Term -> Factor (( '*' | '/' ) Factor)*
// Factor -> '(' Expression ')' | Number | Identifier

// Statement -> "if" "(" Expression ")" Block ("else" Block)?
// Statement -> "while" "(" Expression ")" Block

namespace {
    bool isEndOfExpr(const TokenType token) {
        return token == TokenType::EosToken || token == TokenType::RightParenthesisToken;
    }
} // namespace

Parser::Parser(std::unique_ptr<Lexer> lexer) :
    lexer(std::move(lexer)) {}

Parser::operator bool() const {
    return lexer->hasNextToken();
}

std::unique_ptr<BaseNode> Parser::parseNextNode() {
    const auto [typeType, value] = lexer->nextToken();
    // Assignment
    if (typeType == TokenType::IdentifierToken) {
        lexer->nextToken(true);
        if (lexer->peekToken().type == TokenType::EqualsToken && value.has_value()) {
            return parseDefinition(value.value());
        }
        // Rollback
        lexer->prevToken();
    }
    // Binary expr
    if (typeType == TokenType::NumberToken
        || typeType == TokenType::LeftParenthesisToken
        || typeType == TokenType::IdentifierToken) {
        return parseExpr();
    }
    return nullptr;
}

std::unique_ptr<ExpressionNode> Parser::parseExpr() {
    return parseTerm();
}

std::unique_ptr<ExpressionNode> Parser::parsePrimary() {
    if (lexer->peekToken().type == TokenType::LeftParenthesisToken) {
        lexer->nextToken();
        auto expr = parseExpr();
        lexer->nextToken(true);
        return expr;
    }
    if (lexer->peekToken().type == TokenType::NumberToken) {
        auto value = parseNumber();
        lexer->nextToken(true);
        return value;
    }
    if (lexer->peekToken().type == TokenType::IdentifierToken) {
        auto ident = parseIdent();
        lexer->nextToken(true);
        return ident;
    }
    throw std::runtime_error("Unexpected token");
}

std::unique_ptr<ExpressionNode> Parser::parseFactor() {
    auto lhs = parsePrimary();
    while (lexer->peekToken().type == TokenType::StarToken
        || lexer->peekToken().type == TokenType::SlashToken) {
        const auto op = lexer->peekToken().type;
        lexer->nextToken(true);
        auto rhs = parsePrimary();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<ExpressionNode> Parser::parseTerm() {
    auto lhs = parseFactor();
    if (lexer->peekToken().type == TokenType::PlusToken) {
        lexer->nextToken(true);
        return std::make_unique<BinOpNode>(TokenType::PlusToken,
            std::move(lhs),
            parseFactor());
    }
    if (lexer->peekToken().type == TokenType::MinusToken) {
        lexer->nextToken(true);
        return std::make_unique<BinOpNode>(TokenType::MinusToken,
        std::move(lhs),
        parseFactor());
    }
    if (isEndOfExpr(lexer->peekToken().type)) {
        return lhs;
    }
    throw std::runtime_error("Unexpected token");
}

std::unique_ptr<ExpressionNode> Parser::parseIdent() const {
    return std::make_unique<VariableAccessNode>(lexer->peekToken().value.value_or(""));
}

std::unique_ptr<BaseNode> Parser::parseDefinition(std::string identName) {
    lexer->nextToken(true);
    auto expr = parseExpr();
    return std::make_unique<VariableDefinitionStatement>(std::move(identName), std::move(expr));
}

std::unique_ptr<NumberNode> Parser::parseNumber() const {
    auto number = std::make_unique<NumberNode>(strtod(lexer->peekToken().value.value_or("").c_str(), nullptr));
    return number;
}