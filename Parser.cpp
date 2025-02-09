#include "Parser.h"

#include <complex>

#include "ast/BinOpNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/ForLoopNode.h"
#include "ast/IfStatement.h"

namespace {
    bool isEndOfExpr(const TokenType token) {
        return token == TokenType::Eos || token == TokenType::RightParenthesis;
    }

    bool isArithmeticOp(const TokenType token) {
        return token == TokenType::Plus
               || token == TokenType::Minus
               || token == TokenType::Star
               || token == TokenType::Slash;
    }

    bool isSign(const TokenType token) {
        return token == TokenType::Plus || token == TokenType::Minus;
    }
} // namespace

Parser::Parser(std::unique_ptr<Lexer> lexer_) :
    lexer(std::move(lexer_)) {
    lexer->nextToken();
}

Parser::operator bool() const {
    return lexer->hasNextToken();
}

std::unique_ptr<BaseNode> Parser::parseNextNode() {
    const auto [tokenType, value] = lexer->currToken();
    // Assignment
    if (tokenType == TokenType::Identifier) {
        lexer->nextToken();
        if (lexer->currToken().type == TokenType::Assignment && value.has_value()) {
            lexer->nextToken();
            return parseAssignment(value.value());
        }
        // Rollback
        lexer->prevToken();
    } else if (tokenType == TokenType::FunctionDefinition) {
        lexer->nextToken();
        return parseFunctionDef();
    }
    if (tokenType == TokenType::If) {
        lexer->nextToken();
        return parseIfStatement();
    }
    if (tokenType == TokenType::ForLoop) {
        lexer->nextToken();
        return parseForStatement();
    }
    // Expressions
    if (isSign(tokenType)
        || tokenType == TokenType::Number
        || tokenType == TokenType::LeftParenthesis
        || tokenType == TokenType::Identifier
        || tokenType == TokenType::IncrementOperator
        || tokenType == TokenType::DecrementOperator
        || tokenType == TokenType::LogicalNegation
        || tokenType == TokenType::Minus
        || tokenType == TokenType::Plus) {
        return parseExpr();
    }
    return nullptr;
}

IfStatement::CondBranch Parser::parseCondBranch() {
    auto condition = parseExpr();
    std::vector<std::unique_ptr<BaseNode>> thenBranch;
    if (lexer->currToken().type == TokenType::LeftCurlyBracket) {
        thenBranch = parseCurlyBracketBlock();
    } else {
        thenBranch.emplace_back(parseExpr());
    }
    return {std::move(condition), std::move(thenBranch)};
}

std::unique_ptr<BaseNode> Parser::parseForStatement() {
    if (lexer->currToken().type != TokenType::LeftParenthesis) {
        throw std::runtime_error("Expected '(' after 'for'");
    }
    lexer->nextToken(); // '('
    auto initExpr = parseNextNode();
    if (lexer->currToken().type != TokenType::Semicolon) {
        throw std::runtime_error("Expected ';' after init statement");
    }
    lexer->nextToken(); // ';'
    auto condition = parseExpr();
    if (lexer->currToken().type != TokenType::Semicolon) {
        throw std::runtime_error("Expected ';' after condition");
    }
    lexer->nextToken(); // ';'
    auto nextExpr = parseExpr();
    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error("Expected ')'");
    }
    lexer->nextToken(); // ')'
    std::vector<std::unique_ptr<BaseNode>> forBody;
    if (lexer->currToken().type == TokenType::LeftCurlyBracket) {
        forBody = parseCurlyBracketBlock();
    } else {
        forBody.emplace_back(parseExpr());
    }
    return std::make_unique<ForLoopNode>(
        std::move(initExpr),
        std::move(nextExpr),
        std::move(condition),
        std::move(forBody)
    );
}

std::unique_ptr<BaseNode> Parser::parseIfStatement() {
    auto ifBranch = parseCondBranch();
    std::vector<IfStatement::CondBranch> elseIfBranches;
    std::optional<std::vector<std::unique_ptr<BaseNode>>> elseBranch;
    while (lexer->currToken().type == TokenType::Else) {
        lexer->nextToken(); // else
        if (lexer->currToken().type != TokenType::If) {
            if (lexer->currToken().type == TokenType::LeftCurlyBracket) {
                elseBranch = parseCurlyBracketBlock();
            } else {
                elseBranch = std::vector<std::unique_ptr<BaseNode>>();
                elseBranch.value().emplace_back(parseExpr());
            }
            break;
        }
        if (lexer->currToken().type != TokenType::If) {
            throw std::runtime_error("If condition does not exist");
        }
        lexer->nextToken(); // if
        elseIfBranches.emplace_back(parseCondBranch());
    }
    return std::make_unique<IfStatement>(std::move(ifBranch), std::move(elseIfBranches), std::move(elseBranch));
}

std::vector<std::unique_ptr<BaseNode>> Parser::parseCurlyBracketBlock() {
    std::vector<std::unique_ptr<BaseNode>> body;
    if (lexer->currToken().type != TokenType::LeftCurlyBracket) {
        throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
    }
    lexer->nextToken(); // {
    while (lexer->currToken().type != TokenType::RightCurlyBracket) {
        if (auto node = parseNextNode(); node != nullptr) {
            body.emplace_back(std::move(node));
        }
    }
    lexer->nextToken(); // }
    return body;
}

std::unique_ptr<ExpressionNode> Parser::parseExpr() {
    return parseTerm();
}

std::unique_ptr<ExpressionNode> Parser::parsePrimary() {
    if (lexer->currToken().type == TokenType::LeftParenthesis) {
        lexer->nextToken();
        auto expr = parseExpr();
        lexer->nextToken();
        return expr;
    }
    if (lexer->currToken().type == TokenType::Number
        || (isSign(lexer->currToken().type) && lexer->peekToken().type == TokenType::Number)) {
        auto value = parseNumber();
        return value;
    }
    if (lexer->currToken().type == TokenType::Identifier) {
        auto ident = parseIdent();
        if (lexer->currToken().type == TokenType::DecrementOperator
            || lexer->currToken().type == TokenType::IncrementOperator) {
            const auto type = lexer->currToken().type;
            lexer->nextToken();
            return std::make_unique<UnaryOpNode>(type,
                                                 UnaryOpNode::UnaryOpType::Postfix,
                                                 std::move(ident));
        }
        if (lexer->currToken().type == TokenType::LeftParenthesis) {
            return parseFunctionCall(std::move(ident));
        }
        return ident;
    }
    if (lexer->currToken().type == TokenType::DecrementOperator
        || lexer->currToken().type == TokenType::IncrementOperator
        || lexer->currToken().type == TokenType::LogicalNegation
        || lexer->currToken().type == TokenType::Plus
        || lexer->currToken().type == TokenType::Minus) {
        const auto type = lexer->currToken().type;
        lexer->nextToken();
        auto val = parsePrimary();
        return std::make_unique<UnaryOpNode>(type,
                                             UnaryOpNode::UnaryOpType::Prefix,
                                             std::move(val));
    }
    throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
}

std::unique_ptr<ExpressionNode> Parser::parseFactor() {
    auto lhs = parsePrimary();
    while (lexer->currToken().type == TokenType::Star
           || lexer->currToken().type == TokenType::Slash
           || lexer->currToken().type == TokenType::LeftAngleBracket
           || lexer->currToken().type == TokenType::LeftAngleBracketEqual
           || lexer->currToken().type == TokenType::RightAngleBracket
           || lexer->currToken().type == TokenType::RightAngleBracketEqual
           || lexer->currToken().type == TokenType::Equal
           || lexer->currToken().type == TokenType::NotEqual) {
        const auto op = lexer->currToken().type;
        lexer->nextToken();
        auto rhs = parsePrimary();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<ExpressionNode> Parser::parseTerm() {
    auto lhs = parseFactor();
    while (lexer->currToken().type == TokenType::Plus
           || lexer->currToken().type == TokenType::Minus) {
        const auto op = lexer->currToken().type;
        lexer->nextToken();
        auto rhs = parseFactor();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), std::move(rhs));
    }

    return lhs;
}

std::unique_ptr<IdentNode> Parser::parseIdent() const {
    auto ident = std::make_unique<IdentNode>(lexer->currToken().value.value_or(""));
    lexer->nextToken();
    return ident;
}

std::unique_ptr<StatementNode> Parser::parseAssignment(std::string identName) {
    return std::make_unique<AssignmentNode>(std::move(identName), parseExpr());
}

std::unique_ptr<NumberNode> Parser::parseNumber() const {
    auto sign = 1;
    if (isSign(lexer->currToken().type)) {
        sign = lexer->currToken().type == TokenType::Minus ? -1 : 1;
        lexer->nextToken();
    }
    if (!lexer->currToken().value.has_value()) {
        throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
    }
    auto number = std::make_unique<NumberNode>(sign * strtod(lexer->currToken().value.value().c_str(), nullptr));
    lexer->nextToken();
    return number;
}

std::unique_ptr<ExpressionNode> Parser::parseFunctionCall(std::unique_ptr<IdentNode> ident) {
    if (lexer->currToken().type != TokenType::LeftParenthesis) {
        throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
    }
    lexer->nextToken(); // '('
    std::vector<std::unique_ptr<ExpressionNode>> args;
    do {
        if (lexer->currToken().type == TokenType::Comma) {
            lexer->nextToken();
        }
        if (lexer->currToken().type != TokenType::RightParenthesis) {
            args.emplace_back(parseExpr());
        }
    } while (lexer->currToken().type == TokenType::Comma);

    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
    }
    lexer->nextToken(); // ')'
    return std::make_unique<FunctionCallNode>(std::move(ident), std::move(args));
}

std::unique_ptr<StatementNode> Parser::parseFunctionDef() {
    if (lexer->currToken().type != TokenType::Identifier) {
        throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
    }
    auto fnName = parseIdent();
    lexer->nextToken();
    std::vector<std::unique_ptr<IdentNode>> params;
    while (lexer->currToken().type != TokenType::RightParenthesis) {
        params.emplace_back(parseIdent());
        if (lexer->currToken().type != TokenType::Comma && lexer->currToken().type != TokenType::RightParenthesis) {
            throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
        }
        if (lexer->currToken().type == TokenType::Comma) {
            lexer->nextToken();
        }
    }
    lexer->nextToken(); // {
    return std::make_unique<FunctionNode>(std::move(fnName),
                                          std::move(params),
                                          parseCurlyBracketBlock());
}
