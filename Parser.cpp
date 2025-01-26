#include "Parser.h"

#include <complex>
#include <iostream>

#include "ast/FunctionCallNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/IdentNode.h"
#include "ast/VariableDefinitionStatement.h"

/*
<Declaration> ::= <Identifier> <Initialization>
               | <FunctionDefinition>

<Identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<Initialization> ::= "=" <Expression>
                 | ε

<Parameters> ::= <Parameter> "," <Parameters>
             | <Parameter>
             | ε

<Parameter> ::= <Identifier>

<Block> ::= "{" <Declarations> "}"

<Declarations> ::= <Declaration> | <Declaration> <Declarations>

<Assignment> ::= <Identifier> "=" <Expression> ";"

<Expression> ::= <Literal>
             | <Identifier>
             | <FunctionCall>
             | <Expression> <AddOp> <Term>
             | <Expression> "++"
             | <Expression> "--"
             | <Term>

<FunctionCall> ::= <Identifier> "(" <Arguments> ")"

<Arguments> ::= <Expression> "," <Arguments>
             | <Expression>
             | ε

<AddOp> ::= "+" | "-"

<Literal> ::= <Integer>

<Integer> ::= [0-9]+

<Term> ::= <Factor>
        | <Term> <MulOp> <Factor>

<MulOp> ::= "*" | "/"

<Factor> ::= "(" <Expression> ")"
         | <Identifier>
         | <Number>
         | <FunctionCall>
         | <PrefixIncDec> <Identifier>
         | <Identifier> <PostfixIncDec>

<PrefixIncDec> ::= "++" | "--"

<PostfixIncDec> ::= "++" | "--"

<Statement> ::= "if" "(" <Expression> ")" <Block> ("else" <Block>)?
             | "while" "(" <Expression> ")" <Block>
             | "for" "(" <ForLoopInit> ";" <Expression> ";" <Expression> ")" <Block>
             | <Expression> ";"
             | <FunctionDefinition>

<FunctionDefinition> ::= "fn" <Identifier> "(" <Parameters> ")" <Block>

<ForLoopInit> ::= <Assignment>
              | <Declaration>
 */

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

Parser::Parser(std::unique_ptr<Lexer> lexer) :
    lexer(std::move(lexer)) {}

Parser::operator bool() const {
    return lexer->hasNextToken();
}

std::unique_ptr<BaseNode> Parser::parseNextNode() {
    const auto [typeType, value] = lexer->nextToken();
    // Assignment
    if (typeType == TokenType::Identifier) {
        lexer->nextToken();
        if (lexer->currToken().type == TokenType::Equals && value.has_value()) {
            lexer->nextToken();
            return parseDefinition(value.value());
        }
        // Rollback
        lexer->prevToken();
    }
    // Binary expr
    if (isSign(typeType)
        || typeType == TokenType::Number
        || typeType == TokenType::LeftParenthesis
        || typeType == TokenType::Identifier
        || typeType == TokenType::IncrementOperator
        || typeType == TokenType::DecrementOperator) {
        return parseExpr();
    }
    return nullptr;
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
        || lexer->currToken().type == TokenType::IncrementOperator) {
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
           || lexer->currToken().type == TokenType::Slash) {
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

    if (isEndOfExpr(lexer->currToken().type) || lexer->currToken().type == TokenType::Comma) {
        return lhs;
    }
    throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
}

std::unique_ptr<IdentNode> Parser::parseIdent() const {
    auto ident = std::make_unique<IdentNode>(lexer->currToken().value.value_or(""));
    lexer->nextToken();
    return ident;
}

std::unique_ptr<BaseNode> Parser::parseDefinition(std::string identName) {
    return std::make_unique<VariableDefinitionStatement>(std::move(identName), parseExpr());
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
        args.emplace_back(parseExpr());
    } while (lexer->currToken().type == TokenType::Comma);

    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error("Unexpected token: " + lexer->currToken().toString());
    }
    lexer->nextToken(); // ')'
    return std::make_unique<FunctionCallNode>(std::move(ident), std::move(args));
}
