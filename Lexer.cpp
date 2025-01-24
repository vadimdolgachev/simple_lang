//
// Created by vadim on 20.10.24.
//

#include "Lexer.h"

#include <iostream>

namespace {
    bool isEos(const int v) {
        return v == ';';
    }
}

void Lexer::readNextChar() {
    do {
        lastChar = stream->get();
        if (lastChar == '\n' || !stream->eof()) {
            break;
        }
    } while (hasNextToken());
}

Token Lexer::fetchNextToken(const bool inExpression) {
    std::optional<std::string> tokenValue;

    if (currTokIndex + 1 < tokenQueue.size()) {
        currTokIndex += 1;
        return currToken();
    }

    do {
        readNextChar();
    } while (std::isspace(lastChar));

    auto resultToken = TokenType::UnknownToken;

    if (isEos(lastChar)) {
        while (isEos(getPeekChar())) {
            readNextChar();
        }
        resultToken = TokenType::EosToken;
    } else if (lastChar == EOF) {
        resultToken = TokenType::EosToken;
    } else if (isCharOfNumber(lastChar)) {
        // parse number
        resultToken = TokenType::NumberToken;
        tokenValue = parseNumber();
    } else if (lastChar == '(') {
        resultToken = TokenType::LeftParenthesisToken;
    } else if (lastChar == ')') {
        resultToken = TokenType::RightParenthesisToken;
    } else if (lastChar == '{') {
        resultToken = TokenType::LeftCurlyBracketToken;
    } else if (lastChar == '}') {
        resultToken = TokenType::RightCurlyBracketToken;
    } else if (lastChar == ',') {
        resultToken = TokenType::CommaToken;
    } else if (lastChar == '=') {
        resultToken = TokenType::EqualsToken;
    } else if (lastChar == '+') {
        resultToken = TokenType::PlusToken;
        if (const auto token = maybeParseUnaryToken()) {
            resultToken = *token;
        } else if (isCharOfNumber(getPeekChar()) && !inExpression) {
            resultToken = TokenType::NumberToken;
            tokenValue = parseNumber();
        }
    } else if (lastChar == '-') {
        resultToken = TokenType::MinusToken;
        if (const auto token = maybeParseUnaryToken()) {
            resultToken = *token;
        } else if (isCharOfNumber(getPeekChar()) && !inExpression) {
            resultToken = TokenType::NumberToken;
            tokenValue = parseNumber();
        }
    } else if (lastChar == '*') {
        resultToken = TokenType::StarToken;
    } else if (lastChar == '/') {
        resultToken = TokenType::SlashToken;
    } else if (lastChar == '<') {
        resultToken = TokenType::LeftAngleBracketToken;
    } else if (lastChar == '>') {
        resultToken = TokenType::RightAngleBracketToken;
    } else {
        // parse identifiers
        if (std::isalpha(lastChar)) {
            std::string ident;
            while (std::isalnum(lastChar)) {
                ident.push_back(static_cast<char>(lastChar));
                if (const int peekChar = getPeekChar(); !isalnum(peekChar)) {
                    break;
                }
                readNextChar();
            }
            if (tokenValue == "def") {
                resultToken = TokenType::FunctionDefinitionToken;
            } else if (tokenValue == "if") {
                resultToken = TokenType::IfToken;
            } else if (tokenValue == "else") {
                resultToken = TokenType::ElseToken;
            } else if (tokenValue == "for") {
                resultToken = TokenType::ForLoopToken;
            } else {
                resultToken = TokenType::IdentifierToken;
            }
            tokenValue = ident;
        }
    }

    pushToken({resultToken, tokenValue});

    return currToken();
}

int Lexer::getPeekChar() const {
    return stream->peek();
}

bool Lexer::isSignOfNumber(const int ch) {
    return ch == '+' || ch == '-';
}

bool Lexer::isCharOfNumber(const int ch) {
    return std::isdigit(ch) || ch == '.';
}

std::string Lexer::parseNumber() {
    std::string tokenValue;
    do {
        if (isspace(lastChar)) {
            if (ispunct(getPeekChar())) {
                break;
            }
            readNextChar();
            continue;
        }

        if ((isSignOfNumber(lastChar) && tokenValue.empty()) || isCharOfNumber(lastChar)) {
            tokenValue.push_back(static_cast<char>(lastChar));
            // last symbol of number
            if (ispunct(getPeekChar()) && getPeekChar() != '.') {
                break;
            }
            readNextChar();
        } else {
            break;
        }
    } while (hasNextToken());
    return tokenValue;
}

std::optional<TokenType> Lexer::maybeParseUnaryToken() {
    if (const int peek = getPeekChar(); peek == lastChar) {
        readNextChar();
        if (currToken().type == TokenType::PlusToken) {
            return TokenType::IncrementOperatorToken;
        }
        if (currToken().type == TokenType::MinusToken) {
            return TokenType::DecrementOperatorToken;
        }
    }
    return std::nullopt;
}

Lexer::Lexer(std::unique_ptr<std::istream> stream):
    stream(std::move(stream)),
    lastChar(' ') {
}

Token Lexer::nextToken(const bool inExpression) {
    return fetchNextToken(inExpression);
}

Token Lexer::currToken() const {
    return tokenQueue[currTokIndex];
}

bool Lexer::hasNextToken() const {
    if (!tokenQueue.empty() && currTokIndex < tokenQueue.size() - 1) {
        return true;
    }
    if (!stream->fail()) {
        return true;
    }
    return false;
}

Token Lexer::prevToken() {
    if (tokenQueue.empty()) {
        throw std::out_of_range("Empty token history");
    }
    currTokIndex -= 1;
    return currToken();
}

Token Lexer::peekToken(const bool inExpression) {
    if (tokenQueue.empty()) {
        throw std::out_of_range("Empty token history");
    }
    if (currTokIndex + 1 < tokenQueue.size()) {
        return tokenQueue[currTokIndex + 1];
    }
    const auto token = fetchNextToken(inExpression);
    currTokIndex -= 1;
    return token;
}

bool Lexer::isArithmeticOp(const TokenType token) {
    return token == TokenType::PlusToken
           || token == TokenType::MinusToken
           || token == TokenType::StarToken
           || token == TokenType::SlashToken;
}

void Lexer::pushToken(Token token) {
    if (tokenQueue.size() > 10) {
        tokenQueue.pop_front();
    }
    tokenQueue.emplace_back(std::move(token));
    currTokIndex = tokenQueue.size() - 1;
}
