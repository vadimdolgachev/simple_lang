//
// Created by vadim on 20.10.24.
//

#include "Lexer.h"

#include <iostream>

void Lexer::readNextChar() {
    do {
        lastChar = stream->get();
        if (lastChar == '\n' || !stream->eof()) {
            break;
        }
    } while (hasNextToken());
}

Token Lexer::fetchNextToken() {
    std::optional<std::string> tokenValue;

    if (currTokIndex + 1 < tokenQueue.size()) {
        currTokIndex += 1;
        return currToken();
    }

    do {
        readNextChar();
    } while (std::isspace(lastChar));

    auto resultToken = TokenType::Unknown;

    if (lastChar == ';') {
        resultToken = TokenType::Semicolon;
    } else if (lastChar == EOF) {
        resultToken = TokenType::Eos;
    } else if (isCharOfNumber(lastChar)) {
        // parse number
        resultToken = TokenType::Number;
        tokenValue = parseNumber();
    } else if (lastChar == '(') {
        resultToken = TokenType::LeftParenthesis;
    } else if (lastChar == ')') {
        resultToken = TokenType::RightParenthesis;
    } else if (lastChar == '{') {
        resultToken = TokenType::LeftCurlyBracket;
    } else if (lastChar == '}') {
        resultToken = TokenType::RightCurlyBracket;
    } else if (lastChar == ',') {
        resultToken = TokenType::Comma;
    } else if (lastChar == '=') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::Equal;
        } else {
            resultToken = TokenType::Assignment;
        }
    } else if (lastChar == '+') {
        if (getPeekChar() == '+') {
            readNextChar();
            resultToken = TokenType::IncrementOperator;
        } else {
            resultToken = TokenType::Plus;
        }
    } else if (lastChar == '-') {
        if (getPeekChar() == '-') {
            readNextChar();
            resultToken = TokenType::DecrementOperator;
        } else {
            resultToken = TokenType::Minus;
        }
    } else if (lastChar == '*') {
        resultToken = TokenType::Star;
    } else if (lastChar == '/') {
        resultToken = TokenType::Slash;
    } else if (lastChar == '<') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::LeftAngleBracketEqual;
        } else {
            resultToken = TokenType::LeftAngleBracket;
        }
    } else if (lastChar == '>') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::RightAngleBracketEqual;
        } else {
            resultToken = TokenType::RightAngleBracket;
        }
    } else if (lastChar == '!') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::NotEqual;
        } else {
            resultToken = TokenType::LogicalNegation;
        }
    } else if (lastChar == '&') {
        if (getPeekChar() == '&') {
            readNextChar();
            resultToken = TokenType::LogicalAnd;
        } else {
            resultToken = TokenType::BitwiseAnd;
        }
    } else if (lastChar == '|') {
        if (getPeekChar() == '|') {
            readNextChar();
            resultToken = TokenType::LogicalOr;
        } else {
            resultToken = TokenType::BitwiseOr;
        }
    } else {
        // parse identifiers
        if (std::isalpha(lastChar) || lastChar == '"') {
            const bool isStringLiteral = lastChar == '"';
            if (isStringLiteral) {
                readNextChar();
            }
            tokenValue = std::string();
            const auto isAllowChar = [isStringLiteral](const int c) {
                return isStringLiteral ? c != '"' : std::isalnum(c);
            };
            while (isAllowChar(lastChar)) {
                tokenValue.value().push_back(static_cast<char>(lastChar));
                if (const int peekChar = getPeekChar(); !isAllowChar(peekChar)) {
                    break;
                }
                readNextChar();
            }
            if (isStringLiteral) {
                readNextChar();
            }
            if (tokenValue == "fn") {
                resultToken = TokenType::FunctionDefinition;
            } else if (tokenValue == "if") {
                resultToken = TokenType::If;
            } else if (tokenValue == "else") {
                resultToken = TokenType::Else;
            } else if (tokenValue == "for") {
                resultToken = TokenType::ForLoop;
            } else if (tokenValue == "while") {
                resultToken = TokenType::WhileLoop;
            } else if (tokenValue == "do") {
                resultToken = TokenType::DoLoop;
            } else if (tokenValue == "true") {
                resultToken = TokenType::BooleanTrue;
            } else if (tokenValue == "false") {
                resultToken = TokenType::BooleanFalse;
            } else if (isStringLiteral) {
                resultToken = TokenType::String;
            } else {
                resultToken = TokenType::Identifier;
            }
        }
    }

    pushToken({resultToken, tokenValue});

    return currToken();
}

int Lexer::getPeekChar() const {
    return stream->peek();
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

        if (isCharOfNumber(lastChar)) {
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

Lexer::Lexer(std::unique_ptr<std::istream> stream):
    stream(std::move(stream)),
    lastChar(' ') {}

Token Lexer::nextToken() {
    return fetchNextToken();
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

Token Lexer::peekToken() {
    if (tokenQueue.empty()) {
        throw std::out_of_range("Empty token history");
    }
    if (currTokIndex + 1 < tokenQueue.size()) {
        return tokenQueue[currTokIndex + 1];
    }
    const auto token = fetchNextToken();
    currTokIndex -= 1;
    return token;
}

void Lexer::pushToken(Token token) {
    if (tokenQueue.size() > 10) {
        tokenQueue.pop_front();
    }
    tokenQueue.emplace_back(std::move(token));
    currTokIndex = tokenQueue.size() - 1;
}
