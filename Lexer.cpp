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
        resultToken = TokenType::Equals;
    } else if (lastChar == '+') {
        resultToken = TokenType::Plus;
        if (const auto token = maybeParseUnaryToken()) {
            resultToken = *token;
        }
    } else if (lastChar == '-') {
        resultToken = TokenType::Minus;
        if (const auto token = maybeParseUnaryToken()) {
            resultToken = *token;
        }
    } else if (lastChar == '*') {
        resultToken = TokenType::Star;
    } else if (lastChar == '/') {
        resultToken = TokenType::Slash;
    } else if (lastChar == '<') {
        resultToken = TokenType::LeftAngleBracket;
    } else if (lastChar == '>') {
        resultToken = TokenType::RightAngleBracket;
    } else {
        // parse identifiers
        if (std::isalpha(lastChar)) {
            tokenValue = std::string();
            while (std::isalnum(lastChar)) {
                tokenValue.value().push_back(static_cast<char>(lastChar));
                if (const int peekChar = getPeekChar(); !isalnum(peekChar)) {
                    break;
                }
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

std::optional<TokenType> Lexer::maybeParseUnaryToken() {
    if (const int peek = getPeekChar(); peek == lastChar) {
        readNextChar();
        return lastChar == '+' ? TokenType::IncrementOperator : TokenType::DecrementOperator;
    }
    return std::nullopt;
}

Lexer::Lexer(std::unique_ptr<std::istream> stream):
    stream(std::move(stream)),
    lastChar(' ') {
}

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
