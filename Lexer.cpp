//
// Created by vadim on 20.10.24.
//

#include "Lexer.h"

#include <algorithm>
#include <iostream>

namespace {
    constexpr uint32_t MAX_READ_LINES = 2;
    const auto isLineSepPred = [](const auto &v) {
        return v.val == '\n';
    };
}

void Lexer::readNextChar() {
    do {
        currChar.val = static_cast<char>(stream->get());
        if (!stream->eof()) {
            if (const auto lineCounts = std::ranges::count_if(textQueue, isLineSepPred);
                lineCounts >= MAX_READ_LINES) {
                const auto it = std::ranges::next(std::ranges::find_if(textQueue,
                                                                       isLineSepPred));
                if (it != textQueue.end()) {
                    textQueue.erase(textQueue.begin(), it);
                }
            }
            textQueue.emplace_back(currChar);
            ++currChar.pos;
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
    } while (std::isspace(currChar.val));

    const auto startTokenPosition = currChar.pos;
    auto resultToken = TokenType::Unknown;

    if (currChar.val == ';') {
        resultToken = TokenType::Semicolon;
    } else if (currChar.val == EOF) {
        resultToken = TokenType::Eos;
    } else if (isCharOfNumber(currChar.val)) {
        // parse number
        resultToken = TokenType::Number;
        tokenValue = parseNumber();
    } else if (currChar.val == '(') {
        resultToken = TokenType::LeftParenthesis;
    } else if (currChar.val == ')') {
        resultToken = TokenType::RightParenthesis;
    } else if (currChar.val == '{') {
        resultToken = TokenType::LeftCurlyBracket;
    } else if (currChar.val == '}') {
        resultToken = TokenType::RightCurlyBracket;
    } else if (currChar.val == ',') {
        resultToken = TokenType::Comma;
    } else if (currChar.val == '=') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::Equal;
        } else {
            resultToken = TokenType::Assignment;
        }
    } else if (currChar.val == '+') {
        if (getPeekChar() == '+') {
            readNextChar();
            resultToken = TokenType::IncrementOperator;
        } else {
            resultToken = TokenType::Plus;
        }
    } else if (currChar.val == '-') {
        if (getPeekChar() == '-') {
            readNextChar();
            resultToken = TokenType::DecrementOperator;
        } else {
            resultToken = TokenType::Minus;
        }
    } else if (currChar.val == '*') {
        resultToken = TokenType::Star;
    } else if (currChar.val == '/') {
        resultToken = TokenType::Slash;
    } else if (currChar.val == '<') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::LeftAngleBracketEqual;
        } else {
            resultToken = TokenType::LeftAngleBracket;
        }
    } else if (currChar.val == '>') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::RightAngleBracketEqual;
        } else {
            resultToken = TokenType::RightAngleBracket;
        }
    } else if (currChar.val == '!') {
        if (getPeekChar() == '=') {
            readNextChar();
            resultToken = TokenType::NotEqual;
        } else {
            resultToken = TokenType::LogicalNegation;
        }
    } else if (currChar.val == '&') {
        if (getPeekChar() == '&') {
            readNextChar();
            resultToken = TokenType::LogicalAnd;
        } else {
            resultToken = TokenType::BitwiseAnd;
        }
    } else if (currChar.val == '|') {
        if (getPeekChar() == '|') {
            readNextChar();
            resultToken = TokenType::LogicalOr;
        } else {
            resultToken = TokenType::BitwiseOr;
        }
    } else {
        // parse identifiers
        if (std::isalpha(currChar.val) || currChar.val == '"') {
            const bool isStringLiteral = currChar.val == '"';
            if (isStringLiteral) {
                readNextChar();
            }
            tokenValue = std::string();
            const auto isAllowChar = [isStringLiteral](const int c) {
                return isStringLiteral ? c != '"' : std::isalnum(c);
            };
            while (isAllowChar(currChar.val)) {
                tokenValue.value().push_back(static_cast<char>(currChar.val));
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
            } else if (tokenValue == "true" || tokenValue == "false") {
                resultToken = TokenType::Boolean;
            } else if (isStringLiteral) {
                resultToken = TokenType::String;
            } else {
                resultToken = TokenType::Identifier;
            }
        }
    }

    pushToken({resultToken, tokenValue, startTokenPosition - 1, currChar.pos - 1});

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
        if (isspace(currChar.val)) {
            if (ispunct(getPeekChar())) {
                break;
            }
            readNextChar();
            continue;
        }

        if (isCharOfNumber(currChar.val)) {
            tokenValue.push_back(static_cast<char>(currChar.val));
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
    stream(std::move(stream)) {}

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

std::deque<Lexer::Character> Lexer::readText() const {
    return textQueue;
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
