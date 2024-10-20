//
// Created by vadim on 20.10.24.
//

#include "Lexer.h"

void Lexer::readNextChar() {
    do {
        lastChar = stream->get();
        if (lastChar == '\n' || !stream->eof()) {
            break;
        }
    } while (hasNextToken());
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

void Lexer::parseNumber() {
    numberValue.clear();
    do {
        if (isspace(lastChar)) {
            if (ispunct(getPeekChar())) {
                break;
            }
            readNextChar();
            continue;
        }

        if ((isSignOfNumber(lastChar) && numberValue.empty()) || isCharOfNumber(lastChar)) {
            numberValue.push_back(static_cast<char>(lastChar));
            // last symbol of number
            if (ispunct(getPeekChar()) && getPeekChar() != '.') {
                break;
            }
            readNextChar();
        } else {
            break;
        }
    } while (hasNextToken());
}

Lexer::Lexer(std::unique_ptr<std::istream> stream):
    stream(std::move(stream)),
    lastChar(' '),
    currentToken(TokenType::UnknownToken) {
}

TokenType Lexer::readNextToken(const bool inExpression) {
    do {
        readNextChar();
    } while (std::isspace(lastChar));

    if (lastChar == ';' && !inExpression) {
        do {
            readNextChar();
        } while (isspace(lastChar) && lastChar != '\n');
    }

    if (lastChar == EOF) {
        currentToken = TokenType::EosToken;
        return currentToken;
    }

    currentToken = TokenType::UnknownToken;
    // parse number
    if ((isSignOfNumber(lastChar) && !inExpression) || isCharOfNumber(lastChar)) {
        currentToken = TokenType::NumberToken;
        parseNumber();
    } else if (isSignOfNumber(lastChar)) {
        if (const int peek = getPeekChar(); peek == lastChar) {
            while (!isalnum(getPeekChar())) {
                readNextChar();
            }
            if (lastChar == '+') {
                currentToken = TokenType::IncrementOperatorToken;
            } else {
                currentToken = TokenType::DecrementOperatorToken;
            }
        } else if (lastChar == '+') {
            currentToken = TokenType::PlusToken;
        } else if (lastChar == '-') {
            currentToken = TokenType::MinusToken;
        }
    } else if (lastChar == '(') {
        currentToken = TokenType::LeftParenthesisToken;
    } else if (lastChar == ')') {
        currentToken = TokenType::RightParenthesisToken;
    } else if (lastChar == '{') {
        currentToken = TokenType::LeftCurlyBracketToken;
    } else if (lastChar == '}') {
        currentToken = TokenType::RightCurlyBracketToken;
    } else if (lastChar == ',') {
        currentToken = TokenType::CommaToken;
    } else if (lastChar == '=') {
        currentToken = TokenType::EqualsToken;
    } else if (lastChar == '+') {
        currentToken = TokenType::PlusToken;
    } else if (lastChar == '-') {
        currentToken = TokenType::MinusToken;
    } else if (lastChar == '*') {
        currentToken = TokenType::MultiplyToken;
    } else if (lastChar == '/') {
        currentToken = TokenType::DivideToken;
    } else if (lastChar == '<') {
        currentToken = TokenType::LeftAngleBracketToken;
    } else if (lastChar == '>') {
        currentToken = TokenType::RightAngleBracketToken;
    } else {
        // parse identifiers
        if (std::isalpha(lastChar)) {
            identifier.clear();
            while (std::isalnum(lastChar)) {
                identifier.push_back(static_cast<char>(lastChar));
                if (const int peekChar = getPeekChar(); !isalnum(peekChar)) {
                    break;
                }
                readNextChar();
            }
            if (identifier == "def") {
                currentToken = TokenType::FunctionDefinitionToken;
            } else if (identifier == "if") {
                currentToken = TokenType::IfToken;
            } else if (identifier == "else") {
                currentToken = TokenType::ElseToken;
            } else if (identifier == "for") {
                currentToken = TokenType::ForLoopToken;
            } else {
                currentToken = TokenType::IdentifierToken;
            }
        }
    }
    return currentToken;
}

std::string Lexer::getIdentifier() const {
    return identifier;
}

TokenType Lexer::getCurrentToken() const {
    return currentToken;
}

bool Lexer::hasNextToken() const {
    return !stream->fail();
}

std::string Lexer::getNumberValue() const {
    return numberValue;
}
