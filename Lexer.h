//
// Created by vadim on 20.10.24.
//

#ifndef LEXER_H
#define LEXER_H

#include <cstdint>
#include <deque>
#include <format>
#include <memory>
#include <istream>
#include <optional>

enum class TokenType : std::uint8_t {
    // Keywords
    If,
    Else,
    ForLoop,
    FunctionDefinition,

    // Identifiers and literals
    Identifier,
    Number,

    // Operators
    IncrementOperator,
    DecrementOperator,
    Plus,
    Minus,
    Star,
    Slash,
    Equals,

    // Comparison and relational operators
    LeftAngleBracket,
    RightAngleBracket,

    // Punctuation
    Comma,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,

    // Special
    Eos,
    Unknown,
};

inline std::string toString(const TokenType token) {
    switch (token) {
        case TokenType::If: {
            return "If";
        }
        case TokenType::Else: {
            return "Else";
        }
        case TokenType::ForLoop: {
            return "ForLoop";
        }
        case TokenType::FunctionDefinition: {
            return "FunctionDefinition";
        }
        case TokenType::Identifier: {
            return "Identifier";
        }
        case TokenType::Number: {
            return "Number";
        }
        case TokenType::IncrementOperator: {
            return "IncrementOperator";
        }
        case TokenType::DecrementOperator: {
            return "DecrementOperator";
        }
        case TokenType::Plus: {
            return "Plus";
        }
        case TokenType::Minus: {
            return "Minus";
        }
        case TokenType::Star: {
            return "Star";
        }
        case TokenType::Slash: {
            return "Slash";
        }
        case TokenType::Equals: {
            return "Equals";
        }
        case TokenType::LeftAngleBracket: {
            return "LeftAngleBracket";
        }
        case TokenType::RightAngleBracket: {
            return "RightAngleBracket";
        }
        case TokenType::Comma: {
            return "Comma";
        }
        case TokenType::LeftParenthesis: {
            return "LeftParenthesis";
        }
        case TokenType::RightParenthesis: {
            return "RightParenthesis";
        }
        case TokenType::LeftCurlyBracket: {
            return "LeftCurlyBracket";
        }
        case TokenType::RightCurlyBracket: {
            return "RightCurlyBracket";
        }
        case TokenType::Eos: {
            return "Eos";
        }
        case TokenType::Unknown: {
            return "Unknown";
        }
        default: {
            return "Invalid";
        }
    }
}

struct Token final {
    TokenType type = TokenType::Unknown;
    std::optional<std::string> value;

    [[nodiscard]] std::string toString() const {
        return value
                   ? std::format("type: {}, value: {}", ::toString(type), *value)
                   : std::format("type: {}", ::toString(type));
    }
};

class Lexer {
public:
    explicit Lexer(std::unique_ptr<std::istream> stream);

    Token nextToken();

    Token prevToken();

    Token peekToken();

    [[nodiscard]] Token currToken() const;

    [[nodiscard]] bool hasNextToken() const;

private:
    void pushToken(Token token);

    void readNextChar();

    [[nodiscard]] Token fetchNextToken();

    [[nodiscard]] int getPeekChar() const;

    static bool isCharOfNumber(int ch);

    [[nodiscard]] std::string parseNumber();

    std::optional<TokenType> maybeParseUnaryToken();

    std::unique_ptr<std::istream> stream;
    int lastChar;
    std::deque<Token> tokenQueue;
    size_t currTokIndex = 0;
};


#endif //LEXER_H
