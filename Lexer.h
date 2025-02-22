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
    WhileLoop,
    DoLoop,
    FunctionDefinition,
    Return,

    // Identifiers and literals
    Identifier,
    Number,
    String,
    Boolean,

    // Operators
    IncrementOperator,   // ++
    DecrementOperator,   // --
    Plus,                // +
    Minus,               // -
    Star,                // *
    Slash,               // /
    Assignment,          // =

    // Comparison and relational operators
    LeftAngleBracket,         // <
    LeftAngleBracketEqual,    // <=
    RightAngleBracket,        // >
    RightAngleBracketEqual,   // >=
    Equal,                    // ==
    NotEqual,                 // !=

    // Logical operators
    LogicalAnd,       // &&
    LogicalOr,        // ||
    LogicalNegation,  // !

    // Bitwise operators
    BitwiseAnd,   // &
    BitwiseOr,    // |
    BitwiseXor,   // ^
    BitwiseNot,   // ~

    // Punctuation
    Comma,              // ,
    Semicolon,          // ;
    LeftParenthesis,    // (
    RightParenthesis,   // )
    LeftCurlyBracket,   // {
    RightCurlyBracket,  // }
    LeftSquareBracket,  // [
    RightSquareBracket, // ]

    // Special
    Eos,       // End of Stream
    Unknown    // Unknown token
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
        case TokenType::WhileLoop: {
            return "WhileLoop";
        }
        case TokenType::DoLoop: {
            return "DoLoop";
        }
        case TokenType::FunctionDefinition: {
            return "FunctionDefinition";
        }
        case TokenType::Return: {
            return "Return";
        }
        case TokenType::Identifier: {
            return "Identifier";
        }
        case TokenType::Number: {
            return "Number";
        }
        case TokenType::String: {
            return "String";
        }
        case TokenType::Boolean: {
            return "Boolean";
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
        case TokenType::Assignment: {
            return "Equals";
        }
        case TokenType::LeftAngleBracket: {
            return "LeftAngleBracket";
        }
        case TokenType::LeftAngleBracketEqual: {
            return "LeftAngleBracketEqual";
        }
        case TokenType::RightAngleBracket: {
            return "RightAngleBracket";
        }
        case TokenType::RightAngleBracketEqual: {
            return "RightAngleBracketEqual";
        }
        case TokenType::Equal: {
            return "Equal";
        }
        case TokenType::NotEqual: {
            return "NotEqual";
        }
        case TokenType::LogicalAnd: {
            return "LogicalAnd";
        }
        case TokenType::LogicalOr: {
            return "LogicalOr";
        }
        case TokenType::LogicalNegation: {
            return "LogicalNegation";
        }
        case TokenType::BitwiseAnd: {
            return "BitwiseAnd";
        }
        case TokenType::BitwiseOr: {
            return "BitwiseOr";
        }
        case TokenType::BitwiseXor: {
            return "BitwiseXor";
        }
        case TokenType::BitwiseNot: {
            return "BitwiseNot";
        }
        case TokenType::Comma: {
            return "Comma";
        }
        case TokenType::Semicolon: {
            return "Semicolon";
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
        case TokenType::LeftSquareBracket: {
            return "LeftSquareBracket";
        }
        case TokenType::RightSquareBracket: {
            return "RightSquareBracket";
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

    std::unique_ptr<std::istream> stream;
    int lastChar;
    std::deque<Token> tokenQueue;
    size_t currTokIndex = 0;
};


#endif //LEXER_H
