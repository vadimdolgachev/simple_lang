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
    If = 0,
    Else = 1,
    ForLoop = 2,
    WhileLoop = 3,
    DoLoop = 4,
    FunctionDefinition = 5,
    Return = 6,

    // Identifiers and literals
    Identifier = 7,
    Number = 8,
    String = 9,
    Boolean = 10,

    // Operators
    PlusPlus = 11,            // ++
    Increment = PlusPlus,
    MinusMinus = 12,          // --
    Decrement = MinusMinus,
    Plus = 13,                // +
    Minus = 14,               // -
    Star = 15,                // *
    Multiply = Star,
    Slash = 16,               // /
    Divide = Slash,
    Assignment = 17,          // =

    // Comparison and relational operators
    LeftAngleBracket = 18,         // <
    Less = LeftAngleBracket,
    LeftAngleBracketEqual = 19,    // <=
    LessEqual = LeftAngleBracketEqual,
    RightAngleBracket = 20,        // >
    Greater = RightAngleBracket,
    RightAngleBracketEqual = 21,   // >=
    GreaterEqual = RightAngleBracketEqual,
    Equal = 22,                    // ==
    NotEqual = 23,                 // !=

    // Logical operators
    LogicalAnd = 24,       // &&
    LogicalOr = 25,        // ||
    LogicalNegation = 26,  // !

    // Bitwise operators
    BitwiseAnd = 27,   // &
    BitwiseOr = 28,    // |
    BitwiseXor = 29,   // ^
    BitwiseNot = 30,   // ~

    // Punctuation
    Comma = 31,              // ,
    Dot = 32,                // .
    Colon = 33,              // :
    Semicolon = 34,          // ;
    LeftParenthesis = 35,    // (
    RightParenthesis = 36,   // )
    LeftCurlyBracket = 37,   // {
    RightCurlyBracket = 38,  // }
    LeftSquareBracket = 39,  // [
    RightSquareBracket = 40, // ]
    Question = 41,           // ?

    // Special
    Comment = 42,   // Comment
    Eos = 43,       // End of Stream
    Unknown = 44    // Unknown token
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
        case TokenType::Increment: {
            return "Increment";
        }
        case TokenType::Decrement: {
            return "Decrement";
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
        case TokenType::Comment: {
            return "Comment";
        }
        default: {
            return "Invalid";
        }
    }
}

struct Token final {
    TokenType type = TokenType::Unknown;
    std::optional<std::string> value;
    int32_t startPosition = 0;
    int32_t endPosition = 0;

    [[nodiscard]] std::string toString() const {
        return value
                   ? std::format("type: {}, value: {}", ::toString(type), *value)
                   : std::format("type: {}", ::toString(type));
    }
};

class Lexer {
public:
    struct Character final {
        char val;
        int32_t pos;
        int32_t lineNumber;
    };

    explicit Lexer(std::unique_ptr<std::istream> stream);

    Token nextToken();

    Token prevToken();

    Token peekToken();

    [[nodiscard]] Token currToken() const;

    [[nodiscard]] bool hasNextToken() const;

    [[nodiscard]] std::deque<Character> readText() const;

private:
    void pushToken(Token token);

    void readNextChar();

    [[nodiscard]] Token fetchNextToken();

    [[nodiscard]] int getPeekChar() const;

    [[nodiscard]] std::string parseNumber();
    [[nodiscard]] std::string parseComment();

    std::unique_ptr<std::istream> stream;
    Character currChar = {EOF, 0, 0};
    std::deque<Token> tokenQueue;
    std::deque<Character> textQueue;
    size_t currTokIndex = 0;
};


#endif //LEXER_H
