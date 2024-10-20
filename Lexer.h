//
// Created by vadim on 20.10.24.
//

#ifndef LEXER_H
#define LEXER_H

#include <cstdint>
#include <memory>
#include <istream>

enum class TokenType : std::uint8_t {
    EosToken,
    NumberToken,
    FunctionDefinitionToken,
    IdentifierToken,
    IfToken,
    ElseToken,
    ForLoopToken,
    IncrementOperatorToken,
    DecrementOperatorToken,
    LeftParenthesisToken,
    RightParenthesisToken,
    LeftCurlyBracketToken,
    RightCurlyBracketToken,
    LeftAngleBracketToken,
    RightAngleBracketToken,
    CommaToken,
    EqualsToken,
    PlusToken,
    MinusToken,
    MultiplyToken,
    DivideToken,
    UnknownToken,
};

class Lexer {
public:
    explicit Lexer(std::unique_ptr<std::istream> stream);

    TokenType readNextToken(bool inExpression = false);

    [[nodiscard]] std::string getIdentifier() const;

    [[nodiscard]] TokenType getCurrentToken() const;

    [[nodiscard]] bool hasNextToken() const;

    [[nodiscard]] std::string getNumberValue() const;

private:
    void readNextChar();

    [[nodiscard]] int getPeekChar() const;

    static bool isSignOfNumber(int ch);

    static bool isCharOfNumber(int ch);

    void parseNumber();

    std::unique_ptr<std::istream> stream;
    int lastChar;
    TokenType currentToken;
    std::string numberValue;
    std::string identifier;
};


#endif //LEXER_H
