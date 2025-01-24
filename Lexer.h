//
// Created by vadim on 20.10.24.
//

#ifndef LEXER_H
#define LEXER_H

#include <cstdint>
#include <deque>
#include <memory>
#include <istream>
#include <optional>

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
    StarToken,
    SlashToken,
    UnknownToken,
};

struct Token final {
    TokenType type = TokenType::UnknownToken;
    std::optional<std::string> value;
};

class Lexer {
public:
    explicit Lexer(std::unique_ptr<std::istream> stream);

    Token nextToken(bool inExpression = false);

    Token prevToken();

    Token peekToken(bool inExpression);

    [[nodiscard]] Token currToken() const;

    [[nodiscard]] bool hasNextToken() const;

    [[nodiscard]] static bool isArithmeticOp(TokenType token);

private:
    void pushToken(Token token);

    void readNextChar();

    [[nodiscard]] Token fetchNextToken(bool inExpression = false);

    [[nodiscard]] int getPeekChar() const;

    static bool isSignOfNumber(int ch);

    static bool isCharOfNumber(int ch);

    [[nodiscard]] std::string parseNumber();

    std::optional<TokenType> maybeParseUnaryToken();

    std::unique_ptr<std::istream> stream;
    int lastChar;
    std::deque<Token> tokenQueue;
    size_t currTokIndex = 0;
};


#endif //LEXER_H
