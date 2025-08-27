//
// Created by vadim on 20.10.24.
//

#include "Lexer.h"

#include <algorithm>
#include <iostream>
#include <unordered_map>

struct HashArray {
    size_t operator()(const std::array<char, 2> &arr) const noexcept {
        constexpr auto prime = 31;
        return (arr[0] * prime) + arr[1];
    }
};

namespace {
    constexpr uint32_t MAX_READ_LINES = 2;
    const auto isLineSepPred = [](const auto &v) {
        return v.val == '\n';
    };
    const std::unordered_map<std::string, TokenType> KEYWORDS = {
            {"fn", TokenType::FunctionDefinition},
            {"if", TokenType::If},
            {"else", TokenType::Else},
            {"for", TokenType::ForLoop},
            {"while", TokenType::WhileLoop},
            {"do", TokenType::DoLoop},
            {"true", TokenType::Boolean},
            {"false", TokenType::Boolean},
            {"return", TokenType::Return},
            {"struct", TokenType::Struct}
    };
    const std::unordered_map<char, TokenType> SINGLE_TOKENS = {
            {'(', TokenType::LeftParenthesis},
            {')', TokenType::RightParenthesis},
            {'{', TokenType::LeftCurlyBracket},
            {'}', TokenType::RightCurlyBracket},
            {'[', TokenType::LeftSquareBracket},
            {']', TokenType::RightSquareBracket},
            {',', TokenType::Comma},
            {'.', TokenType::Dot},
            {'?', TokenType::Question},
            {'*', TokenType::Star},
            {'/', TokenType::Slash},
            {';', TokenType::Semicolon},
            {':', TokenType::Colon},
            {'|', TokenType::BitwiseOr},
            {'&', TokenType::BitwiseAnd},
            {'>', TokenType::RightAngleBracket},
            {'<', TokenType::LeftAngleBracket},
            {'!', TokenType::LogicalNegation},
            {'-', TokenType::Minus},
            {'+', TokenType::Plus},
            {'=', TokenType::Assignment},
    };
    const std::unordered_map<std::array<char, 2>, TokenType, HashArray> DOUBLE_TOKENS = {
            {{'=', '='}, TokenType::Equal},
            {{'!', '='}, TokenType::NotEqual},
            {{'>', '='}, TokenType::RightAngleBracketEqual},
            {{'<', '='}, TokenType::LeftAngleBracketEqual},
            {{'+', '+'}, TokenType::PlusPlus},
            {{'-', '-'}, TokenType::MinusMinus},
            {{'|', '|'}, TokenType::LogicalOr},
            {{'&', '&'}, TokenType::LogicalAnd},
            {{'/', '/'}, TokenType::Comment},
    };
} // namespace

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
    if (currTokIndex + 1 < tokenQueue.size()) {
        currTokIndex += 1;
        return currToken();
    }

    do {
        readNextChar();
    } while (std::isspace(currChar.val));

    const auto startTokenPosition = currChar.pos;
    auto resultToken = TokenType::Unknown;

    std::string tokenValue(1, currChar.val);
    if (currChar.val == EOF) {
        resultToken = TokenType::Eos;
    } else if (std::isdigit(currChar.val) || (currChar.val == '.' && std::isdigit(getPeekChar()))) {
        // parse number
        resultToken = TokenType::Number;
        tokenValue = parseNumber();
    } else if (const auto it = DOUBLE_TOKENS.find({currChar.val, static_cast<char>(getPeekChar())});
        it != DOUBLE_TOKENS.end()) {
        resultToken = it->second;
        readNextChar();
    } else if (const auto res = SINGLE_TOKENS.find(currChar.val); res != SINGLE_TOKENS.end()) {
        resultToken = res->second;
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
                tokenValue.push_back(currChar.val);
                if (const int peekChar = getPeekChar(); !isAllowChar(peekChar)) {
                    break;
                }
                readNextChar();
            }
            if (isStringLiteral && currChar.val != '"') {
                readNextChar();
            }
            if (const auto it = KEYWORDS.find(tokenValue); it != KEYWORDS.end()) {
                resultToken = it->second;
            } else if (isStringLiteral) {
                resultToken = TokenType::String;
            } else {
                resultToken = TokenType::Identifier;
            }
        }
    }

    if (resultToken == TokenType::Comment) {
        tokenValue = parseComment();
    }

    pushToken({resultToken, tokenValue, startTokenPosition - 1, currChar.pos - 1});

    return currToken();
}

int Lexer::getPeekChar() const {
    return stream->peek();
}

std::string Lexer::parseNumber() {
    std::string tokenValue;
    do {
        if (std::isdigit(currChar.val) || currChar.val == '.') {
            tokenValue.push_back(currChar.val);
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

std::string Lexer::parseComment() {
    readNextChar();
    std::string comment;
    const auto isAllowChar = [](const int c) {
        return c != '\n' && c != EOF;
    };
    while (isAllowChar(currChar.val)) {
        comment.push_back(currChar.val);
        readNextChar();
    }
    return comment;
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
