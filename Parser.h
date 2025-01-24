#ifndef PARSER_H
#define PARSER_H

#include <memory>

#include "Lexer.h"
#include "ast/BaseNode.h"
#include "ast/BinOpNode.h"
#include "ast/NumberNode.h"

class Parser {
public:
    explicit Parser(std::unique_ptr<Lexer> lexer);

    explicit operator bool() const;

    std::unique_ptr<BaseNode> parseNextNode();

private:
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseExpr();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parsePrimary();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseFactor();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseTerm();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseIdent() const;
    [[nodiscard]] std::unique_ptr<BaseNode> parseDefinition(std::string identName);
    [[nodiscard]] std::unique_ptr<NumberNode> parseNumber() const;

    std::unique_ptr<Lexer> lexer;
};

#endif // PARSER_H
