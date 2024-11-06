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

    std::unique_ptr<ExpressionNode> parseExpr(int operatorPrecedence = 0);

    [[nodiscard]] std::unique_ptr<NumberNode> parseNumberExpr() const;

    std::unique_ptr<BinOpNode> parseBinExpr(std::unique_ptr<ExpressionNode> lhsOp);

  private:
    std::unique_ptr<Lexer> lexer;
};

#endif // PARSER_H
