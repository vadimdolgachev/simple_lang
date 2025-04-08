#ifndef PARSER_H
#define PARSER_H

#include <memory>

#include "Lexer.h"
#include "ast/BaseNode.h"
#include "ast/CondBranch.h"
#include "ast/DeclarationNode.h"

class Parser {
public:
    explicit Parser(std::unique_ptr<Lexer> lexer);

    [[nodiscard]] bool hasNextNode() const;

    [[nodiscard]] std::unique_ptr<BaseNode> nextNode();

private:
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseExpr();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseComparisonExpr();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseAdditiveExpr();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseBoolLogic();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseFactor();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseTerm();
    [[nodiscard]] std::unique_ptr<ExpressionNode> tryParseUnaryOp();
    [[nodiscard]] std::unique_ptr<ExpressionNode> tryParsePrefixOp();
    [[nodiscard]] std::unique_ptr<ExpressionNode> tryParseIdentifier();
    [[nodiscard]] std::unique_ptr<BooleanNode> parseBoolean() const;
    [[nodiscard]] std::unique_ptr<ExpressionNode> tryParseLiteral() const;
    [[nodiscard]] std::unique_ptr<IdentNode> parseIdent() const;
    [[nodiscard]] std::unique_ptr<NumberNode> parseNumber() const;
    [[nodiscard]] std::unique_ptr<StringNode> parseString() const;
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseFunctionCall(std::unique_ptr<IdentNode> ident);
    [[nodiscard]] std::unique_ptr<StatementNode> parseFunctionDef();
    [[nodiscard]] std::unique_ptr<StatementNode> parseIfStatement();
    [[nodiscard]] std::unique_ptr<BlockNode> parseCurlyBracketBlock();
    [[nodiscard]] CondBranch parseCondBranch();
    [[nodiscard]] std::unique_ptr<AssignmentNode> tryParseAssignment();
    [[nodiscard]] std::unique_ptr<StatementNode> parseForStatement();
    [[nodiscard]] std::unique_ptr<StatementNode> parseWhileStatement();
    [[nodiscard]] std::unique_ptr<StatementNode> parseDoWhileStatement();
    [[nodiscard]] std::unique_ptr<BlockNode> parseBlock();
    [[nodiscard]] std::unique_ptr<DeclarationNode> parseDeclarationNode(bool needConsumeSemicolon, bool globalScope = false);
    [[nodiscard]] std::unique_ptr<StatementNode> parseReturnStatement();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseTernaryOperator(std::unique_ptr<ExpressionNode> cond);
    void consumeSemicolon() const;
    [[nodiscard]] std::string makeErrorMsg(const std::string &msg) const;
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseObjectMember(std::unique_ptr<ExpressionNode> object);

    std::unique_ptr<Lexer> lexer;
};

#endif // PARSER_H
