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

    [[nodiscard]] BaseNodePtr nextNode();

private:
    [[nodiscard]] ExprNodePtr parseExpr();
    [[nodiscard]] ExprNodePtr parseComparisonExpr();
    [[nodiscard]] ExprNodePtr parseAdditiveExpr();
    [[nodiscard]] ExprNodePtr parseBoolLogic();
    [[nodiscard]] ExprNodePtr parseFactor();
    [[nodiscard]] ExprNodePtr parseTerm();
    [[nodiscard]] ExprNodePtr tryParseUnaryOp();
    [[nodiscard]] ExprNodePtr tryParsePrefixOp();
    [[nodiscard]] ExprNodePtr tryParseIdentifier();
    [[nodiscard]] NodePtr<BooleanNode> parseBoolean() const;
    [[nodiscard]] ExprNodePtr tryParseLiteral() const;
    [[nodiscard]] NodePtr<IdentNode> parseIdent() const;
    [[nodiscard]] NodePtr<NumberNode> parseNumber() const;
    [[nodiscard]] NodePtr<StringNode> parseString() const;
    [[nodiscard]] ExprNodePtr parseFunctionCall(NodePtr<IdentNode> ident);
    [[nodiscard]] StmtNodePtr parseFunctionDef();
    [[nodiscard]] StmtNodePtr parseIfStatement();
    [[nodiscard]] NodePtr<BlockNode> parseCurlyBracketBlock();
    [[nodiscard]] CondBranch parseCondBranch();
    [[nodiscard]] NodePtr<AssignmentNode> tryParseAssignment();
    [[nodiscard]] StmtNodePtr parseForStatement();
    [[nodiscard]] StmtNodePtr parseWhileStatement();
    [[nodiscard]] StmtNodePtr parseDoWhileStatement();
    [[nodiscard]] NodePtr<BlockNode> parseBlock();
    [[nodiscard]] NodePtr<DeclarationNode> parseDeclarationNode(bool needConsumeSemicolon, bool isLocalScope);
    [[nodiscard]] StmtNodePtr parseReturnStatement();
    [[nodiscard]] ExprNodePtr parseTernaryOperator(ExprNodePtr cond);
    void consumeSemicolon() const;
    [[nodiscard]] std::string makeErrorMsg(const std::string &msg) const;
    [[nodiscard]] ExprNodePtr parseObjectMember(ExprNodePtr object);

    std::unique_ptr<Lexer> lexer;
    bool isLocalScope = false;
};

#endif // PARSER_H
