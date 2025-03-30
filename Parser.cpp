#include "Parser.h"

#include <unordered_map>

#include "Util.h"
#include "ast/BinOpNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/IdentNode.h"
#include "ast/AssignmentNode.h"
#include "ast/BooleanNode.h"
#include "ast/IfStatement.h"
#include "ast/StringNode.h"
#include "ast/NumberNode.h"
#include "ast/LoopCondNode.h"
#include "ast/BlockNode.h"
#include "ast/CommentNode.h"
#include "ast/MethodCallNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/ReturnNode.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/TypeNode.h"

namespace {
    bool isSign(const TokenType token) {
        return token == TokenType::Plus || token == TokenType::Minus;
    }

    const std::unordered_map<std::string, TypeKind> TYPES = {
            {"bool", TypeKind::Boolean},
            {"byte", TypeKind::Byte},
            {"char", TypeKind::Char},
            {"double", TypeKind::Double},
            {"int", TypeKind::Integer},
            {"void", TypeKind::Void},
            {"str", TypeKind::Str},
    };
} // namespace

Parser::Parser(std::unique_ptr<Lexer> lexer_) :
    lexer(std::move(lexer_)) {
    lexer->nextToken();
}

bool Parser::hasNextNode() const {
    return lexer->hasNextToken();
}

std::unique_ptr<BaseNode> Parser::nextNode() {
    // Declaration
    if (const auto &token = lexer->currToken(); token.type == TokenType::Identifier) {
        lexer->nextToken();
        if (lexer->currToken().type == TokenType::Colon && token.value.has_value()) {
            lexer->prevToken();
            return std::make_unique<DeclarationNode>(parseDeclarationNode(true));
        }
        lexer->prevToken();
    }
    // Assignment
    if (auto assignment = tryParseAssignment()) {
        return assignment;
    }
    const auto token = lexer->currToken().type;
    const auto value = lexer->currToken().value;
    if (token == TokenType::FunctionDefinition) {
        lexer->nextToken();
        return parseFunctionDef();
    }
    // Statements
    if (token == TokenType::If) {
        lexer->nextToken();
        return parseIfStatement();
    }
    if (token == TokenType::ForLoop) {
        lexer->nextToken();
        return parseForStatement();
    }
    if (token == TokenType::WhileLoop) {
        lexer->nextToken();
        return parseWhileStatement();
    }
    if (token == TokenType::DoLoop) {
        lexer->nextToken();
        return parseDoWhileStatement();
    }
    if (token == TokenType::Return) {
        lexer->nextToken();
        return parseReturnStatement();
    }
    if (token == TokenType::Comment) {
        lexer->nextToken();
        return std::make_unique<CommentNode>(value.value_or(""));
    }
    // Expressions
    auto result = parseExpr();
    consumeSemicolon();
    return result;
}

CondBranch Parser::parseCondBranch() {
    auto condition = parseExpr();
    auto thenBranch = parseBlock();
    return {std::move(condition), std::move(thenBranch)};
}

std::unique_ptr<AssignmentNode> Parser::tryParseAssignment() {
    if (auto token = lexer->currToken(); token.type == TokenType::Identifier) {
        auto ident = parseIdent();
        if (lexer->currToken().type == TokenType::Assignment && token.value.has_value()) {
            lexer->nextToken();
            auto result = std::make_unique<AssignmentNode>(std::move(ident),
                                                           parseExpr());
            consumeSemicolon();
            return result;
        }
        // Rollback
        lexer->prevToken();
    }
    return nullptr;
}

std::unique_ptr<StatementNode> Parser::parseForStatement() {
    if (lexer->currToken().type != TokenType::LeftParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected '(' after 'for'"));
    }
    lexer->nextToken(); // '('
    auto init = std::make_unique<DeclarationNode>(parseDeclarationNode(false));
    if (lexer->currToken().type != TokenType::Semicolon) {
        throw std::runtime_error(makeErrorMsg("Expected ';' after init statement"));
    }
    lexer->nextToken(); // ';'
    auto condition = parseExpr();
    if (lexer->currToken().type != TokenType::Semicolon) {
        throw std::runtime_error(makeErrorMsg("Expected ';' after condition"));
    }
    lexer->nextToken(); // ';'
    auto increment = parseExpr();
    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected ')'"));
    }
    lexer->nextToken(); // ')'
    auto then = parseBlock();
    return std::make_unique<LoopCondNode>(
            LoopCondNode::Type::For,
            CondBranch{std::move(condition), std::move(then)},
            std::move(init),
            std::move(increment));
}

std::unique_ptr<StatementNode> Parser::parseWhileStatement() {
    if (lexer->currToken().type != TokenType::LeftParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected '(' after 'while'"));
    }
    lexer->nextToken(); // '('
    auto condition = parseExpr();
    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected ')' after condition"));
    }
    lexer->nextToken(); // ')'
    auto body = parseBlock();;
    return std::make_unique<LoopCondNode>(LoopCondNode::Type::While,
                                          CondBranch{std::move(condition),
                                                     std::move(body)});
}

std::unique_ptr<StatementNode> Parser::parseDoWhileStatement() {
    if (lexer->currToken().type != TokenType::LeftCurlyBracket) {
        throw std::runtime_error(makeErrorMsg("Expected '{' after 'do'"));
    }
    auto body = parseCurlyBracketBlock();
    if (lexer->currToken().type != TokenType::WhileLoop) {
        throw std::runtime_error(makeErrorMsg("Expected 'while' keyword"));
    }
    lexer->nextToken();
    if (lexer->currToken().type != TokenType::LeftParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected '(' after 'while'"));
    }
    lexer->nextToken(); // '('
    auto condition = parseExpr();
    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected ')' after condition"));
    }
    lexer->nextToken(); // ')'
    consumeSemicolon();
    return std::make_unique<LoopCondNode>(LoopCondNode::Type::DoWhile,
                                          CondBranch{std::move(condition),
                                                     std::move(body)});
}

void Parser::consumeSemicolon() const {
    if (lexer->currToken().type != TokenType::Semicolon) {
        throw std::runtime_error(makeErrorMsg("Expected ';' character"));
    }
    lexer->nextToken();
}

std::unique_ptr<StatementNode> Parser::parseIfStatement() {
    auto ifBranch = parseCondBranch();
    std::vector<CondBranch> elseIfBranches;
    std::optional<std::unique_ptr<BlockNode>> elseBranch;
    while (lexer->currToken().type == TokenType::Else) {
        lexer->nextToken(); // else
        if (lexer->currToken().type != TokenType::If) {
            elseBranch = parseBlock();
            break;
        }
        if (lexer->currToken().type != TokenType::If) {
            throw std::runtime_error(makeErrorMsg("If condition does not exist"));
        }
        lexer->nextToken(); // if
        elseIfBranches.emplace_back(parseCondBranch());
    }
    return std::make_unique<IfStatement>(std::move(ifBranch), std::move(elseIfBranches),
                                         std::move(elseBranch));
}

std::unique_ptr<BlockNode> Parser::parseCurlyBracketBlock() {
    BlockNode::Statements body;
    if (lexer->currToken().type != TokenType::LeftCurlyBracket) {
        throw std::runtime_error(
                makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
    }
    lexer->nextToken(); // {
    while (lexer->currToken().type != TokenType::RightCurlyBracket) {
        if (auto node = nextNode(); node != nullptr) {
            body.emplace_back(std::move(node));
        }
    }
    lexer->nextToken(); // }
    return std::make_unique<BlockNode>(std::move(body));
}

std::unique_ptr<ExpressionNode> Parser::parseExpr() {
    if (const auto &token = lexer->currToken();
        isSign(token.type)
        || token.type == TokenType::Number
        || token.type == TokenType::String
        || token.type == TokenType::Boolean
        || token.type == TokenType::LeftParenthesis
        || token.type == TokenType::Identifier
        || token.type == TokenType::IncrementOperator
        || token.type == TokenType::DecrementOperator
        || token.type == TokenType::LogicalNegation) {
        auto expr = parseBoolLogic();
        if (lexer->currToken().type == TokenType::Question) {
            expr = parseTernaryOperator(std::move(expr));
        }
        return expr;
    }
    throw std::runtime_error(makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
}

std::unique_ptr<ExpressionNode> Parser::parseBoolLogic() {
    auto lhs = parseComparisonExpr();
    while (lexer->currToken().type == TokenType::LogicalAnd
           || lexer->currToken().type == TokenType::LogicalOr) {
        const auto op = lexer->currToken().type;
        lexer->nextToken();
        auto rhs = parseExpr();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<ExpressionNode> Parser::parseComparisonExpr() {
    auto lhs = parseAdditiveExpr();
    while (lexer->currToken().type == TokenType::LeftAngleBracket
           || lexer->currToken().type == TokenType::LeftAngleBracketEqual
           || lexer->currToken().type == TokenType::RightAngleBracket
           || lexer->currToken().type == TokenType::RightAngleBracketEqual
           || lexer->currToken().type == TokenType::Equal
           || lexer->currToken().type == TokenType::NotEqual) {
        const auto op = lexer->currToken().type;
        lexer->nextToken();
        auto rhs = parseExpr();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<ExpressionNode> Parser::parseAdditiveExpr() {
    auto lhs = parseTerm();
    while (lexer->currToken().type == TokenType::Plus
           || lexer->currToken().type == TokenType::Minus) {
        const auto op = lexer->currToken().type;
        lexer->nextToken();
        auto rhs = parseExpr();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), std::move(rhs));
    }
    return lhs;
}

std::unique_ptr<ExpressionNode> Parser::parseTerm() {
    auto lhs = parseFactor();
    while (lexer->currToken().type == TokenType::Star
           || lexer->currToken().type == TokenType::Slash) {
        const auto op = lexer->currToken().type;
        lexer->nextToken();
        lhs = std::make_unique<BinOpNode>(op, std::move(lhs), parseFactor());
    }
    return lhs;
}

std::unique_ptr<ExpressionNode> Parser::tryParseUnaryOp() {
    if (lexer->currToken().type == TokenType::Plus
        || lexer->currToken().type == TokenType::Minus
        || lexer->currToken().type == TokenType::LogicalNegation) {
        const auto type = lexer->currToken().type;
        lexer->nextToken();
        auto val = parseFactor();
        return std::make_unique<UnaryOpNode>(type,
                                             UnaryOpNode::UnaryOpType::Prefix,
                                             std::move(val));
    }
    return nullptr;
}

std::unique_ptr<ExpressionNode> Parser::tryParsePrefixOp() {
    if (lexer->currToken().type == TokenType::DecrementOperator
        || lexer->currToken().type == TokenType::IncrementOperator) {
        const auto type = lexer->currToken().type;
        lexer->nextToken();
        auto val = parseFactor();
        return std::make_unique<UnaryOpNode>(type,
                                             UnaryOpNode::UnaryOpType::Prefix,
                                             std::move(val));
    }
    return nullptr;
}

std::unique_ptr<ExpressionNode> Parser::tryParseIdentifier() {
    if (lexer->currToken().type == TokenType::Identifier) {
        auto ident = parseIdent();
        if (lexer->currToken().type == TokenType::DecrementOperator
            || lexer->currToken().type == TokenType::IncrementOperator) {
            const auto type = lexer->currToken().type;
            lexer->nextToken();
            return std::make_unique<UnaryOpNode>(type,
                                                 UnaryOpNode::UnaryOpType::Postfix,
                                                 std::move(ident));
        }
        if (lexer->currToken().type == TokenType::LeftParenthesis) {
            return parseFunctionCall(std::move(ident));
        }
        return ident;
    }
    return nullptr;
}

std::unique_ptr<BooleanNode> Parser::parseBoolean() const {
    if (lexer->currToken().type != TokenType::Boolean && lexer->currToken().value.has_value()) {
        throw std::runtime_error(makeErrorMsg("Invalid boolean expression"));
    }
    auto result = std::make_unique<BooleanNode>(lexer->currToken().value == "true");
    lexer->nextToken();
    return result;
}

std::unique_ptr<ExpressionNode> Parser::tryParseLiteral() const {
    if (lexer->currToken().type == TokenType::Number
        || (isSign(lexer->currToken().type) && lexer->peekToken().type == TokenType::Number)) {
        return parseNumber();
    }
    if (lexer->currToken().type == TokenType::String) {
        return parseString();
    }
    if (lexer->currToken().type == TokenType::Boolean) {
        return parseBoolean();
    }
    return nullptr;
}

std::unique_ptr<ExpressionNode> Parser::parseFactor() {
    if (lexer->currToken().type == TokenType::LeftParenthesis) {
        lexer->nextToken(); // "("
        auto expr = parseExpr();
        if (lexer->currToken().type != TokenType::RightParenthesis) {
            throw std::runtime_error(makeErrorMsg("Expected ')' character"));
        }
        lexer->nextToken(); // ")"
        return expr;
    }
    if (auto expr = tryParseLiteral()) {
        if (lexer->currToken().type == TokenType::Dot) {
            lexer->nextToken();
            auto ident = tryParseIdentifier();
            auto [funCall, orig] = tryCast<FunctionCallNode>(std::move(ident));
            if (funCall != nullptr) {
                return std::make_unique<MethodCallNode>(std::move(expr), std::move(funCall));
            }
            throw std::runtime_error(
                    makeErrorMsg("Unexpected expression: " + orig->toString()));
        }
        return expr;
    }
    if (auto expr = tryParseIdentifier()) {
        return expr;
    }
    if (auto expr = tryParseUnaryOp()) {
        return expr;
    }
    if (auto expr = tryParsePrefixOp()) {
        return expr;
    }
    throw std::runtime_error(makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
}

std::unique_ptr<IdentNode> Parser::parseIdent() const {
    auto ident = std::make_unique<IdentNode>(lexer->currToken().value.value_or(""));
    lexer->nextToken();
    return ident;
}

std::unique_ptr<NumberNode> Parser::parseNumber() const {
    auto sign = 1;
    if (isSign(lexer->currToken().type)) {
        sign = lexer->currToken().type == TokenType::Minus ? -1 : 1;
        lexer->nextToken();
    }
    if (!lexer->currToken().value.has_value()) {
        throw std::runtime_error(
                makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
    }

    const auto numberStr = lexer->currToken().value.value();
    double number = 0.0;
    auto [ptr, ec] = std::from_chars(
            numberStr.data(),
            numberStr.data() + numberStr.size(),
            number);
    if (ec != std::errc() || ptr != numberStr.data() + numberStr.size()) {
        throw std::logic_error(makeErrorMsg("Invalid numeric literal: " + numberStr));
    }
    const bool isFloat = numberStr.find_first_of(".eE") != std::string::npos;
    auto node = std::make_unique<NumberNode>(sign * number, isFloat);
    lexer->nextToken();
    return node;
}

std::unique_ptr<StringNode> Parser::parseString() const {
    auto result = std::make_unique<StringNode>(lexer->currToken().value.value_or(""));
    lexer->nextToken();
    return result;
}

std::unique_ptr<ExpressionNode> Parser::parseFunctionCall(std::unique_ptr<IdentNode> ident) {
    if (lexer->currToken().type != TokenType::LeftParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected '(' character"));
    }
    lexer->nextToken(); // '('
    std::vector<std::unique_ptr<ExpressionNode>> args;
    do {
        if (lexer->currToken().type == TokenType::Comma) {
            lexer->nextToken();
        }
        if (lexer->currToken().type != TokenType::RightParenthesis) {
            args.emplace_back(parseExpr());
        }
    } while (lexer->currToken().type == TokenType::Comma);

    if (lexer->currToken().type != TokenType::RightParenthesis) {
        throw std::runtime_error(makeErrorMsg("Expected ')' character"));
    }
    lexer->nextToken(); // ')'
    return std::make_unique<FunctionCallNode>(std::move(ident), std::move(args));
}

std::unique_ptr<StatementNode> Parser::parseFunctionDef() {
    if (lexer->currToken().type != TokenType::Identifier) {
        throw std::runtime_error(
                makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
    }
    const auto fnName = parseIdent();
    lexer->nextToken();
    std::vector<DeclarationNode> params;
    while (lexer->currToken().type != TokenType::RightParenthesis) {
        params.emplace_back(parseDeclarationNode(false));
        if (lexer->currToken().type != TokenType::Comma && lexer->currToken().type !=
            TokenType::RightParenthesis) {
            throw std::runtime_error(
                    makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
        }
        if (lexer->currToken().type == TokenType::Comma) {
            lexer->nextToken();
        }
    }
    lexer->nextToken(); // ")"
    auto retType = TypeKind::Void;
    if (lexer->currToken().type == TokenType::Colon) {
        lexer->nextToken();
        const auto typeName = lexer->currToken().value.value();
        const auto it = TYPES.find(typeName);
        if (it == TYPES.end()) {
            throw std::runtime_error(
                    makeErrorMsg("Unexpected type: " + lexer->currToken().toString()));
        }
        lexer->nextToken();
        retType = it->second;
    }
    auto proto = std::make_unique<ProtoFunctionStatement>(fnName->name,
                                                          TypeNode::makePrimitive(retType, false),
                                                          std::move(params));
    if (lexer->currToken().type == TokenType::LeftCurlyBracket) {
        return std::make_unique<FunctionNode>(std::move(proto), parseCurlyBracketBlock());
    }
    consumeSemicolon();
    return proto;
}

std::unique_ptr<BlockNode> Parser::parseBlock() {
    std::unique_ptr<BlockNode> block;
    if (lexer->currToken().type == TokenType::LeftCurlyBracket) {
        block = parseCurlyBracketBlock();
    } else {
        block = std::make_unique<BlockNode>(makeVectorUnique<BaseNode>(parseExpr()));
        consumeSemicolon();
    }
    return block;
}

DeclarationNode Parser::parseDeclarationNode(const bool needConsumeSemicolon) {
    auto ident = std::make_unique<IdentNode>(lexer->currToken().value.value());
    lexer->nextToken();
    if (lexer->currToken().type != TokenType::Colon) {
        throw std::runtime_error(
                makeErrorMsg("Unexpected token: " + lexer->currToken().toString()));
    }
    lexer->nextToken();
    const auto typeName = lexer->currToken().value.value();
    const auto it = TYPES.find(typeName);
    if (it == TYPES.end()) {
        throw std::runtime_error(makeErrorMsg("Unexpected type: " + lexer->currToken().toString()));
    }
    lexer->nextToken();
    std::optional<std::unique_ptr<ExpressionNode>> init = std::nullopt;
    if (lexer->currToken().type == TokenType::Assignment) {
        lexer->nextToken();
        init = parseExpr();
    }
    if (needConsumeSemicolon) {
        consumeSemicolon();
    }
    return {std::move(ident),
            TypeNode::makePrimitive(it->second, false),
            std::move(init)};
}

std::unique_ptr<StatementNode> Parser::parseReturnStatement() {
    std::unique_ptr<ExpressionNode> expr;
    if (lexer->currToken().type != TokenType::Semicolon) {
        expr = parseExpr();
    }
    consumeSemicolon();
    return std::make_unique<ReturnNode>(std::move(expr));
}

std::unique_ptr<ExpressionNode> Parser::parseTernaryOperator(std::unique_ptr<ExpressionNode> cond) {
    if (lexer->currToken().type != TokenType::Question) {
        throw std::runtime_error(makeErrorMsg("Expected '?' symbol"));
    }
    lexer->nextToken();
    auto trueExpr = parseExpr();
    if (lexer->currToken().type != TokenType::Colon) {
        throw std::runtime_error(makeErrorMsg("Expected ':' symbol"));
    }
    lexer->nextToken();
    auto falseExpr = parseExpr();
    return std::make_unique<TernaryOperatorNode>(std::move(cond),
                                                 std::move(trueExpr),
                                                 std::move(falseExpr));
}

std::string Parser::makeErrorMsg(const std::string &msg) const {
    std::string lines;
    uint32_t startLinePos = 0;
    for (const auto &[ch, pos, lineNumber]: lexer->readText()) {
        if (ch == '\n' && pos < lexer->currToken().startPosition) {
            startLinePos = pos + 1;
        }
        lines.push_back(ch);
    }
    const auto padding = lexer->currToken().startPosition - startLinePos;
    if (lines.size() > 0 && lines[lines.size() - 1] != '\n') {
        lines.push_back('\n');
    }
    lines.insert(lines.end(), padding, '-');
    lines.insert(lines.end(), lexer->currToken().endPosition - lexer->currToken().startPosition + 1,
                 '^');
    return std::format("\n{}\n{}", lines, msg);
}
