//
// Created by vadim on 14.12.24.
//

#include <memory>
#include <sstream>
#include <gtest/gtest.h>

#include "Lexer.h"
#include "Parser.h"
#include "Util.h"

#include "ast/BinOpNode.h"
#include "ast/BaseNode.h"
#include "ast/AssignmentNode.h"
#include "ast/BooleanNode.h"
#include "ast/CommentNode.h"
#include "ast/IdentNode.h"
#include "ast/UnaryOpNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/IfStatement.h"
#include "ast/NumberNode.h"
#include "ast/StringNode.h"
#include "ast/LoopCondNode.h"
#include "ast/MethodCallNode.h"
#include "ast/FieldAccessNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/ReturnNode.h"
#include "ast/TernaryOperatorNode.h"
#include "ast/TypeNode.h"

namespace {
    class VarDefinitionTest : public testing::Test {};

    TEST_F(VarDefinitionTest, DeclarationVar) {
        const std::string input = "varName:int;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDecl, orig] = tryCast<DeclarationNode>(std::move(node));
        ASSERT_NE(varDecl, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDecl->ident->name, "varName") << "Wrong variable name: " << input;

        EXPECT_FALSE(varDecl->init.has_value()) << "Variable has an initial value: " << input;
    }

    TEST_F(VarDefinitionTest, DeclarationBinExpr) {
        const std::string input = "varName:int=2*(1-2);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDecl, orig] = tryCast<DeclarationNode>(std::move(node));
        ASSERT_NE(varDecl, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDecl->ident->name, "varName") << "Wrong variable name: " << input;

        EXPECT_TRUE(varDecl->init.has_value()) << "Variable has no initial value: " << input;
        const auto *binExpr = dynamic_cast<BinOpNode *>(varDecl->init.value().get());
        ASSERT_NE(binExpr, nullptr) << "RHS is not a binary expression: " << input;
    }

    TEST_F(VarDefinitionTest, AssignBinExpr) {
        const std::string input = "varName=2*(1-2);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->lvalue->name, "varName") << "Wrong variable name: " << input;

        const auto *binExpr = dynamic_cast<BinOpNode *>(varDef->rvalue.get());
        ASSERT_NE(binExpr, nullptr) << "RHS is not a binary expression: " << input;
    }

    TEST_F(VarDefinitionTest, AssignIntNumber) {
        const std::string input = "varName=1;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->lvalue->name, "varName") << "Wrong variable name: " << input;

        const auto *number = dynamic_cast<NumberNode *>(varDef->rvalue.get());
        ASSERT_NE(number, nullptr) << "RHS is not a number: " << input;
        EXPECT_EQ(number->value, 1) << "Wrong number value: " << input;
        EXPECT_EQ(number->isFloat, false) << "Wrong number type: " << input;
    }

    TEST_F(VarDefinitionTest, AssignFloatNumber) {
        const std::string input = "varName=1.0;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->lvalue->name, "varName") << "Wrong variable name: " << input;

        const auto *number = dynamic_cast<NumberNode *>(varDef->rvalue.get());
        ASSERT_NE(number, nullptr) << "RHS is not a number: " << input;
        EXPECT_FLOAT_EQ(number->value, 1.0) << "Wrong number value: " << input;
        EXPECT_EQ(number->isFloat, true) << "Wrong number type: " << input;
    }


    TEST_F(VarDefinitionTest, AssignFloat2Number) {
        const std::string input = "varName=.5;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->lvalue->name, "varName") << "Wrong variable name: " << input;

        const auto *number = dynamic_cast<NumberNode *>(varDef->rvalue.get());
        ASSERT_NE(number, nullptr) << "RHS is not a number: " << input;
        EXPECT_FLOAT_EQ(number->value, 0.5) << "Wrong number value: " << input;
        EXPECT_EQ(number->isFloat, true) << "Wrong number type: " << input;
    }

    TEST_F(VarDefinitionTest, AssignFloat3Number) {
        const std::string input = "varName=1.;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->lvalue->name, "varName") << "Wrong variable name: " << input;

        const auto *number = dynamic_cast<NumberNode *>(varDef->rvalue.get());
        ASSERT_NE(number, nullptr) << "RHS is not a number: " << input;
        EXPECT_FLOAT_EQ(number->value, 1.0) << "Wrong number value: " << input;
        EXPECT_EQ(number->isFloat, true) << "Wrong number type: " << input;
    }

    TEST_F(VarDefinitionTest, MultipleAssigns) {
        const std::string input = "varName1=1;varName2=2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef1, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef1, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef1->lvalue->name, "varName1") << "Wrong variable name: " << input;

        const auto *number1 = dynamic_cast<NumberNode *>(varDef1->rvalue.get());
        ASSERT_NE(number1, nullptr) << "RHS is not a number: " << input;
        EXPECT_EQ(number1->value, 1) << "Wrong number value: " << input;

        node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef2, orig2] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef2, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef2->lvalue->name, "varName2") << "Wrong variable name: " << input;

        const auto *number2 = dynamic_cast<NumberNode *>(varDef2->rvalue.get());
        ASSERT_NE(number2, nullptr) << "RHS is not a number: " << input;
        EXPECT_EQ(number2->value, 2) << "Wrong number value: " << input;
    }

    class BinExpressionsTest : public testing::Test {};

    TEST_F(BinExpressionsTest, SimpleSubtraction) {
        const std::string input = "-1-21.2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::Minus) << "Wrong operator: " << input;

        const auto *lhs = dynamic_cast<NumberNode *>(binOp->lhs.get());
        ASSERT_NE(lhs, nullptr) << "Invalid LHS: " << input;
        EXPECT_EQ(lhs->value, -1) << "Wrong LHS value: " << input;

        const auto *rhs = dynamic_cast<NumberNode *>(binOp->rhs.get());
        ASSERT_NE(rhs, nullptr) << "Invalid RHS: " << input;
        EXPECT_DOUBLE_EQ(rhs->value, 21.2) << "Wrong RHS value: " << input;
    }

    TEST_F(BinExpressionsTest, BinOrder) {
        const std::string input = "2 * (2 - 1) / 2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::Slash) << "Wrong operator: " << input;

        const auto *lhs = dynamic_cast<BinOpNode *>(binOp->lhs.get());
        ASSERT_NE(lhs, nullptr) << "Invalid LHS: " << input;
        const auto *lhs1 = dynamic_cast<NumberNode *>(lhs->lhs.get());
        ASSERT_NE(lhs1, nullptr) << "Invalid LHS: " << input;
        ASSERT_DOUBLE_EQ(lhs1->value, 2) << "Wrong LHS value: " << input;
        const auto *rhs1 = dynamic_cast<BinOpNode *>(lhs->rhs.get());
        ASSERT_NE(rhs1, nullptr) << "Invalid RHS: " << input;

        const auto *lhs2 = dynamic_cast<NumberNode *>(rhs1->lhs.get());
        ASSERT_NE(lhs2, nullptr) << "Invalid LHS: " << input;
        ASSERT_DOUBLE_EQ(lhs2->value, 2) << "Wrong LHS value: " << input;
        const auto *rhs2 = dynamic_cast<NumberNode *>(rhs1->rhs.get());
        ASSERT_NE(rhs2, nullptr) << "Invalid RHS: " << input;
        ASSERT_DOUBLE_EQ(rhs2->value, 1) << "Wrong RHS value: " << input;

        ASSERT_DOUBLE_EQ(lhs1->value, 2) << "Wrong LHS value: " << input;

        const auto *rhs = dynamic_cast<NumberNode *>(binOp->rhs.get());
        ASSERT_NE(rhs, nullptr) << "Invalid RHS: " << input;
        EXPECT_DOUBLE_EQ(rhs->value, 2) << "Wrong RHS value: " << input;
    }

    TEST_F(BinExpressionsTest, NestedOperations) {
        const std::string input = "(2*(1+2));";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::Star) << "Wrong operator: " << input;

        const auto *lhs = dynamic_cast<NumberNode *>(binOp->lhs.get());
        ASSERT_NE(lhs, nullptr) << "Invalid LHS: " << input;
        EXPECT_EQ(lhs->value, 2) << "Wrong LHS value: " << input;

        const auto *rhs = dynamic_cast<BinOpNode *>(binOp->rhs.get());
        ASSERT_NE(rhs, nullptr) << "Invalid RHS: " << input;
        EXPECT_EQ(rhs->binOp, TokenType::Plus) << "Wrong nested operator: " << input;

        const auto *rhsLhs = dynamic_cast<NumberNode *>(rhs->lhs.get());
        ASSERT_NE(rhsLhs, nullptr) << "Invalid nested LHS: " << input;
        EXPECT_EQ(rhsLhs->value, 1) << "Wrong nested LHS value: " << input;

        const auto *rhsRhs = dynamic_cast<NumberNode *>(rhs->rhs.get());
        ASSERT_NE(rhsRhs, nullptr) << "Invalid nested RHS: " << input;
        EXPECT_EQ(rhsRhs->value, 2) << "Wrong nested RHS value: " << input;
    }

    class ComparisonOpTest : public testing::Test {};

    TEST_F(ComparisonOpTest, Equal) {
        const std::string input = "v1 == v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::Equal) << "Wrong operator: " << input;

        const auto *lhs = dynamic_cast<IdentNode *>(binOp->lhs.get());
        ASSERT_NE(lhs, nullptr) << "Invalid LHS: " << input;
        EXPECT_EQ(lhs->name, "v1") << "Wrong LHS var name: " << input;

        const auto *rhs = dynamic_cast<IdentNode *>(binOp->rhs.get());
        ASSERT_NE(rhs, nullptr) << "Invalid RHS: " << input;
        EXPECT_EQ(rhs->name, "v2") << "Wrong RHS var name: " << input;
    }

    TEST_F(ComparisonOpTest, NotEqual) {
        const std::string input = "v1 != v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::NotEqual) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, LessThan) {
        const std::string input = "v1 < v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::LeftAngleBracket) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, LessThanEqual) {
        const std::string input = "v1 <= v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::LeftAngleBracketEqual) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, GreatThan) {
        const std::string input = "v1 > v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::RightAngleBracket) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, GreatThanEqual) {
        const std::string input = "v1 >= v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::RightAngleBracketEqual) << "Wrong operator: " << input;
    }

    class LogicalOpTest : public testing::Test {};

    TEST_F(LogicalOpTest, LogicalAnd) {
        const std::string input = "v1 && v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::LogicalAnd) << "Wrong operator: " << input;
    }

    TEST_F(LogicalOpTest, LogicalAndNeg) {
        const std::string input = "!(v1 && v2);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [unaryOp, orig] = tryCast<UnaryOpNode>(std::move(node));
        ASSERT_NE(unaryOp, nullptr) << "Not a unary operation: " << input;

        ASSERT_NE(unaryOp->expr, nullptr) << "Not a expression: " << input;
        const auto *binOp = dynamic_cast<BinOpNode *>(unaryOp->expr.get());
        EXPECT_EQ(binOp->binOp, TokenType::LogicalAnd) << "Wrong operator: " << input;

        ASSERT_NE(binOp->lhs, nullptr) << "Wrong lhs: " << input;
        const auto *lhs = dynamic_cast<IdentNode *>(binOp->lhs.get());
        EXPECT_EQ(lhs->name, "v1") << "Wrong ident name: " << input;

        ASSERT_NE(binOp->rhs, nullptr) << "Wrong rhs: " << input;
        const auto *rhs = dynamic_cast<IdentNode *>(binOp->rhs.get());
        EXPECT_EQ(rhs->name, "v2") << "Wrong ident name: " << input;
    }

    TEST_F(LogicalOpTest, LogicalOr) {
        const std::string input = "v1 || v2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::LogicalOr) << "Wrong operator: " << input;
    }

    TEST_F(LogicalOpTest, MixedLogicalOpsWithParentheses) {
        const std::string input = "(a || b) && (c || d);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));

        ASSERT_NE(binOp, nullptr);
        EXPECT_EQ(binOp->binOp, TokenType::LogicalAnd);

        auto *left = dynamic_cast<BinOpNode *>(binOp->lhs.get());
        ASSERT_NE(left, nullptr);
        EXPECT_EQ(left->binOp, TokenType::LogicalOr);
    }

    TEST_F(LogicalOpTest, NestedLogicalOps) {
        const std::string input = "a || b && c;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));

        ASSERT_NE(binOp, nullptr);
        EXPECT_EQ(binOp->binOp, TokenType::LogicalOr);

        auto *right = dynamic_cast<BinOpNode *>(binOp->rhs.get());
        ASSERT_NE(right, nullptr);
        EXPECT_EQ(right->binOp, TokenType::LogicalAnd);
    }

    TEST_F(BinExpressionsTest, ComplexExpression) {
        const std::string input = "+1 *  (   2    +3.0);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::Star) << "Wrong operator: " << input;

        const auto *lhs = dynamic_cast<NumberNode *>(binOp->lhs.get());
        ASSERT_NE(lhs, nullptr) << "Invalid LHS: " << input;
        EXPECT_EQ(lhs->value, 1) << "Wrong LHS value: " << input;

        const auto *rhs = dynamic_cast<BinOpNode *>(binOp->rhs.get());
        ASSERT_NE(rhs, nullptr) << "Invalid RHS: " << input;
        EXPECT_EQ(rhs->binOp, TokenType::Plus) << "Wrong nested operator: " << input;

        const auto *rhsLhs = dynamic_cast<NumberNode *>(rhs->lhs.get());
        ASSERT_NE(rhsLhs, nullptr) << "Invalid nested LHS: " << input;
        EXPECT_EQ(rhsLhs->value, 2) << "Wrong nested LHS value: " << input;

        const auto *rhsRhs = dynamic_cast<NumberNode *>(rhs->rhs.get());
        ASSERT_NE(rhsRhs, nullptr) << "Invalid nested RHS: " << input;
        EXPECT_DOUBLE_EQ(rhsRhs->value, 3.0) << "Wrong nested RHS value: " << input;
    }

    class NodesTest : public testing::Test {};

    TEST_F(NodesTest, NumberNode) {
        const std::string input = "-1.123;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [numberNode, orig] = tryCast<NumberNode>(std::move(node));
        ASSERT_NE(numberNode, nullptr) << "Not a number node: " << input;
        EXPECT_DOUBLE_EQ(numberNode->value, -1.123) << "Wrong number value: " << input;
    }

    TEST_F(NodesTest, StringNode) {
        const std::string input = R"("hello,
 world";)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [strNode, orig] = tryCast<StringNode>(std::move(node));
        ASSERT_NE(strNode, nullptr) << "Not a string node: " << input;
        EXPECT_EQ(std::format("\"{}\";", strNode->str), input) << "Wrong string value: " << input;
    }

    class BooleanNodeTest : public NodesTest,
                            public testing::WithParamInterface<std::pair<std::string, bool>> {};

    TEST_P(BooleanNodeTest, ParsesBooleanLiterals) {
        const auto &[input, expected] = GetParam();
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(std::format("{};", input))));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [booleanNode, orig] = tryCast<BooleanNode>(std::move(node));
        ASSERT_NE(booleanNode, nullptr) << "Not a boolean node: " << input;
        EXPECT_EQ(booleanNode->value, expected) << "Wrong value for: " << input;
    }

    INSTANTIATE_TEST_SUITE_P(
            BooleanLiterals,
            BooleanNodeTest,
            testing::Values(
                std::make_pair("true", true),
                std::make_pair ("false", false)
            ),
            [](const testing::TestParamInfo<BooleanNodeTest::ParamType>& info) {return info.param.first;}
            );

    class IdentifiersTest : public testing::Test {};

    TEST_F(IdentifiersTest, IdentNode) {
        const std::string input = "var;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [identNode, orig] = tryCast<IdentNode>(std::move(node));
        ASSERT_NE(identNode, nullptr) << "Not an identifier node: " << input;
        EXPECT_EQ(identNode->name, "var") << "Wrong identifier name: " << input;
    }

    class UnaryOpTest : public testing::Test {
    protected:
        void testUnaryOperation(const std::string &input,
                                const TokenType expectedOp,
                                const UnaryOpNode::UnaryOpType expectedPos,
                                const std::string &expectedIdent) {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
            auto node = parser->nextNode();

            ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

            auto [unOp, orig] = tryCast<UnaryOpNode>(std::move(node));
            ASSERT_NE(unOp, nullptr) << "Not a unary operation: " << input;

            EXPECT_EQ(unOp->operatorType, expectedOp) << "Wrong operator type: " << input;
            EXPECT_EQ(unOp->unaryPosType, expectedPos) << "Wrong operator position: " << input;

            ASSERT_NE(unOp->expr, nullptr) << "Null expression: " << input;

            const auto *ident = dynamic_cast<IdentNode *>(unOp->expr.get());
            ASSERT_NE(ident, nullptr) << "Not an identifier: " << input;
            EXPECT_EQ(ident->name, expectedIdent) << "Wrong identifier: " << input;
        }
    };

    TEST_F(UnaryOpTest, PostfixIncrement) {
        testUnaryOperation("var++;",
                           TokenType::PlusPlus,
                           UnaryOpNode::UnaryOpType::Postfix,
                           "var");
    }

    TEST_F(UnaryOpTest, PrefixIncrement) {
        testUnaryOperation("++var;",
                           TokenType::PlusPlus,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    TEST_F(UnaryOpTest, PostfixDecrement) {
        testUnaryOperation("var--;",
                           TokenType::MinusMinus,
                           UnaryOpNode::UnaryOpType::Postfix,
                           "var");
    }

    TEST_F(UnaryOpTest, PrefixDecrement) {
        testUnaryOperation("--var;",
                           TokenType::MinusMinus,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    TEST_F(UnaryOpTest, Plus) {
        testUnaryOperation("+var;",
                           TokenType::Plus,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    TEST_F(UnaryOpTest, Minus) {
        testUnaryOperation("-var;",
                           TokenType::Minus,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    TEST_F(UnaryOpTest, Negation) {
        testUnaryOperation("!var;",
                           TokenType::LogicalNegation,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    class FunctionTest : public testing::Test {};

    TEST_F(FunctionTest, ProtoFunction) {
        const std::string input = "fn foo(arg1: int, arg2: char, arg3: byte, arg4: str): str;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse input: " << input;

        auto [protoFn, orig] = tryCast<ProtoFunctionStatement>(std::move(node));
        ASSERT_NE(protoFn, nullptr) << "Not a function call node: " << input;

        EXPECT_EQ(protoFn->name, "foo") << "Wrong function name in: " << input;

        ASSERT_EQ(protoFn->params.size(), 4) << "Wrong number of arguments in: " << input;

        EXPECT_EQ(protoFn->params[0]->ident->name, "arg1") << "Wrong identifier name in: " << input;
        EXPECT_EQ(protoFn->params[1]->ident->name, "arg2") << "Wrong identifier name in: " << input;
        EXPECT_EQ(protoFn->params[2]->ident->name, "arg3") << "Wrong identifier name in: " << input;
        EXPECT_EQ(protoFn->params[3]->ident->name, "arg4") << "Wrong identifier name in: " << input;

        EXPECT_EQ(protoFn->returnType.kind, TypeKind::Str) << "Wrong return type: " << input;
    }

    TEST_F(FunctionTest, ComplexFunctionCall) {
        const std::string input = "foo(1, 2.1, var, 1 + 2);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse input: " << input;

        auto [fnCall, orig] = tryCast<FunctionCallNode>(std::move(node));
        ASSERT_NE(fnCall, nullptr) << "Not a function call node: " << input;

        EXPECT_EQ(fnCall->ident->name, "foo") << "Wrong function name in: " << input;

        ASSERT_EQ(fnCall->args.size(), 4) << "Wrong number of arguments in: " << input;

        const auto *arg1 = dynamic_cast<NumberNode *>(fnCall->args[0].get());
        ASSERT_NE(arg1, nullptr) << "Argument 1 is not a number in: " << input;
        EXPECT_EQ(arg1->value, 1) << "Wrong value for argument 1 in: " << input;

        const auto *arg2 = dynamic_cast<NumberNode *>(fnCall->args[1].get());
        ASSERT_NE(arg2, nullptr) << "Argument 2 is not a number in: " << input;
        EXPECT_DOUBLE_EQ(arg2->value, 2.1) << "Wrong value for argument 2 in: " << input;

        const auto *arg3 = dynamic_cast<IdentNode *>(fnCall->args[2].get());
        ASSERT_NE(arg3, nullptr) << "Argument 3 is not an identifier in: " << input;
        EXPECT_EQ(arg3->name, "var") << "Wrong identifier name in: " << input;

        const auto *arg4 = dynamic_cast<BinOpNode *>(fnCall->args[3].get());
        ASSERT_NE(arg4, nullptr) << "Argument 4 is not a binary operation in: " << input;

        EXPECT_EQ(arg4->binOp, TokenType::Plus) << "Wrong binary operator in: " << input;

        const auto *lhs = dynamic_cast<NumberNode *>(arg4->lhs.get());
        ASSERT_NE(lhs, nullptr) << "Left operand is not a number in: " << input;
        EXPECT_EQ(lhs->value, 1) << "Wrong left operand value in: " << input;

        const auto *rhs = dynamic_cast<NumberNode *>(arg4->rhs.get());
        ASSERT_NE(rhs, nullptr) << "Right operand is not a number in: " << input;
        EXPECT_EQ(rhs->value, 2) << "Wrong right operand value in: " << input;
    }

    TEST_F(FunctionTest, MultipleFunctionCall) {
        const std::string input = "foo1(1);foo2(2);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse input: " << input;

        auto [fnCall1, orig1] = tryCast<FunctionCallNode>(std::move(node));
        ASSERT_NE(fnCall1, nullptr) << "Not a function call node: " << input;

        EXPECT_EQ(fnCall1->ident->name, "foo1") << "Wrong function name in: " << input;

        ASSERT_EQ(fnCall1->args.size(), 1) << "Wrong number of arguments in: " << input;

        const auto *fn1Arg1 = dynamic_cast<NumberNode *>(fnCall1->args[0].get());
        ASSERT_NE(fn1Arg1, nullptr) << "Argument 1 is not a number in: " << input;
        EXPECT_EQ(fn1Arg1->value, 1) << "Wrong value for argument 1 in: " << input;

        node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse input: " << input;

        auto [fnCall2, orig2] = tryCast<FunctionCallNode>(std::move(node));
        ASSERT_NE(fnCall2, nullptr) << "Not a function call node: " << input;

        EXPECT_EQ(fnCall2->ident->name, "foo2") << "Wrong function name in: " << input;

        ASSERT_EQ(fnCall2->args.size(), 1) << "Wrong number of arguments in: " << input;

        const auto *fn2Arg1 = dynamic_cast<NumberNode *>(fnCall2->args[0].get());
        ASSERT_NE(fn2Arg1, nullptr) << "Argument 1 is not a number in: " << input;
        EXPECT_EQ(fn2Arg1->value, 2) << "Wrong value for argument 1 in: " << input;
    }

    TEST_F(FunctionTest, ComplexFunctionDefinition) {
        const std::string input = R"(
        fn foo(arg1: int, arg2: int, arg3: int, arg4: int) {
            v = 1;
            ++v;
        }
    )";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input))
                );

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse function definition";

        auto [fnNode, orig] = tryCast<FunctionNode>(std::move(node));
        ASSERT_NE(fnNode, nullptr) << "Parsed node is not a function";

        EXPECT_EQ(fnNode->proto->name, "foo") << "Wrong function name";

        ASSERT_EQ(fnNode->proto->params.size(), 4) << "Wrong number of parameters";
        EXPECT_EQ(fnNode->proto->params[0]->ident->name, "arg1") << "Wrong parameter 1 name";
        EXPECT_EQ(fnNode->proto->params[1]->ident->name, "arg2") << "Wrong parameter 2 name";
        EXPECT_EQ(fnNode->proto->params[2]->ident->name, "arg3") << "Wrong parameter 3 name";
        EXPECT_EQ(fnNode->proto->params[3]->ident->name, "arg4") << "Wrong parameter 4 name";

        ASSERT_FALSE(fnNode->body->statements.empty()) << "Function body should not be empty";
        ASSERT_EQ(fnNode->body->statements.size(), 2) << "Wrong number of body statements";

        const auto *stmt1 = dynamic_cast<AssignmentNode *>(fnNode->body->statements[0].get());
        ASSERT_NE(stmt1, nullptr) << "First statement is not a variable definition";
        EXPECT_EQ(stmt1->lvalue->name, "v") << "Wrong identifier in first statement";

        const auto *stmt2 = dynamic_cast<UnaryOpNode *>(fnNode->body->statements[1].get());
        ASSERT_NE(stmt2, nullptr) << "Second statement is not a unary operation";
        EXPECT_EQ(stmt2->operatorType, TokenType::PlusPlus)
            << "Wrong unary operator type";
        EXPECT_EQ(stmt2->unaryPosType, UnaryOpNode::UnaryOpType::Prefix)
            << "Wrong unary operator position";

        const auto *operand = dynamic_cast<IdentNode *>(stmt2->expr.get());
        ASSERT_NE(operand, nullptr) << "Invalid operand type";
        EXPECT_EQ(operand->name, "v") << "Wrong operand identifier";

        EXPECT_EQ(fnNode->proto->returnType.kind, TypeKind::Void) << "Wrong return type: " << input;
    }

    class IfStatementTest : public testing::Test {};

    TEST_F(IfStatementTest, IfWithoutElse) {
        const std::string input = "if (flag) { doSomething(); }";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if without else";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->elseIfBranches.size(), 0) << "Unexpected else-if branches";
        ASSERT_FALSE(ifNode->elseBranch.has_value()) << "Unexpected else branch";
    }

    TEST_F(IfStatementTest, IfWithElseIf) {
        const std::string input = "if (flag) { doSomething(); } else if (otherFlag) { doOtherThing(); }";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if-elseif";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->elseIfBranches.size(), 1) << "Wrong number of else-if branches";
        ASSERT_FALSE(ifNode->elseBranch.has_value()) << "Unexpected else branch";
    }

    TEST_F(IfStatementTest, IfWithElse) {
        const std::string input = "if (flag) { doSomething(); } else { doOtherThing(); }";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if with else";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_TRUE(ifNode->elseIfBranches.empty()) << "Unexpected else-if branches";
        ASSERT_TRUE(ifNode->elseBranch.has_value()) << "Missing else branch";
    }

    TEST_F(IfStatementTest, IfElseIfElse) {
        const std::string input =
                R"(if (flag) {
                        doSomething();
                    } else if (otherFlag) {
                        doOtherThing();
                    } else {
                        doAnotherThing();
                    })";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if-elseif-else";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->elseIfBranches.size(), 1) << "Wrong number of else-if branches";
        ASSERT_TRUE(ifNode->elseBranch.has_value()) << "Missing else branch";
    }

    TEST_F(IfStatementTest, IfWithoutBraces) {
        const std::string input = R"(if (flag)
                                        doSomething();)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if without braces";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->ifBranch.then->statements.size(), 1) << "Wrong then branch size";
    }

    TEST_F(IfStatementTest, IfElseWithoutBraces) {
        const std::string input = R"(if (flag)
                                        doSomething();
                                    else
                                        doOtherThing();)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if-else without braces";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->ifBranch.then->statements.size(), 1) << "Wrong then branch size";
        ASSERT_TRUE(ifNode->elseBranch.has_value()) << "Missing else branch";
        ASSERT_EQ(ifNode->elseBranch.value()->statements.size(), 1) << "Wrong else branch size";
    }

    class LoopStatementTest : public testing::Test {};

    TEST_F(LoopStatementTest, ForEmpty) {
        const std::string input = R"(for (var: int = 0; var < 10; ++var) {})";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input))
                );
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the for-loop expression";

        const auto *const forNode = dynamic_cast<LoopCondNode *>(node.get());
        ASSERT_NE(forNode, nullptr) << "Parsed node is not a for-loop node";

        ASSERT_NE(forNode->init, nullptr) << "For loop missing initialization expression";
        const auto *varNode = dynamic_cast<DeclarationNode *>(forNode->init->get());
        ASSERT_NE(varNode, nullptr) << "For loop initialization is not an assignment node";
        EXPECT_EQ(varNode->ident->name, "var") << "Wrong variable name in initialization: " << input;

        const auto *varNumber = dynamic_cast<NumberNode *>(varNode->init->get());
        ASSERT_NE(varNumber, nullptr) << "For loop initialization value is not a number node";
        EXPECT_DOUBLE_EQ(varNumber->value, 0) << "Wrong initialization value for variable 'var': " << input;

        ASSERT_NE(forNode->condBranch.cond, nullptr) << "For loop missing condition expression";
        const auto *condNode = dynamic_cast<BinOpNode *>(forNode->condBranch.cond.get());
        ASSERT_NE(condNode, nullptr) << "Condition expression is not a binary operation node";

        const auto *condVar = dynamic_cast<IdentNode *>(condNode->lhs.get());
        ASSERT_NE(condVar, nullptr) << "Left-hand side of condition is not an identifier node";
        EXPECT_EQ(condVar->name, "var") << "Wrong variable name in condition: " << input;

        ASSERT_EQ(condNode->binOp, TokenType::LeftAngleBracket)
            << "Condition operator is not '<' as expected: " << input;

        ASSERT_NE(forNode->increment->get(), nullptr) << "For loop missing iteration expression";
        const auto *nextOp = dynamic_cast<UnaryOpNode *>(forNode->increment->get());
        ASSERT_NE(nextOp, nullptr) << "Iteration expression is not a unary operation node";

        ASSERT_EQ(nextOp->operatorType,
                  TokenType::PlusPlus) << "Expected increment operator in iteration expression";
        ASSERT_EQ(nextOp->unaryPosType,
                  UnaryOpNode::UnaryOpType::Prefix) << "Expected prefix increment operator in iteration expression";

        const auto *nextIdent = dynamic_cast<IdentNode *>(nextOp->expr.get());
        ASSERT_NE(nextIdent, nullptr) << "Iteration expression does not contain a valid identifier node";
        EXPECT_EQ(nextIdent->name, "var") << "Wrong variable name in iteration: " << input;

        ASSERT_TRUE(forNode->condBranch.then->statements.empty()) << "Expected empty body for for loop: " << input;
    }

    TEST_F(LoopStatementTest, WhileEmpty) {
        const std::string input = R"(while (var < 10) {})";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the while-loop expression";

        const auto *const loopNode = dynamic_cast<LoopCondNode *>(node.get());
        ASSERT_NE(loopNode, nullptr) << "Parsed node is not a while-loop node";

        ASSERT_NE(loopNode->condBranch.cond, nullptr) << "While loop missing condition expression";
        const auto *condNode = dynamic_cast<BinOpNode *>(loopNode->condBranch.cond.get());
        ASSERT_NE(condNode, nullptr) << "Condition expression is not a binary operation node";

        const auto *condVar = dynamic_cast<IdentNode *>(condNode->lhs.get());
        ASSERT_NE(condVar, nullptr) << "Left-hand side of condition is not an identifier node";
        EXPECT_EQ(condVar->name, "var") << "Wrong variable name in condition: " << input;

        ASSERT_EQ(condNode->binOp, TokenType::LeftAngleBracket)
            << "Condition operator is not '<' as expected: " << input;

        ASSERT_TRUE(loopNode->condBranch.then->statements.empty()) << "Expected empty body for for loop: " << input;
    }

    TEST_F(LoopStatementTest, DoWhileEmpty) {
        const std::string input = R"(do {} while (var < 10);)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the do-while-loop expression";

        const auto *const loopNode = dynamic_cast<LoopCondNode *>(node.get());
        ASSERT_NE(loopNode, nullptr) << "Parsed node is not a do-while-loop node";

        ASSERT_NE(loopNode->condBranch.cond, nullptr) << "While loop missing condition expression";
        const auto *condNode = dynamic_cast<BinOpNode *>(loopNode->condBranch.cond.get());
        ASSERT_NE(condNode, nullptr) << "Condition expression is not a binary operation node";

        const auto *condVar = dynamic_cast<IdentNode *>(condNode->lhs.get());
        ASSERT_NE(condVar, nullptr) << "Left-hand side of condition is not an identifier node";
        EXPECT_EQ(condVar->name, "var") << "Wrong variable name in condition: " << input;

        ASSERT_EQ(condNode->binOp, TokenType::LeftAngleBracket)
            << "Condition operator is not '<' as expected: " << input;

        ASSERT_TRUE(loopNode->condBranch.then->statements.empty()) << "Expected empty body for for loop: " << input;
    }

    class ReturnStatementTest : public testing::Test {};

    TEST_F(ReturnStatementTest, ReturnEmpty) {
        const std::string input = R"(return;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the return statement";

        const auto *const retNode = dynamic_cast<ReturnNode *>(node.get());
        ASSERT_NE(retNode, nullptr) << "Parsed node is not a ReturnNode";
        ASSERT_EQ(retNode->expr, nullptr) << "Expression in ReturnNode is not null";
    }

    TEST_F(ReturnStatementTest, ReturnIdent) {
        const std::string input = R"(return var;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the return statement";

        const auto *const retNode = dynamic_cast<ReturnNode *>(node.get());
        ASSERT_NE(retNode, nullptr) << "Parsed node is not a ReturnNode";
        const auto *const identNode = dynamic_cast<IdentNode *>(retNode->expr.get());
        ASSERT_NE(identNode, nullptr) << "IdentNode is null";
        ASSERT_EQ(identNode->name, "var") << "Wrong variable name in condition: " << input;
    }

    TEST_F(ReturnStatementTest, ReturnBinExpr) {
        const std::string input = R"(return 1+2;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the return statement";

        const auto *const retNode = dynamic_cast<ReturnNode *>(node.get());
        ASSERT_NE(retNode, nullptr) << "Parsed node is not a ReturnNode";
        const auto *const binOp = dynamic_cast<BinOpNode *>(retNode->expr.get());
        ASSERT_NE(binOp, nullptr) << "BinOpNode is null";
    }

    class TernaryOperatorTest : public testing::Test {};
    TEST_F(TernaryOperatorTest, TernaryOperatorExpr) {
        const std::string input = R"(true ? 10 : 100;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the ternary operator";

        const auto *const ternaryNode = dynamic_cast<TernaryOperatorNode *>(node.get());
        ASSERT_NE(ternaryNode, nullptr) << "Parsed node is not a TernaryOperatorNode";

        const auto *const cond = dynamic_cast<BooleanNode *>(ternaryNode->cond.get());
        ASSERT_TRUE(cond->value) << "Invalid ternary operator value";

        const auto *const trueExpr = dynamic_cast<NumberNode *>(ternaryNode->trueExpr.get());
        ASSERT_FLOAT_EQ(trueExpr->value, 10) << "Invalid ternary operator value";

        const auto *const falseExpr = dynamic_cast<NumberNode *>(ternaryNode->falseExpr.get());
        ASSERT_FLOAT_EQ(falseExpr->value, 100) << "Invalid ternary operator value";
    }

    TEST_F(TernaryOperatorTest, TernaryOperatorInitVar) {
        const std::string input = R"(var: int = true ? 1O : 100;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->nextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the declaration";

        const auto *const declNode = dynamic_cast<DeclarationNode *>(node.get());
        ASSERT_NE(declNode, nullptr) << "Parsed node is not a DeclarationNode";

        const auto *const ternary = dynamic_cast<TernaryOperatorNode *>(declNode->init.value().get());
        ASSERT_NE(ternary, nullptr) << "The value of Init must be of type TernaryOperatorNode";
    }

    TEST_F(NodesTest, MethodCallStringNode) {
        const std::string input = R"("hello".len();)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [methodCall, orig] = tryCast<MethodCallNode>(std::move(node));
        ASSERT_NE(methodCall, nullptr) << "Not a MemberAccessNode node: " << input;
        EXPECT_EQ("len", methodCall->method->ident->name) << "Wrong function name: " << input;
    }

    TEST_F(NodesTest, FieldAccessNode) {
        const std::string input = R"(obj.field1;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [fieldAccess, orig] = tryCast<FieldAccessNode>(std::move(node));
        ASSERT_NE(fieldAccess, nullptr) << "Not a FieldAccessNode node: " << input;
        EXPECT_EQ("field1", fieldAccess->field->name) << "Wrong field name: " << input;
    }

    TEST_F(NodesTest, MultiFieldAccessNode) {
        const std::string input = R"(obj.method().field;)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [fieldAccess, orig] = tryCast<FieldAccessNode>(std::move(node));
        ASSERT_NE(fieldAccess, nullptr) << "Not a FieldAccess node: " << input;

        EXPECT_EQ("field", fieldAccess->field->name) << "Wrong field name: " << input;

        auto *methodCall = dynamic_cast<MethodCallNode *>(fieldAccess->object.get());
        ASSERT_NE(methodCall, nullptr) << "Not a FunctionCallNode node: " << input;
        EXPECT_EQ("method", methodCall->method->ident->name) << "Wrong field name: " << input;

        auto *objectName = dynamic_cast<IdentNode *>(methodCall->object.get());
        ASSERT_NE(objectName, nullptr) << "Not a IdentNode node: " << input;
        EXPECT_EQ("obj", objectName->name) << "Wrong object name: " << input;
    }

    TEST_F(NodesTest, CommentNode) {
        const std::string input = R"(// comment text)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->nextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [commentNode, orig] = tryCast<CommentNode>(std::move(node));
        ASSERT_NE(commentNode, nullptr) << "Not a CommentNode node: " << input;
        EXPECT_EQ(" comment text", commentNode->text) << "Wrong comment text: " << input;
    }

} // namespace

int main(int argc, char *argv[]) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
