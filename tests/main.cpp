//
// Created by vadim on 14.12.24.
//

#include <functional>
#include <memory>
#include <sstream>
#include <gtest/gtest.h>

#include "Lexer.h"
#include "Parser.h"
#include "ast/BinOpNode.h"
#include "ast/BaseNode.h"
#include "ast/AssignmentNode.h"
#include "ast/IdentNode.h"
#include "ast/UnaryOpNode.h"
#include "Util.h"
#include "ast/ForLoopNode.h"
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"
#include "ast/IfStatement.h"

namespace {
    class VarDefinitionTest : public testing::Test {};

    TEST_F(VarDefinitionTest, AssignBinExpr) {
        const std::string input = "varName=2*(1-2);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->name, "varName") << "Wrong variable name: " << input;

        const auto *binExpr = dynamic_cast<BinOpNode *>(varDef->rvalue.get());
        ASSERT_NE(binExpr, nullptr) << "RHS is not a binary expression: " << input;
    }

    TEST_F(VarDefinitionTest, AssignNumber) {
        const std::string input = "varName=1;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [varDef, orig] = tryCast<AssignmentNode>(std::move(node));
        ASSERT_NE(varDef, nullptr) << "Not a variable definition: " << input;

        EXPECT_EQ(varDef->name, "varName") << "Wrong variable name: " << input;

        const auto *number = dynamic_cast<NumberNode *>(varDef->rvalue.get());
        ASSERT_NE(number, nullptr) << "RHS is not a number: " << input;
        EXPECT_EQ(number->value, 1) << "Wrong number value: " << input;
    }

    class BinExpressionsTest : public testing::Test {};

    TEST_F(BinExpressionsTest, SimpleSubtraction) {
        const std::string input = "-1-21.2;";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
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

    TEST_F(BinExpressionsTest, NestedOperations) {
        const std::string input = "(2*(1+2));";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
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
        const std::string input = "v1 == v2";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
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
        const std::string input = "v1 != v2";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::NotEqual) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, LessThan) {
        const std::string input = "v1 < v2";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::LeftAngleBracket) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, LessThanEqual) {
        const std::string input = "v1 <= v2";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::LeftAngleBracketEqual) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, GreatThan) {
        const std::string input = "v1 > v2";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::RightAngleBracket) << "Wrong operator: " << input;
    }

    TEST_F(ComparisonOpTest, GreatThanEqual) {
        const std::string input = "v1 >= v2";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [binOp, orig] = tryCast<BinOpNode>(std::move(node));
        ASSERT_NE(binOp, nullptr) << "Not a binary operation: " << input;

        EXPECT_EQ(binOp->binOp, TokenType::RightAngleBracketEqual) << "Wrong operator: " << input;
    }

    class LogicalOpTest : public testing::Test {};

    TEST_F(LogicalOpTest, Negation) {
        const std::string input = "!v1";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [unaryOp, orig] = tryCast<UnaryOpNode>(std::move(node));
        ASSERT_NE(unaryOp, nullptr) << "Not a unary operation: " << input;

        EXPECT_EQ(unaryOp->operatorType, TokenType::LogicalNegation) << "Wrong operator: " << input;
        EXPECT_EQ(unaryOp->unaryPosType, UnaryOpNode::UnaryOpType::Prefix) << "Wrong operator: " << input;
        const auto *ident = dynamic_cast<IdentNode *>(unaryOp->expr.get());
        ASSERT_NE(ident, nullptr) << "Failed to cast unary operation: " << input;
        EXPECT_EQ(ident->name, "v1") << "Wrong ident name: " << input;
    }

    TEST_F(BinExpressionsTest, ComplexExpression) {
        const std::string input = "+1 *  (   2    +3.0);";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
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
        const std::string input = "-1.123";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse: " << input;

        auto [numberNode, orig] = tryCast<NumberNode>(std::move(node));
        ASSERT_NE(numberNode, nullptr) << "Not a number node: " << input;
        EXPECT_DOUBLE_EQ(numberNode->value, -1.123) << "Wrong number value: " << input;
    }

    class IdentifiersTest : public testing::Test {};

    TEST_F(IdentifiersTest, IdentNode) {
        const std::string input = "var";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
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
            auto node = parser->parseNextNode();

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
        testUnaryOperation("var++",
                           TokenType::IncrementOperator,
                           UnaryOpNode::UnaryOpType::Postfix,
                           "var");
    }

    TEST_F(UnaryOpTest, PrefixIncrement) {
        testUnaryOperation("++var",
                           TokenType::IncrementOperator,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    TEST_F(UnaryOpTest, PostfixDecrement) {
        testUnaryOperation("var--",
                           TokenType::DecrementOperator,
                           UnaryOpNode::UnaryOpType::Postfix,
                           "var");
    }

    TEST_F(UnaryOpTest, PrefixDecrement) {
        testUnaryOperation("--var",
                           TokenType::DecrementOperator,
                           UnaryOpNode::UnaryOpType::Prefix,
                           "var");
    }

    class FunctionCallTest : public testing::Test {};

    TEST_F(FunctionCallTest, ComplexFunctionCall) {
        const std::string input = "foo(1, 2.1, var, 1 + 2)";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));

        auto node = parser->parseNextNode();
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

    class FunctionDefTest : public testing::Test {};

    TEST_F(FunctionDefTest, ComplexFunctionDefinition) {
        const std::string input = R"(
        fn foo(arg1, arg2, arg3, arg4) {
            v = 1
            ++v
        }
    )";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input))
                );

        auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse function definition";

        auto [fnNode, orig] = tryCast<FunctionNode>(std::move(node));
        ASSERT_NE(fnNode, nullptr) << "Parsed node is not a function";

        EXPECT_EQ(fnNode->name->name, "foo") << "Wrong function name";

        ASSERT_EQ(fnNode->params.size(), 4) << "Wrong number of parameters";
        EXPECT_EQ(fnNode->params[0]->name, "arg1") << "Wrong parameter 1 name";
        EXPECT_EQ(fnNode->params[1]->name, "arg2") << "Wrong parameter 2 name";
        EXPECT_EQ(fnNode->params[2]->name, "arg3") << "Wrong parameter 3 name";
        EXPECT_EQ(fnNode->params[3]->name, "arg4") << "Wrong parameter 4 name";

        ASSERT_FALSE(fnNode->body.empty()) << "Function body should not be empty";
        ASSERT_EQ(fnNode->body.size(), 2) << "Wrong number of body statements";

        const auto *stmt1 = dynamic_cast<AssignmentNode *>(fnNode->body[0].get());
        ASSERT_NE(stmt1, nullptr) << "First statement is not a variable definition";
        EXPECT_EQ(stmt1->name, "v") << "Wrong identifier in first statement";

        const auto *stmt2 = dynamic_cast<UnaryOpNode *>(fnNode->body[1].get());
        ASSERT_NE(stmt2, nullptr) << "Second statement is not a unary operation";
        EXPECT_EQ(stmt2->operatorType, TokenType::IncrementOperator)
            << "Wrong unary operator type";
        EXPECT_EQ(stmt2->unaryPosType, UnaryOpNode::UnaryOpType::Prefix)
            << "Wrong unary operator position";

        const auto *operand = dynamic_cast<IdentNode *>(stmt2->expr.get());
        ASSERT_NE(operand, nullptr) << "Invalid operand type";
        EXPECT_EQ(operand->name, "v") << "Wrong operand identifier";
    }

    class IfStatementTest : public testing::Test {};

    TEST_F(IfStatementTest, IfWithoutElse) {
        const std::string input = "if (flag) { doSomething() }";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if without else";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->elseIfBranches.size(), 0) << "Unexpected else-if branches";
        ASSERT_FALSE(ifNode->elseBranch.has_value()) << "Unexpected else branch";
    }

    TEST_F(IfStatementTest, IfWithElseIf) {
        const std::string input = "if (flag) { doSomething() } else if (otherFlag) { doOtherThing() }";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if-elseif";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->elseIfBranches.size(), 1) << "Wrong number of else-if branches";
        ASSERT_FALSE(ifNode->elseBranch.has_value()) << "Unexpected else branch";
    }

    TEST_F(IfStatementTest, IfWithElse) {
        const std::string input = "if (flag) { doSomething() } else { doOtherThing() }";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if with else";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_TRUE(ifNode->elseIfBranches.empty()) << "Unexpected else-if branches";
        ASSERT_TRUE(ifNode->elseBranch.has_value()) << "Missing else branch";
    }

    TEST_F(IfStatementTest, IfElseIfElse) {
        const std::string input =
                R"(if (flag) {
                        doSomething()
                    } else if (otherFlag) {
                        doOtherThing() }
                    else {
                        doAnotherThing()
                    })";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if-elseif-else";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->elseIfBranches.size(), 1) << "Wrong number of else-if branches";
        ASSERT_TRUE(ifNode->elseBranch.has_value()) << "Missing else branch";
    }

    TEST_F(IfStatementTest, IfWithoutBraces) {
        const std::string input = R"(if (flag)
                                        doSomething())";
        const auto parser = std::make_unique<Parser>(
            std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if without braces";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->ifBranch.then.size(), 1) << "Wrong then branch size";
    }

    TEST_F(IfStatementTest, IfElseWithoutBraces) {
        const std::string input = R"(if (flag)
                                        doSomething()
                                    else
                                        doOtherThing())";
        const auto parser = std::make_unique<Parser>(
            std::make_unique<Lexer>(std::make_unique<std::istringstream>(input)));
        const auto node = parser->parseNextNode();
        ASSERT_NE(node, nullptr) << "Failed to parse if-else without braces";
        const auto *const ifNode = dynamic_cast<IfStatement *>(node.get());
        ASSERT_NE(ifNode, nullptr) << "Not an if-statement node";
        ASSERT_EQ(ifNode->ifBranch.then.size(), 1) << "Wrong then branch size";
        ASSERT_TRUE(ifNode->elseBranch.has_value()) << "Missing else branch";
        ASSERT_EQ(ifNode->elseBranch->size(), 1) << "Wrong else branch size";
    }

    class ForStatementTest : public testing::Test {};

    TEST_F(ForStatementTest, ForEmpty) {
        const std::string input = R"(for (var = 0; var < 10; ++var) {})";
        const auto parser = std::make_unique<Parser>(
                std::make_unique<Lexer>(std::make_unique<std::istringstream>(input))
                );
        const auto node = parser->parseNextNode();

        ASSERT_NE(node, nullptr) << "Failed to parse the for-loop expression";

        const auto *const forNode = dynamic_cast<ForLoopNode *>(node.get());
        ASSERT_NE(forNode, nullptr) << "Parsed node is not a for-loop node";

        ASSERT_NE(forNode->init, nullptr) << "For loop missing initialization expression";
        const auto *varNode = dynamic_cast<AssignmentNode *>(forNode->init.get());
        ASSERT_NE(varNode, nullptr) << "For loop initialization is not an assignment node";
        EXPECT_EQ(varNode->name, "var") << "Wrong variable name in initialization: " << input;

        const auto *varNumber = dynamic_cast<NumberNode *>(varNode->rvalue.get());
        ASSERT_NE(varNumber, nullptr) << "For loop initialization value is not a number node";
        EXPECT_DOUBLE_EQ(varNumber->value, 0) << "Wrong initialization value for variable 'var': " << input;

        ASSERT_NE(forNode->conditional, nullptr) << "For loop missing condition expression";
        const auto *condNode = dynamic_cast<BinOpNode *>(forNode->conditional.get());
        ASSERT_NE(condNode, nullptr) << "Condition expression is not a binary operation node";

        const auto *condVar = dynamic_cast<IdentNode *>(condNode->lhs.get());
        ASSERT_NE(condVar, nullptr) << "Left-hand side of condition is not an identifier node";
        EXPECT_EQ(condVar->name, "var") << "Wrong variable name in condition: " << input;

        ASSERT_EQ(condNode->binOp, TokenType::LeftAngleBracket) << "Condition operator is not '<' as expected: " << input;

        ASSERT_NE(forNode->next, nullptr) << "For loop missing iteration expression";
        const auto *nextOp = dynamic_cast<UnaryOpNode *>(forNode->next.get());
        ASSERT_NE(nextOp, nullptr) << "Iteration expression is not a unary operation node";

        ASSERT_EQ(nextOp->operatorType,
                  TokenType::IncrementOperator) << "Expected increment operator in iteration expression";
        ASSERT_EQ(nextOp->unaryPosType,
                  UnaryOpNode::UnaryOpType::Prefix) << "Expected prefix increment operator in iteration expression";

        const auto *nextIdent = dynamic_cast<IdentNode *>(nextOp->expr.get());
        ASSERT_NE(nextIdent, nullptr) << "Iteration expression does not contain a valid identifier node";
        EXPECT_EQ(nextIdent->name, "var") << "Wrong variable name in iteration: " << input;

        ASSERT_TRUE(forNode->body.empty()) << "Expected empty body for for loop: " << input;
    }

} // namespace

int main(int argc, char *argv[]) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
