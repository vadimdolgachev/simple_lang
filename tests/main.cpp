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
#include "ast/FunctionCallNode.h"
#include "ast/FunctionNode.h"

namespace {
    std::string makeTestFailMsg(const std::uint32_t line) {
        return std::string("test failed, line=").append(std::to_string(line));
    }

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

    class FunctionCallTest : public ::testing::Test {};

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

    class FunctionDefTest : public ::testing::Test {};

    TEST_F(FunctionDefTest, ComplexFunctionDefinition) {
        const std::string input = R"(
        fn foo(arg1, arg2, arg3, arg4) {
            v = 1
            ++v
        }
    )";

        try {
            const auto parser = std::make_unique<Parser>(
                    std::make_unique<Lexer>(std::make_unique<std::istringstream>(input))
                    );

            auto node = parser->parseNextNode();
            ASSERT_NE(node, nullptr) << "Failed to parse function definition";

            auto [fnNode, orig] = tryCast<FunctionNode>(std::move(node));
            ASSERT_NE(fnNode, nullptr) << "Parsed node is not a function";

            // Проверка имени функции
            EXPECT_EQ(fnNode->name->name, "foo") << "Wrong function name";

            // Проверка параметров
            ASSERT_EQ(fnNode->params.size(), 4) << "Wrong number of parameters";
            EXPECT_EQ(fnNode->params[0]->name, "arg1") << "Wrong parameter 1 name";
            EXPECT_EQ(fnNode->params[1]->name, "arg2") << "Wrong parameter 2 name";
            EXPECT_EQ(fnNode->params[2]->name, "arg3") << "Wrong parameter 3 name";
            EXPECT_EQ(fnNode->params[3]->name, "arg4") << "Wrong parameter 4 name";

            // Проверка тела функции
            ASSERT_FALSE(fnNode->body.empty()) << "Function body should not be empty";
            ASSERT_EQ(fnNode->body.size(), 2) << "Wrong number of body statements";

            // Проверка первого выражения (v = 1)
            const auto *stmt1 = dynamic_cast<AssignmentNode *>(fnNode->body[0].get());
            ASSERT_NE(stmt1, nullptr) << "First statement is not a variable definition";
            EXPECT_EQ(stmt1->name, "v") << "Wrong identifier in first statement";

            // Проверка второго выражения (++v)
            const auto *stmt2 = dynamic_cast<UnaryOpNode *>(fnNode->body[1].get());
            ASSERT_NE(stmt2, nullptr) << "Second statement is not a unary operation";
            EXPECT_EQ(stmt2->operatorType, TokenType::IncrementOperator)
            << "Wrong unary operator type";
            EXPECT_EQ(stmt2->unaryPosType, UnaryOpNode::UnaryOpType::Prefix)
            << "Wrong unary operator position";

            const auto *operand = dynamic_cast<IdentNode *>(stmt2->expr.get());
            ASSERT_NE(operand, nullptr) << "Invalid operand type";
            EXPECT_EQ(operand->name, "v") << "Wrong operand identifier";

        } catch (const std::exception &e) {
            FAIL() << "Exception caught during parsing: " << e.what();
        } catch (...) {
            FAIL() << "Unknown exception caught during parsing";
        }
    }


} // namespace

int main(int argc, char *argv[]) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
