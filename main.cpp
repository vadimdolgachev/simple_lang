#include <iostream>
#include <list>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>
#include <unordered_map>

#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BasicAliasAnalysis.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/MemorySSA.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ProfileSummaryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"

#include "KaleidoscopeJIT.h"
#include "Lexer.h"
#include "ast/BinOpNode.h"
#include "ast/CallFunctionNode.h"
#include "ast/ForLoopNode.h"
#include "ast/FunctionNode.h"
#include "ast/IfStatementStatement.h"
#include "ast/NumberNode.h"
#include "ast/ProtoFunctionStatement.h"
#include "ast/UnaryOpNode.h"
#include "ast/VariableAccessNode.h"
#include "ast/VariableDefinitionStatement.h"
#include "ir/IRCodegen.h"

namespace {
    std::unique_ptr<llvm::LLVMContext> llvmContext;
    std::unique_ptr<llvm::Module> llvmModule;
    std::unique_ptr<llvm::IRBuilder<> > llvmIRBuilder;
    std::unique_ptr<llvm::orc::KaleidoscopeJIT> llvmJit;
    std::unordered_map<std::string, llvm::Value *> namedValues;
    std::unique_ptr<llvm::FunctionPassManager> functionPassManager;
    std::unique_ptr<llvm::FunctionAnalysisManager> functionAnalysisManager;
    std::unique_ptr<llvm::ModuleAnalysisManager> moduleAnalysisManager;
    std::unique_ptr<llvm::PassInstrumentationCallbacks> passInstsCallbacks;
    std::unique_ptr<llvm::StandardInstrumentations> standardInsts;
    const llvm::ExitOnError ExitOnError;

    void initLlvmModules() {
        llvmContext = std::make_unique<llvm::LLVMContext>();
        llvmModule = std::make_unique<llvm::Module>("my cool jit", *llvmContext);
        llvmModule->setDataLayout(llvmJit->getDataLayout());

        llvmIRBuilder = std::make_unique<llvm::IRBuilder<> >(*llvmContext);

        functionPassManager = std::make_unique<llvm::FunctionPassManager>();
        functionAnalysisManager = std::make_unique<llvm::FunctionAnalysisManager>();
        moduleAnalysisManager = std::make_unique<llvm::ModuleAnalysisManager>();
        passInstsCallbacks = std::make_unique<llvm::PassInstrumentationCallbacks>();
        standardInsts = std::make_unique<llvm::StandardInstrumentations>(*llvmContext, /*DebugLogging*/ true);
        standardInsts->registerCallbacks(*passInstsCallbacks, moduleAnalysisManager.get());

        // Add transform passes.
        // Do simple "peephole" optimizations and bit-twiddling optzns.
        functionPassManager->addPass(llvm::InstCombinePass());
        // Reassociate expressions.
        functionPassManager->addPass(llvm::ReassociatePass());
        // Eliminate Common SubExpressions.
        functionPassManager->addPass(llvm::GVNPass());
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        functionPassManager->addPass(llvm::SimplifyCFGPass());

        // Register analysis passes used in these transform passes.
        functionAnalysisManager->registerPass([&] { return llvm::AAManager(); });
        functionAnalysisManager->registerPass([&] { return llvm::AssumptionAnalysis(); });
        functionAnalysisManager->registerPass([&] { return llvm::DominatorTreeAnalysis(); });
        functionAnalysisManager->registerPass([&] { return llvm::LoopAnalysis(); });
        functionAnalysisManager->registerPass([&] { return llvm::MemoryDependenceAnalysis(); });
        functionAnalysisManager->registerPass([&] { return llvm::MemorySSAAnalysis(); });
        functionAnalysisManager->registerPass([&] { return llvm::OptimizationRemarkEmitterAnalysis(); });
        functionAnalysisManager->registerPass([&] {
            return llvm::OuterAnalysisManagerProxy<llvm::ModuleAnalysisManager, llvm::Function>(*moduleAnalysisManager);
        });
        functionAnalysisManager->registerPass(
            [&] { return llvm::PassInstrumentationAnalysis(passInstsCallbacks.get()); });
        functionAnalysisManager->registerPass([&] { return llvm::TargetIRAnalysis(); });
        functionAnalysisManager->registerPass([&] { return llvm::TargetLibraryAnalysis(); });
        moduleAnalysisManager->registerPass([&] { return llvm::ProfileSummaryAnalysis(); });
    }

    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionStatement> > functionProtos;

    std::unique_ptr<BaseNode> parseAstNodeItem(const std::unique_ptr<Lexer> &lexer);

    std::unique_ptr<BaseNode> parseExpr(const std::unique_ptr<Lexer> &lexer, bool inExpression = false);

    std::tuple<std::unique_ptr<ExpressionNode>, std::unique_ptr<BaseNode> > toExpr(std::unique_ptr<BaseNode> node) {
        if (dynamic_cast<ExpressionNode *>(node.get()) != nullptr) {
            return {std::unique_ptr<ExpressionNode>(dynamic_cast<ExpressionNode *>(node.release())), nullptr};
        }
        return {nullptr, std::move(node)};
    }

    std::tuple<std::unique_ptr<StatementNode>, std::unique_ptr<BaseNode> >
    toStatement(std::unique_ptr<BaseNode> node) {
        if (dynamic_cast<StatementNode *>(node.get()) != nullptr) {
            return {std::unique_ptr<StatementNode>(dynamic_cast<StatementNode *>(node.release())), nullptr};
        }
        return {nullptr, std::move(node)};
    }

    std::unique_ptr<ExpressionNode> parseNumberExpr(const std::unique_ptr<Lexer> &lexer,
                                                    const bool inExpression = false) {
        auto number = std::make_unique<NumberNode>(strtod(lexer->getNumberValue().c_str(), nullptr));
        lexer->readNextToken(inExpression);
        return number;
    }

    std::unique_ptr<ExpressionNode> parseParentheses(const std::unique_ptr<Lexer> &lexer) {
        if (lexer->getCurrentToken() != TokenType::LeftParenthesisToken) {
            return nullptr;
        }
        lexer->readNextToken(); // eat TokenType::LeftParenthesis
        auto expr = parseAstNodeItem(lexer);
        if (lexer->getCurrentToken() != TokenType::RightParenthesisToken) {
            return nullptr;
        }
        lexer->readNextToken(); // eat TokenType::RightParenthesis
        return std::get<0>(toExpr(std::move(expr)));
    }

    std::unique_ptr<BaseNode> parseIdentifier(const std::unique_ptr<Lexer> &lexer, const bool inExpression = false) {
        const std::string name = lexer->getIdentifier();
        lexer->readNextToken(inExpression); // eat identifier
        if (lexer->getCurrentToken() == TokenType::EqualsToken) {
            lexer->readNextToken(); // eat =
            auto expr = parseAstNodeItem(lexer);
            return std::make_unique<VariableDefinitionStatement>(name, std::get<0>(toExpr(std::move(expr))));
        }
        if (lexer->getCurrentToken() != TokenType::LeftParenthesisToken) {
            return std::make_unique<VariableAccessNode>(name);
        }

        std::vector<std::unique_ptr<ExpressionNode> > args;
        lexer->readNextToken(); // eat TokenType::LeftParenthesis
        while (true) {
            if (auto arg = parseAstNodeItem(lexer)) {
                args.push_back(std::get<0>(toExpr(std::move(arg))));
                if (lexer->getCurrentToken() == TokenType::CommaToken) {
                    lexer->readNextToken(); // eat ','
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        if (lexer->getCurrentToken() != TokenType::RightParenthesisToken) {
            return nullptr;
        }
        lexer->readNextToken(); // eat TokenType::RightParenthesis
        return std::make_unique<CallFunctionNode>(name, std::move(args));
    }

    std::list<std::unique_ptr<BaseNode> > parseCurlyBrackets(const std::unique_ptr<Lexer> &lexer) {
        std::list<std::unique_ptr<BaseNode> > expressions;
        while (auto node = parseAstNodeItem(lexer)) {
            expressions.push_back(std::move(node));
            if (lexer->getCurrentToken() != TokenType::RightCurlyBracketToken) {
                break;
            }
            lexer->readNextToken();
        }
        return expressions;
    }

    std::unique_ptr<StatementNode> parseIfExpression(const std::unique_ptr<Lexer> &lexer) {
        lexer->readNextToken();
        if (lexer->getCurrentToken() != TokenType::LeftParenthesisToken) {
            return nullptr;
        }
        auto cond = parseParentheses(lexer);
        if (lexer->getCurrentToken() != TokenType::LeftCurlyBracketToken) {
            return nullptr;
        }
        lexer->readNextToken();
        std::list<std::unique_ptr<BaseNode> > thenBranch = parseCurlyBrackets(lexer);
        lexer->readNextToken();
        std::optional<std::list<std::unique_ptr<BaseNode> > > elseBranch;
        if (lexer->getCurrentToken() == TokenType::ElseToken) {
            lexer->readNextToken();
            if (lexer->getCurrentToken() != TokenType::LeftCurlyBracketToken) {
                return nullptr;
            }
            lexer->readNextToken();
            elseBranch = parseCurlyBrackets(lexer);
        }
        return std::make_unique<IfStatementStatement>(std::move(cond), std::move(thenBranch), std::move(elseBranch));
    }

    std::unique_ptr<StatementNode> parseForLoopExpression(const std::unique_ptr<Lexer> &lexer) {
        lexer->readNextToken();
        if (lexer->getCurrentToken() != TokenType::LeftParenthesisToken) {
            return nullptr;
        }
        lexer->readNextToken();
        auto loopInit = parseIdentifier(lexer);
        if (loopInit == nullptr) {
            return nullptr;
        }
        lexer->readNextToken(true);
        auto loopFinish = parseAstNodeItem(lexer);
        if (loopFinish == nullptr) {
            return nullptr;
        }
        lexer->readNextToken(true);
        auto loopNext = parseAstNodeItem(lexer);
        if (loopNext == nullptr) {
            return nullptr;
        }
        lexer->readNextToken();
        if (lexer->getCurrentToken() != TokenType::LeftCurlyBracketToken) {
            return nullptr;
        }
        lexer->readNextToken();
        auto loopBody = parseCurlyBrackets(lexer);

        auto forLoopExpr = std::make_unique<ForLoopNode>(std::get<0>(toStatement(std::move(loopInit))),
                                                         std::get<0>(toExpr(std::move(loopNext))),
                                                         std::get<0>(toExpr(std::move(loopFinish))),
                                                         std::move(loopBody));
        return forLoopExpr;
    }

    std::unique_ptr<ExpressionNode> parseUnaryExpression(const std::unique_ptr<Lexer> &lexer) {
        const auto operatorType = lexer->getCurrentToken();
        lexer->readNextToken(true);
        auto expr = parseExpr(lexer, true);

        auto nodeOpType = OperatorType::UnknownOperator;
        switch (operatorType) {
            case TokenType::DecrementOperatorToken:
                nodeOpType = OperatorType::DecrementOperator;
                break;
            case TokenType::IncrementOperatorToken:
                nodeOpType = OperatorType::IncrementOperator;
                break;
            default:
                return nullptr;
        }
        return std::make_unique<UnaryOpNode>(nodeOpType,
                                             std::get<0>(toExpr(std::move(expr))));
    }

    std::unique_ptr<StatementNode> parseStatement(const std::unique_ptr<Lexer> &lexer) {
        if (lexer->getCurrentToken() == TokenType::IfToken) {
            return parseIfExpression(lexer);
        }
        if (lexer->getCurrentToken() == TokenType::ForLoopToken) {
            return parseForLoopExpression(lexer);
        }
        return nullptr;
    }

    std::unique_ptr<BaseNode> parseExpr(const std::unique_ptr<Lexer> &lexer, const bool inExpression) {
        if (lexer->getCurrentToken() == TokenType::NumberToken) {
            return parseNumberExpr(lexer, inExpression);
        }
        if (lexer->getCurrentToken() == TokenType::IdentifierToken) {
            return parseIdentifier(lexer, inExpression);
        }
        if (lexer->getCurrentToken() == TokenType::IncrementOperatorToken
            || lexer->getCurrentToken() == TokenType::DecrementOperatorToken) {
            return parseUnaryExpression(lexer);
        }
        if (lexer->getCurrentToken() == TokenType::LeftParenthesisToken) {
            return parseParentheses(lexer);
        }
        return nullptr;
    }

    int getBinOpPrecedence(const TokenType binOp) {
        int binOpPrec = -1;
        if (binOp == TokenType::PlusToken || binOp == TokenType::MinusToken) {
            binOpPrec = 1;
        } else if (binOp == TokenType::DivideToken || binOp == TokenType::MultiplyToken) {
            binOpPrec = 2;
        } else if (binOp == TokenType::LeftAngleBracketToken || binOp == TokenType::RightAngleBracketToken) {
            binOpPrec = 0;
        }
        return binOpPrec;
    }

    std::unique_ptr<ExpressionNode> parseBinOp(const std::unique_ptr<Lexer> &lexer,
                                               const int expPrec,
                                               std::unique_ptr<ExpressionNode> lhs) {
        while (true) {
            const auto binOp = lexer->getCurrentToken();
            const int curBinOpPrec = getBinOpPrecedence(binOp);
            if (curBinOpPrec < expPrec) {
                return lhs;
            }

            lexer->readNextToken(true); // read rhs
            auto rhs = parseExpr(lexer, true);
            if (rhs == nullptr) {
                return nullptr;
            }

            const auto nextBinOp = lexer->getCurrentToken();
            if (const int nextBinOpPrec = getBinOpPrecedence(nextBinOp); curBinOpPrec < nextBinOpPrec) {
                if (rhs = parseBinOp(lexer, curBinOpPrec, std::get<0>(toExpr(std::move(rhs)))); rhs == nullptr) {
                    return nullptr;
                }
            }

            lhs = std::make_unique<BinOpNode>(binOp, std::move(lhs), std::get<0>(toExpr(std::move(rhs))));
        }
    }

    std::unique_ptr<BaseNode> parseAstNodeItem(const std::unique_ptr<Lexer> &lexer) {
        if (auto node = parseExpr(lexer, true)) {
            auto [expr, srcNode] = toExpr(std::move(node));
            if (expr) {
                return parseBinOp(lexer, 0, std::move(expr));
            }
            return std::move(srcNode);
        }
        if (auto statement = parseStatement(lexer)) {
            return statement;
        }
        return nullptr;
    }

    void print(const llvm::Value *const llvmIR) {
        llvm::outs() << "IR: ";
        llvmIR->print(llvm::outs(), true);
        llvm::outs() << '\n';
    }

    void print(const BaseNode *const nodeAst) {
        std::list<BinOpNode *> values;
        const auto *ptr = dynamic_cast<const BinOpNode *>(nodeAst);
        do {
            if (!values.empty()) {
                ptr = values.front();
                values.pop_front();
            }
            if (ptr == nullptr) {
                continue;
            }
            if (auto *const rhs = dynamic_cast<BinOpNode *>(ptr->rhs.get())) {
                values.push_back(rhs);
            }
            if (auto *const lhs = dynamic_cast<BinOpNode *>(ptr->lhs.get())) {
                values.push_back(lhs);
            }
            std::cout << ">" << ptr->toString() << "\n";
        } while (!values.empty());
    }

    std::unique_ptr<ProtoFunctionStatement> parseProto(const std::unique_ptr<Lexer> &lexer) {
        const std::string name = lexer->getIdentifier();
        lexer->readNextToken(); // eat callee
        if (lexer->getCurrentToken() != TokenType::LeftParenthesisToken) {
            return nullptr;
        }
        lexer->readNextToken(); // eat TokenType::LeftParenthesis
        std::vector<std::string> args;
        while (lexer->hasNextToken()) {
            if (lexer->getCurrentToken() != TokenType::IdentifierToken) {
                break;
            }
            if (auto arg = parseIdentifier(lexer)) {
                const auto *const var = dynamic_cast<const VariableAccessNode *>(arg.get());
                args.push_back(var->name);
                if (lexer->getCurrentToken() == TokenType::CommaToken) {
                    lexer->readNextToken(); // eat next arg
                }
            } else {
                break;
            }
        }
        if (lexer->getCurrentToken() != TokenType::RightParenthesisToken) {
            return nullptr;
        }
        lexer->readNextToken(); // eat TokenType::RightParenthesis
        return std::make_unique<ProtoFunctionStatement>(name, args);
    }

    std::unique_ptr<FunctionNode> parseFunctionDefinition(const std::unique_ptr<Lexer> &lexer) {
        lexer->readNextToken(); // eat def
        auto proto = parseProto(lexer);
        if (lexer->getCurrentToken() != TokenType::LeftCurlyBracketToken) {
            return nullptr;
        }
        lexer->readNextToken();
        std::list<std::unique_ptr<BaseNode> > body = parseCurlyBrackets(lexer);
        return std::make_unique<FunctionNode>(std::move(proto), std::move(body));
    }

    std::unique_ptr<FunctionNode> parseTopLevelExpr(const std::unique_ptr<Lexer> &lexer,
                                                    const char *const functionName) {
        std::list<std::unique_ptr<BaseNode> > body;
        while (auto expr = parseAstNodeItem(lexer)) {
            if (expr == nullptr) {
                break;
            }
            body.push_back(std::move(expr));
            lexer->readNextToken();
        }
        auto proto = std::make_unique<ProtoFunctionStatement>(
            functionName, std::vector<std::string>());
        return std::make_unique<FunctionNode>(std::move(proto), std::move(body));
    }

    double print(const double param) {
        printf("print: %f\n", param);
        return param;
    }

    void mainHandler(const std::unique_ptr<Lexer> &lexer) {
        lexer->readNextToken();
        do {
            if (lexer->getCurrentToken() == TokenType::FunctionDefinitionToken) {
                auto definition = parseFunctionDefinition(lexer);
                if (definition != nullptr) {
                    print(definition.get());
                }
                ExitOnError(llvmJit->addModule(
                    llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmContext)), nullptr));
                initLlvmModules();
                lexer->readNextToken();
            } else {
                if (const auto function = parseTopLevelExpr(lexer, "_start")) {
                    const auto *const llvmIR = generateIR(function.get(),
                                                          llvmContext,
                                                          llvmIRBuilder,
                                                          llvmModule,
                                                          functionProtos,
                                                          namedValues);
                    if (llvmIR != nullptr) {
                        print(llvmIR);
                        const auto resourceTracker = llvmJit->getMainJITDylib().createResourceTracker();
                        auto threadSafeModule = llvm::orc::ThreadSafeModule(std::move(llvmModule),
                                                                            std::move(llvmContext));
                        ExitOnError(llvmJit->addModule(std::move(threadSafeModule), resourceTracker));
                        initLlvmModules();
                        const auto startSymbol = ExitOnError(llvmJit->lookup("_start"));
                        using FuncType = double (*)();
                        auto *const startFunc = startSymbol.getAddress().toPtr<FuncType>();
                        std::cout << "result=" << startFunc() << "\n";
                        ExitOnError(resourceTracker->remove());
                    }
                }
                lexer->readNextToken();
            }
        } while (lexer->getCurrentToken() != TokenType::EosToken);
    }

    void defineEmbeddedFunctions() {
        llvm::orc::MangleAndInterner mangle(llvmJit->getMainJITDylib().getExecutionSession(),
                                            llvmJit->getDataLayout());
        llvm::orc::SymbolMap symbols;

        constexpr const char *const name = "print";
        auto printProto = std::make_unique<ProtoFunctionStatement>(name, std::vector<std::string>{"param"});
        functionProtos[name] = std::move(printProto);
        symbols[mangle(name)] = {
            llvm::orc::ExecutorAddr::fromPtr<double(double)>(&print),
            llvm::JITSymbolFlags()
        };

        ExitOnError(llvmJit->getMainJITDylib().define(absoluteSymbols(std::move(symbols))));
    }

    void testParseBinExpression();

    void testParseNumber();

    void testFunctionDefinition();

    void testIdentifier();

    void testVarDefinition();

    void testIfExpression();
} // namespace

int main() {
    testParseBinExpression();
    testParseNumber();
    testFunctionDefinition();
    testIdentifier();
    testVarDefinition();
    testIfExpression();

    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvmJit = ExitOnError(llvm::orc::KaleidoscopeJIT::Create());

    initLlvmModules();

    defineEmbeddedFunctions();

    const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>(R"(
        i1 = 1;
        i2 = 2;
        print(i1+i2);
    )"));

    // auto stream = std::make_unique<std::istringstream>();
    // stream->basic_ios::rdbuf(std::cin.rdbuf());
    // const auto lexer = std::make_unique<Lexer>(std::move(stream));

    mainHandler(lexer);
    return 0;
}

namespace {
    std::string makeTestFailMsg(const std::uint32_t line) {
        return std::string("test failed, line=").append(std::to_string(line));
    }

    void testVarDefinition() {
        auto lexer = std::make_unique<Lexer>(
            std::make_unique<std::istringstream>("varName=2*(1-2);"));
        lexer->readNextToken();
        const auto varExprAst = parseIdentifier(lexer);
        if (varExprAst == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const var = dynamic_cast<VariableDefinitionStatement *>(varExprAst.get());
        if (var->name != "varName") {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        print(varExprAst.get());
        const auto *const binOp = dynamic_cast<BinOpNode *>(var->rvalue.get());
        if (binOp == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
    }

    void testFunctionDefinition() {
        const auto lexer = std::make_unique<Lexer>(
            std::make_unique<std::istringstream>("def test(id1, id2, id3) {varPtr=(1+2+id1) * (2+1+id2);}"));
        lexer->readNextToken();
        if (lexer->getCurrentToken() != TokenType::FunctionDefinitionToken) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto func = parseFunctionDefinition(lexer);
        if (func == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        print(func.get());
        if (func == nullptr || func->proto->name != "test" || func->proto->args.size() != 3) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const varPtr = dynamic_cast<VariableDefinitionStatement *>(func->body.front().get());
        if (varPtr == nullptr || varPtr->name != "varPtr" || varPtr->rvalue == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        functionProtos.clear();
        namedValues.clear();
    }

    void testParseNumber() {
        const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>(" -123.123;"));
        lexer->readNextToken();
        const auto expr = parseExpr(lexer);
        if (expr == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const numberAst = dynamic_cast<const NumberNode *>(expr.get());
        if (numberAst->value != -123.123) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        print(expr.get());
    }

    void testParseBinExpression() { {
            const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>("-1-21.2;"));
            lexer->readNextToken();
            if (lexer->getCurrentToken() != TokenType::NumberToken) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto expr = parseAstNodeItem(lexer);
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpNode *>(expr.get());
            if (binOp == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (binOp->binOp != TokenType::MinusToken) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const lhsNumber = dynamic_cast<NumberNode *>(binOp->lhs.get());
            if (lhsNumber == nullptr || lhsNumber->value != -1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const rhsNumber = dynamic_cast<NumberNode *>(binOp->rhs.get());
            if (rhsNumber == nullptr || rhsNumber->value != 21.2) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            print(expr.get());
        } {
            const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>("(2*(1+2));"));
            lexer->readNextToken();
            const auto expr = parseAstNodeItem(lexer);
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpNode *>(expr.get()); {
                const auto *const lhsNumber = dynamic_cast<NumberNode *>(binOp->lhs.get());
                if (lhsNumber == nullptr || lhsNumber->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } {
                const auto *const binOpRhs = dynamic_cast<BinOpNode *>(binOp->rhs.get());
                if (binOpRhs == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const lhs = dynamic_cast<NumberNode *>(binOpRhs->lhs.get());
                if (lhs == nullptr || lhs->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const rhs = dynamic_cast<NumberNode *>(binOpRhs->rhs.get());
                if (rhs == nullptr || rhs->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            print(expr.get());
        } {
            const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>("+1 *  (   2    +3.0);"));
            lexer->readNextToken();
            const auto expr = parseAstNodeItem(lexer);
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpNode *>(expr.get()); {
                const auto *const lhsNumber = dynamic_cast<NumberNode *>(binOp->lhs.get());
                if (lhsNumber == nullptr || lhsNumber->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            } {
                const auto *const binOpRhs = dynamic_cast<BinOpNode *>(binOp->rhs.get());
                if (binOpRhs == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const lhs = dynamic_cast<NumberNode *>(binOpRhs->lhs.get());
                if (lhs == nullptr || lhs->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const rhs = dynamic_cast<NumberNode *>(binOpRhs->rhs.get());
                if (rhs == nullptr || rhs->value != 3.0) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            print(expr.get());
        }
    }

    void testIdentifier() { {
            const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>("v+1;"));
            lexer->readNextToken();
            const auto expr = parseAstNodeItem(lexer);
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpNode *>(expr.get());
            if (binOp == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const lhs = dynamic_cast<VariableAccessNode *>(binOp->lhs.get());
            if (lhs == nullptr || lhs->name != "v") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const rhs = dynamic_cast<NumberNode *>(binOp->rhs.get());
            if (rhs == nullptr || rhs->value != 1.0) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        } {
            const auto lexer = std::make_unique<Lexer>(
                std::make_unique<std::istringstream>("foo(1, 12.1, id1, -1.2, (1+2));"));
            lexer->readNextToken();
            const auto expr = parseExpr(lexer, true);
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            print(expr.get());
            const auto *const callFunc = dynamic_cast<CallFunctionNode *>(expr.get());
            if (callFunc->callee != "foo") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (callFunc->args.size() != 5) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const number = dynamic_cast<NumberNode *>(callFunc->args[0].get()); number->value != 1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const number = dynamic_cast<NumberNode *>(callFunc->args[1].get()); number->value != 12.1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const var = dynamic_cast<VariableAccessNode *>(callFunc->args[2].get()); var->name != "id1") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const number = dynamic_cast<NumberNode *>(callFunc->args[3].get()); number->value != -1.2) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const binOp = dynamic_cast<BinOpNode *>(callFunc->args[4].get()); binOp == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
    }

    void testIfExpression() {
        const auto lexer = std::make_unique<Lexer>(std::make_unique<std::istringstream>(R"(
            if (1) {
                print(1);
            } else {
                print(0);
            }
        )"));
        lexer->readNextToken();
        const auto ifStatement = parseAstNodeItem(lexer);
        if (ifStatement == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const ifExprPtr = dynamic_cast<IfStatementStatement *>(ifStatement.get());
        if (ifExprPtr == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        if (ifExprPtr->cond == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const numberAstPtr = dynamic_cast<NumberNode *>(ifExprPtr->cond.get());
        if (numberAstPtr == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }

        if (ifExprPtr->thenBranch.empty()) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const thenFuncAstPtr = dynamic_cast<CallFunctionNode *>(ifExprPtr->thenBranch.back().get());
        if (thenFuncAstPtr == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }

        if (ifExprPtr->elseBranch) {
            if (ifExprPtr->elseBranch.value().empty()) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const elseFuncAstPtr = dynamic_cast<CallFunctionNode *>(
                ifExprPtr->elseBranch.value().back().get());
            if (elseFuncAstPtr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
    }
} // namespace
