#include <fstream>
#include <iostream>
#include <list>
#include <memory>
#include <sstream>
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

namespace {
    std::unique_ptr<llvm::LLVMContext> llvmContext;
    std::unique_ptr<llvm::Module> llvmModule;
    std::unique_ptr<llvm::IRBuilder<>> llvmIRBuilder;
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

        llvmIRBuilder = std::make_unique<llvm::IRBuilder<>>(*llvmContext);

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

    class BaseAstNode {
    public:
        virtual ~BaseAstNode() = default;

        [[nodiscard]] virtual llvm::Value *codegen() const = 0;

        [[nodiscard]] virtual std::string toString() const = 0;
    };

    class ExprAst : public BaseAstNode {
    public:
        ~ExprAst() override = default;
    };

    class NumberAst : public ExprAst {
    public:
        explicit NumberAst(double v) :
                value(v) {

        }

        [[nodiscard]] std::string toString() const override {
            return "number=" + std::to_string(value);
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            assert(llvmContext != nullptr);
            return llvm::ConstantFP::get(*llvmContext, llvm::APFloat(value));
        }

        double value;
    };

    class VariableAst : public ExprAst {
    public:
        explicit VariableAst(std::string name) :
                name(std::move(name)) {

        }

        [[nodiscard]] std::string toString() const override {
            return "var=" + name;
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            return namedValues[name];
        }

        const std::string name;
    };

    class BinOpAst : public ExprAst {
    public:
        BinOpAst(const char binOp,
                 std::unique_ptr<ExprAst> lhs,
                 std::unique_ptr<ExprAst> rhs) :
                binOp(binOp),
                lhs(std::move(lhs)),
                rhs(std::move(rhs)) {

        }

        [[nodiscard]] std::string toString() const override {
            const bool isLhsBinOp = dynamic_cast<BinOpAst *>(lhs.get()) != nullptr;
            const bool isRhsBinOp = dynamic_cast<BinOpAst *>(rhs.get()) != nullptr;
            return std::string("op=").append(1, binOp).append(", lhs=")
                    .append(isLhsBinOp ? "(" : "")
                    .append(lhs->toString())
                    .append(isLhsBinOp ? ")" : "")
                    .append(", rhs=")
                    .append(isRhsBinOp ? "(" : "")
                    .append(rhs->toString()).append(isRhsBinOp ? ")" : "");
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            assert(llvmContext != nullptr);
            auto *lhsValue = lhs->codegen();
            auto *rhsValue = rhs->codegen();
            if (lhsValue == nullptr || rhsValue == nullptr) {
                return nullptr;
            }
            if (lhsValue->getType()->isPointerTy()) {
                lhsValue = llvmIRBuilder->CreateLoad(llvm::Type::getDoubleTy(*llvmContext), lhsValue);
            }
            if (rhsValue->getType()->isPointerTy()) {
                rhsValue = llvmIRBuilder->CreateLoad(llvm::Type::getDoubleTy(*llvmContext), rhsValue);
            }

            switch (binOp) {
                case '+':
                    return llvmIRBuilder->CreateFAdd(lhsValue, rhsValue, "addtmp");
                case '-':
                    return llvmIRBuilder->CreateFSub(lhsValue, rhsValue, "subtmp");
                case '*':
                    return llvmIRBuilder->CreateFMul(lhsValue, rhsValue, "multmp");
                case '/':
                    return llvmIRBuilder->CreateFDiv(lhsValue, rhsValue, "divtmp");
                case '<':
                    lhsValue = llvmIRBuilder->CreateFCmpULT(lhsValue, rhsValue, "cmptmp");
                    // Convert bool 0/1 to double 0.0 or 1.0
                    return llvmIRBuilder->CreateUIToFP(lhsValue, llvm::Type::getDoubleTy(*llvmContext), "booltmp");
            }
            return nullptr;
        }

        const char binOp;
        const std::unique_ptr<ExprAst> lhs;
        const std::unique_ptr<ExprAst> rhs;
    };

    class VariableDefinitionAst : public ExprAst {
    public:
        VariableDefinitionAst(std::string name,
                              std::unique_ptr<ExprAst> rvalue) :
                name(std::move(name)),
                rvalue(std::move(rvalue)) {

        }

        [[nodiscard]] std::string toString() const override {
            return "var definition name=" + name + ", rvalue=" + rvalue->toString();
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            assert(llvmContext != nullptr);
            if (llvmIRBuilder->GetInsertBlock() == nullptr) {
                auto *const variable = new llvm::GlobalVariable(
                        *llvmModule,
                        llvmIRBuilder->getDoubleTy(),
                        false,
                        llvm::GlobalValue::CommonLinkage,
                        nullptr,
                        name
                );
                variable->setInitializer(reinterpret_cast<llvm::ConstantFP *>(rvalue->codegen()));
                return variable;
            }
            auto *const variable = new llvm::AllocaInst(llvmIRBuilder->getDoubleTy(), 0, name,
                                                        llvmIRBuilder->GetInsertBlock());
            llvmIRBuilder->CreateStore(rvalue->codegen(), variable);
            return variable;
        }

        const std::string name;
        const std::unique_ptr<ExprAst> rvalue;
    };

    struct ProtoFunctionAst : public BaseAstNode {
        ProtoFunctionAst(std::string name, std::vector<std::string> args) :
                name(std::move(name)),
                args(std::move(args)) {

        }

        [[nodiscard]] std::string toString() const override {
            return "proto func:" + name;
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            assert(llvmContext != nullptr);
            std::vector<llvm::Type *> functionParams(args.size(), llvm::Type::getDoubleTy(*llvmContext));
            auto *const functionType = llvm::FunctionType::get(llvm::Type::getDoubleTy(*llvmContext), functionParams,
                                                               false);
            auto *const function = llvm::Function::Create(functionType,
                                                          llvm::Function::ExternalLinkage,
                                                          name,
                                                          llvmModule.get());
            for (auto it = function->arg_begin(); it != function->arg_end(); ++it) {
                const auto index = std::distance(function->arg_begin(), it);
                it->setName(args[index]);
            }
            return function;
        }

        std::string name;
        std::vector<std::string> args;
    };

    std::unordered_map<std::string, std::unique_ptr<ProtoFunctionAst>> FunctionProtos;

    llvm::Function *getFunction(const std::string &Name) {
        // First, see if the function has already been added to the current module.
        if (auto *F = llvmModule->getFunction(Name))
            return F;

        // If not, check whether we can codegen the declaration from some existing
        // prototype.
        auto FI = FunctionProtos.find(Name);
        if (FI != FunctionProtos.end())
            return reinterpret_cast<llvm::Function *>(FI->second->codegen());

        // If no existing prototype exists, return null.
        return nullptr;
    }

    struct FunctionAst : public BaseAstNode {
        FunctionAst(std::unique_ptr<ProtoFunctionAst> proto, std::list<std::unique_ptr<ExprAst>> body) :
                proto(std::move(proto)),
                body(std::move(body)) {

        }

        [[nodiscard]] std::string toString() const override {
            return proto->toString();
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            assert(llvmContext != nullptr);
            // Transfer ownership of the prototype to the FunctionProtos map, but keep a
            // reference to it for use below.
            auto &P = *proto;
            FunctionProtos[proto->name] = std::make_unique<ProtoFunctionAst>(proto->name,
                                                                             proto->args);
            auto *const function = getFunction(P.name);
            if (function == nullptr) {
                return nullptr;
            }

            // Create a new basic block to start insertion into.
            auto *const basicBlock = llvm::BasicBlock::Create(*llvmContext, "entry", function);
            llvmIRBuilder->SetInsertPoint(basicBlock);

            // Record the function arguments in the namedValues map.
            namedValues.clear();
            for (auto &arg: function->args()) {
                namedValues[std::string(arg.getName())] = &arg;
            }

            for (auto it = body.begin(); it != body.end(); ++it) {
                if (*it == body.back()) {
                    if (auto *const retVal = (*it)->codegen()) {
                        // Finish off the function.
                        llvmIRBuilder->CreateRet(retVal);

                        // Validate the generated code, checking for consistency.
                        verifyFunction(*function);
                        return function;
                    }
                } else {
                    if (auto var = dynamic_cast<VariableDefinitionAst *>(it->get())) {
                        namedValues[var->name] = var->codegen();
                    }
                }
            }

            // Error reading body, remove function.
            function->eraseFromParent();
            return nullptr;
        }

        mutable std::unique_ptr<ProtoFunctionAst> proto;
        const std::list<std::unique_ptr<ExprAst>> body;
    };

    class CallFunctionExpr : public ExprAst {
    public:
        CallFunctionExpr(std::string callee, std::vector<std::unique_ptr<ExprAst>> args) :
                callee(std::move(callee)),
                args(std::move(args)) {

        }

        [[nodiscard]] std::string toString() const override {
            std::stringstream ss;
            ss << "call func: " << callee << "(";
            for (const auto &arg: args) {
                const bool isBinOp = dynamic_cast<BinOpAst *>(arg.get()) != nullptr;
                if (isBinOp) {
                    ss << "(";
                }
                ss << arg->toString() << ",";
                if (isBinOp) {
                    ss << ")";
                }
            }
            ss << ")";
            return ss.str();
        }

        [[nodiscard]] llvm::Value *codegen() const override {
            assert(llvmContext != nullptr);
            // Look up the name in the global module table.
            auto *calleeFunc = getFunction(callee);
            if (calleeFunc == nullptr) {
                return nullptr;
            }

            // If argument mismatch error.
            if (calleeFunc->arg_size() != args.size()) {
                return nullptr;
            }

            std::vector<llvm::Value *> argsFunc;
            for (const auto &arg: args) {
                argsFunc.push_back(arg->codegen());
                if (!argsFunc.back()) {
                    return nullptr;
                }
            }

            return llvmIRBuilder->CreateCall(calleeFunc, argsFunc, "calltmp");
        }

        const std::string callee;
        const std::vector<std::unique_ptr<ExprAst>> args;
    };

    enum class TokenType {
        EosToken,
        NumberToken,
        FunctionDefinitionToken,
        IdentifierToken,
        OtherToken,
    };

    std::unique_ptr<std::istream> stream;
    int lastChar = ' ';
    TokenType currentToken;
    std::string numberValue;
    std::string identifier;

    std::unique_ptr<ExprAst> parseExpression();

    void readNextChar() {
        do {
            lastChar = stream->get();
            if (lastChar == '\n' || !stream->eof()) {
                break;
            }
        } while (*stream);
    }

    int getPeekChar() {
        return stream->peek();
    }

    bool isSignOfNumber(const int ch) {
        return ch == '+' || ch == '-';
    }

    bool isCharOfNumber(const int ch) {
        return isdigit(ch) || ch == '.';
    }

    void parseNumber() {
        numberValue.clear();
        do {
            if (isspace(lastChar)) {
                if (ispunct(getPeekChar())) {
                    break;
                }
                readNextChar();
                continue;
            }

            if ((isSignOfNumber(lastChar) && numberValue.empty()) || isCharOfNumber(lastChar)) {
                numberValue.push_back(static_cast<char>(lastChar));
                // last symbol of number
                if (ispunct(getPeekChar()) && getPeekChar() != '.') {
                    break;
                }
                readNextChar();
            } else {
                break;
            }
        } while (*stream);
    }

    void readNextToken(const bool inExpression = false) {
        do {
            readNextChar();
        } while (isspace(lastChar));

        if (lastChar == ';') {
            do {
                readNextChar();
            } while (isspace(lastChar) && lastChar != '\n');
        }

        if (lastChar == EOF) {
            currentToken = TokenType::EosToken;
            return;
        }

        currentToken = TokenType::OtherToken;
        // parse number
        if ((isSignOfNumber(lastChar) && !inExpression) || isCharOfNumber(lastChar)) {
            currentToken = TokenType::NumberToken;
            parseNumber();
        } else {
            // parse identifiers
            if (isalpha(lastChar)) {
                identifier.clear();
                while (isalnum(lastChar)) {
                    identifier.push_back(lastChar);
                    const char p = getPeekChar();
                    if (!isalnum(p)) {
                        break;
                    }
                    readNextChar();
                }
                if (identifier == "def") {
                    currentToken = TokenType::FunctionDefinitionToken;
                } else {
                    currentToken = TokenType::IdentifierToken;
                }
            }
        }
    }

    std::unique_ptr<ExprAst> parseNumberExpr(const bool inExpression = false) {
        auto number = std::make_unique<NumberAst>(strtod(numberValue.c_str(), nullptr));
        readNextToken(inExpression);
        return number;
    }

    std::unique_ptr<ExprAst> parseParentheses() {
        if (lastChar != '(') {
            return nullptr;
        }
        readNextToken(); // eat (
        auto expr = parseExpression();
        if (lastChar != ')') {
            return nullptr;
        }
        readNextToken(); // eat )
        return expr;
    }

    std::unique_ptr<ExprAst> parseExpr(bool inExpression = false);

    std::unique_ptr<ExprAst> parseIdentifier(const bool inExpression = false) {
        const std::string name = identifier;
        readNextToken(inExpression); // eat identifier
        if (lastChar == '=') {
            readNextToken(); // eat =
            auto expr = parseExpression();
            return std::make_unique<VariableDefinitionAst>(name, std::move(expr));
        } else if (lastChar != '(') {
            return std::make_unique<VariableAst>(name);
        }

        std::vector<std::unique_ptr<ExprAst>> args;
        readNextToken(); // eat '('
        while (true) {
            if (auto arg = parseExpr()) {
                args.push_back(std::move(arg));
                if (lastChar == ',') {
                    readNextToken(); // eat ','
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        if (lastChar != ')') {
            return nullptr;
        }
        readNextToken(); // eat ')'
        return std::make_unique<CallFunctionExpr>(name, std::move(args));
    }

    std::unique_ptr<ExprAst> parseExpr(const bool inExpression) {
        if (currentToken == TokenType::NumberToken) {
            return parseNumberExpr(inExpression);
        } else if (currentToken == TokenType::IdentifierToken) {
            return parseIdentifier(inExpression);
        } else if (lastChar == '(') {
            return parseParentheses();
        }
        return nullptr;
    }

    int getBinOpPrecedence(const char binOp) {
        int binOpPrec = -1;
        if (binOp == '+' || binOp == '-') {
            binOpPrec = 1;
        } else if (binOp == '/' || binOp == '*') {
            binOpPrec = 2;
        }
        return binOpPrec;
    }

    std::unique_ptr<ExprAst> parseBinOp(const int expPrec,
                                        std::unique_ptr<ExprAst> lhs) {
        while (true) {
            const char binOp = static_cast<char>(lastChar);
            const int curBinOpPrec = getBinOpPrecedence(binOp);
            if (curBinOpPrec < expPrec) {
                return lhs;
            }

            readNextToken(true); // read rhs
            auto rhs = parseExpr(true);
            if (rhs == nullptr) {
                return nullptr;
            }

            const char nextBinOp = static_cast<char>(lastChar);
            const int nextBinOpPrec = getBinOpPrecedence(nextBinOp);
            if (curBinOpPrec < nextBinOpPrec) {
                if (rhs = parseBinOp(curBinOpPrec, std::move(rhs)); rhs == nullptr) {
                    return nullptr;
                }
            }

            lhs = std::make_unique<BinOpAst>(binOp, std::move(lhs), std::move(rhs));
        }
    }

    std::unique_ptr<ExprAst> parseExpression() {
        if (auto expr = parseExpr(true)) {
            return parseBinOp(0, std::move(expr));
        }
        return nullptr;
    }

    void print(const BaseAstNode *const nodeAst) {
        if (const auto *const llvmIR = nodeAst->codegen(); llvmIR != nullptr) {
            llvm::outs() << "IR: ";
            llvmIR->print(llvm::outs(), true);
            llvm::outs() << '\n';
        }
        std::list<BinOpAst *> values;
        auto *ptr = dynamic_cast<const BinOpAst *>(nodeAst);
        do {
            if (!values.empty()) {
                ptr = values.front();
                values.pop_front();
            }
            if (ptr == nullptr) {
                continue;
            }
            if (auto *const rhs = dynamic_cast<BinOpAst *>(ptr->rhs.get())) {
                values.push_back(rhs);
            }
            if (auto *const lhs = dynamic_cast<BinOpAst *>(ptr->lhs.get())) {
                values.push_back(lhs);
            }
            std::cout << ">" << ptr->toString() << "\n";
        } while (!values.empty());
    }

    std::unique_ptr<ProtoFunctionAst> parseProto() {
        const std::string name = identifier;
        readNextToken(); // eat callee
        if (lastChar != '(') {
            return nullptr;
        }
        readNextToken(); // eat (
        std::vector<std::string> args;
        while (*stream) {
            if (currentToken != TokenType::IdentifierToken) {
                break;
            }
            if (auto arg = parseIdentifier()) {
                const auto var = dynamic_cast<const VariableAst *>(arg.get());
                args.push_back(var->name);
                if (lastChar == ',') {
                    readNextToken(); // eat next arg
                }
            } else {
                break;
            }
        }
        if (lastChar != ')') {
            return nullptr;
        }
        readNextToken(); // eat )
        return std::make_unique<ProtoFunctionAst>(name, args);
    }

    std::unique_ptr<FunctionAst> parseFunctionDefinition() {
        readNextToken(); // eat def
        auto proto = parseProto();
        if (lastChar != '{') {
            return nullptr;
        }
        readNextToken();
        std::list<std::unique_ptr<ExprAst>> body;
        while (true) {
            if (auto expr = parseExpression()) {
                body.push_back(std::move(expr));
            } else {
                break;
            }
            if (lastChar == '}') {
                break;
            }
        }
        return std::make_unique<FunctionAst>(std::move(proto), std::move(body));
    }

    std::unique_ptr<FunctionAst> parseTopLevelExpr() {
        auto expr = parseExpression();
        if (expr == nullptr) {
            return nullptr;
        }
        print(expr.get());
        auto proto = std::make_unique<ProtoFunctionAst>("__anon_expr",
                                                        std::vector<std::string>());
        std::list<std::unique_ptr<ExprAst>> body;
        body.push_back(std::move(expr));
        return std::make_unique<FunctionAst>(std::move(proto), std::move(body));
    }

    void mainHandler() {
        readNextToken();
        do {
            if (currentToken == TokenType::FunctionDefinitionToken) {
                auto definition = parseFunctionDefinition();
                if (definition != nullptr) {
                    print(definition.get());
                }
                ExitOnError(llvmJit->addModule(
                        llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmContext)), nullptr));
                initLlvmModules();
                readNextToken();
            } else {
                auto topLevelExpr = parseTopLevelExpr();
                if (topLevelExpr != nullptr && topLevelExpr->codegen()) {
                    auto RT = llvmJit->getMainJITDylib().createResourceTracker();
                    auto TSM = llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmContext));
                    ExitOnError(llvmJit->addModule(std::move(TSM), RT));
                    initLlvmModules();
                    auto ExprSymbol = ExitOnError(llvmJit->lookup("__anon_expr"));
                    using FT = double (*)();
                    auto FP = ExprSymbol.getAddress().toPtr<FT>();
                    std::cout << "result=" << FP() << "\n";
                    ExitOnError(RT->remove());
                    readNextToken();
                }
            }
        } while (currentToken != TokenType::EosToken);
    }

    void testParseBinExpression();

    void testParseNumber();

    void testFunctionDefinition();

    void testIdentifier();

    void testVarDefinition();
}

int main() {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    llvmJit = ExitOnError(llvm::orc::KaleidoscopeJIT::Create());

    initLlvmModules();

    testParseBinExpression();
    testParseNumber();
    testFunctionDefinition();
    testIdentifier();
    testVarDefinition();

    stream = std::make_unique<std::istringstream>("def foo(v){var=1; var+v}; foo(0);foo(1);foo(1.2);");
//    stream->basic_ios::rdbuf(std::cin.rdbuf());
    mainHandler();
    return 0;
}

namespace {
    inline std::string makeTestFailMsg(const std::uint32_t line) {
        return std::string("test failed, line=").append(std::to_string(line));
    }

    void testVarDefinition() {
        stream = std::make_unique<std::istringstream>("varName=2*(1-2);");
        readNextToken();
        const auto varExprAst = parseIdentifier();
        if (varExprAst == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const var = dynamic_cast<VariableDefinitionAst *>(varExprAst.get());
        if (var->name != "varName") {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        print(varExprAst.get());
        const auto *const binOp = dynamic_cast<BinOpAst *>(var->rvalue.get());
        if (binOp == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
    }

    void testFunctionDefinition() {
        stream = std::make_unique<std::istringstream>("def test(id1, id2, id3) {varPtr=(1+2+id1) * (2+1+id2);}");
        readNextToken();
        if (currentToken != TokenType::FunctionDefinitionToken) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto func = parseFunctionDefinition();
        if (func == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        print(func.get());
        const auto *const funcPtr = dynamic_cast<FunctionAst *>(func.get());
        if (func == nullptr || funcPtr->proto->name != "test" || funcPtr->proto->args.size() != 3) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *const varPtr = dynamic_cast<VariableDefinitionAst *>(funcPtr->body.front().get());
        if (varPtr == nullptr || varPtr->name != "varPtr" || varPtr->rvalue == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        FunctionProtos.clear();
        namedValues.clear();
        ExitOnError(llvmJit->addModule(
                llvm::orc::ThreadSafeModule(std::move(llvmModule), std::move(llvmContext)), nullptr));
        initLlvmModules();
    }

    void testParseNumber() {
        {
            stream = std::make_unique<std::istringstream>(" -123.123;");
            readNextToken();
            const auto expr = parseExpr();
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const numberAst = dynamic_cast<const NumberAst *>(expr.get());
            if (numberAst->value != -123.123) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            print(expr.get());
        }
    }

    void testParseBinExpression() {
        {
            stream = std::make_unique<std::istringstream>("-1-21.2;");
            readNextToken();
            if (currentToken != TokenType::NumberToken) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto expr = parseExpression();
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpAst *>(expr.get());
            if (binOp->binOp != '-') {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const lhsNumber = dynamic_cast<NumberAst *>(binOp->lhs.get());
            if (lhsNumber == nullptr || lhsNumber->value != -1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const rhsNumber = dynamic_cast<NumberAst *>(binOp->rhs.get());
            if (rhsNumber == nullptr || rhsNumber->value != 21.2) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            print(expr.get());
        }

        {
            stream = std::make_unique<std::istringstream>("(2*(1+2));");
            readNextToken();
            const auto expr = parseExpression();
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpAst *>(expr.get());
            {
                const auto *const lhsNumber = dynamic_cast<NumberAst *>(binOp->lhs.get());
                if (lhsNumber == nullptr || lhsNumber->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            {
                const auto *const binOpRhs = dynamic_cast<BinOpAst *>(binOp->rhs.get());
                if (binOpRhs == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const lhs = dynamic_cast<NumberAst *>(binOpRhs->lhs.get());
                if (lhs == nullptr || lhs->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const rhs = dynamic_cast<NumberAst *>(binOpRhs->rhs.get());
                if (rhs == nullptr || rhs->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            print(expr.get());
        }

        {
            stream = std::make_unique<std::istringstream>("+1 *  (   2    +3.0);");
            readNextToken();
            const auto expr = parseExpression();
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpAst *>(expr.get());
            {
                const auto *const lhsNumber = dynamic_cast<NumberAst *>(binOp->lhs.get());
                if (lhsNumber == nullptr || lhsNumber->value != 1) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            {
                const auto *const binOpRhs = dynamic_cast<BinOpAst *>(binOp->rhs.get());
                if (binOpRhs == nullptr) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const lhs = dynamic_cast<NumberAst *>(binOpRhs->lhs.get());
                if (lhs == nullptr || lhs->value != 2) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
                const auto *const rhs = dynamic_cast<NumberAst *>(binOpRhs->rhs.get());
                if (rhs == nullptr || rhs->value != 3.0) {
                    throw std::logic_error(makeTestFailMsg(__LINE__));
                }
            }
            print(expr.get());
        }
    }

    void testIdentifier() {
        {
            stream = std::make_unique<std::istringstream>("v+1;");
            readNextToken();
            const auto expr = parseExpression();
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const binOp = dynamic_cast<BinOpAst *>(expr.get());
            if (binOp == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const lhs = dynamic_cast<VariableAst *>(binOp->lhs.get());
            if (lhs == nullptr || lhs->name != "v") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *const rhs = dynamic_cast<NumberAst *>(binOp->rhs.get());
            if (rhs == nullptr || rhs->value != 1.0) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            stream = std::make_unique<std::istringstream>("foo(1, 12.1, id1, -1.2, (1+2));");
            readNextToken();
            const auto expr = parseExpr(true);
            if (expr == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            print(expr.get());
            const auto *const callFunc = dynamic_cast<CallFunctionExpr *>(expr.get());
            if (callFunc->callee != "foo") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (callFunc->args.size() != 5) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const number = dynamic_cast<NumberAst *>(callFunc->args[0].get()); number->value != 1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const number = dynamic_cast<NumberAst *>(callFunc->args[1].get()); number->value != 12.1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const var = dynamic_cast<VariableAst *>(callFunc->args[2].get()); var->name != "id1") {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const number = dynamic_cast<NumberAst *>(callFunc->args[3].get()); number->value != -1.2) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            if (auto *const binOp = dynamic_cast<BinOpAst *>(callFunc->args[4].get()); binOp == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
    }
}