#include <chrono>
#include <fstream>
#include <iostream>
#include <list>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>

class ExprAst {
public:
    virtual ~ExprAst() = default;

    [[nodiscard]] virtual std::string toString() const = 0;
};

class NumberAst : public ExprAst {
public:
    explicit NumberAst(double v) :
            value(v) {

    }

    [[nodiscard]] std::string toString() const override {
        return std::to_string(value);
    }

    const double value;
};

class VariableAst : public ExprAst {
public:
    explicit VariableAst(std::string name) :
            name(std::move(name)) {

    }

    [[nodiscard]] std::string toString() const override {
        return name;
    }

    const std::string name;
};

class VariableDefinitionAst : public ExprAst {
public:
    VariableDefinitionAst(std::string var,
                          std::unique_ptr<ExprAst> expr) :
            name(std::move(var)),
            expr(std::move(expr)) {

    }

    [[nodiscard]] std::string toString() const override {
        return "var definition name=" + name + ", expr=" + expr->toString();
    }

    const std::string name;
    const std::unique_ptr<ExprAst> expr;
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

    const char binOp;
    const std::unique_ptr<ExprAst> lhs;
    const std::unique_ptr<ExprAst> rhs;
};

struct ProtoFunctionAst : public ExprAst {
    ProtoFunctionAst(std::string name, std::vector<std::unique_ptr<ExprAst>> args) :
            name(std::move(name)),
            args(std::move(args)) {

    }

    [[nodiscard]] std::string toString() const override {
        return "proto func:" + name;
    }

    const std::string name;
    const std::vector<std::unique_ptr<ExprAst>> args;
};

struct FunctionAst : public ExprAst {
    FunctionAst(std::unique_ptr<ProtoFunctionAst> protoAst, std::unique_ptr<ExprAst> body) :
            protoAst(std::move(protoAst)),
            body(std::move(body)) {

    }

    [[nodiscard]] std::string toString() const override {
        return protoAst->toString();
    }

    const std::unique_ptr<ProtoFunctionAst> protoAst;
    const std::unique_ptr<ExprAst> body;
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

    const std::string callee;
    const std::vector<std::unique_ptr<ExprAst>> args;
};

enum class TokenType {
    EosToken,
    NumberToken,
    DefinitionToken,
    IdentifierToken,
    OperatorToken,
    OtherToken,
};

std::unique_ptr<std::basic_istream<char>> stream;
int lastChar = ' ';
TokenType currentToken;
std::string numberValue;
std::string identifier;

std::unique_ptr<ExprAst> parseExpression();

void readNextChar() {
    do {
        lastChar = stream->get();
        std::cout << "read char:" << static_cast<char>(lastChar) << "\n";
        if (!stream->eof()) {
            break;
        }
    } while (*stream);
}

int getPeekChar() {
    return stream->peek();
}

void parseNumber() {
    numberValue.clear();
    do {
        if (isdigit(lastChar) || lastChar == '.' || (lastChar == '-' && numberValue.empty())) {
            numberValue.push_back(lastChar);
            const char next = getPeekChar();
            if (!(isdigit(next) || next == '.')) {
                break;
            }
            readNextChar();
        } else {
            break;
        }
    } while (*stream);
}

void printCurrentToken() {
    std::string text;
    switch (currentToken) {
        case TokenType::IdentifierToken:
            text = "identifier=" + identifier;
            break;
        case TokenType::EosToken:
            text = "eos";
            break;
        case TokenType::NumberToken:
            text = "number=" + numberValue;
            break;
        case TokenType::DefinitionToken:
            text = "definition=" + identifier;
            break;
        case TokenType::OtherToken:
            text = "OtherToken=";
            break;
        default:
            text = "unknown token";
    }
    std::cout << "read token: " << text << ", lastChar=" << static_cast<char>(lastChar) << "\n";
}

void readNextToken(const bool inExpression = false) {
    do {
        readNextChar();
    } while (isspace(lastChar));

    if (lastChar == ';') {
        do {
            readNextChar();
        } while (isspace(lastChar));
    }

    if (lastChar == EOF) {
        currentToken = TokenType::EosToken;
        printCurrentToken();
        return;
    }

    currentToken = TokenType::OtherToken;
    // parse number
    const int peek = getPeekChar();
    if (((lastChar == '-' && !inExpression) && (isdigit(peek) || peek == '.'))
        || isdigit(lastChar) || lastChar == '.') {
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
                currentToken = TokenType::DefinitionToken;
            } else {
                currentToken = TokenType::IdentifierToken;
            }
        }
    }
    printCurrentToken();
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

std::unique_ptr<ExprAst> parseElement(bool inExpression = false);

std::unique_ptr<ExprAst> parseIdentifier() {
    const std::string name = identifier;
    readNextToken();
    if (lastChar == '=') {
        readNextToken();
        auto expr = parseExpression();
        return std::make_unique<VariableDefinitionAst>(name, std::move(expr));
    }
    if (lastChar != '(') {
        return std::make_unique<VariableAst>(name);
    }

    std::vector<std::unique_ptr<ExprAst>> args;
    readNextToken();
    while (true) {
        if (auto arg = parseElement()) {
            args.push_back(std::move(arg));
            readNextToken();
        } else {
            break;
        }
    }

    return std::make_unique<CallFunctionExpr>(name, std::move(args));
}

std::unique_ptr<ExprAst> parseElement(const bool inExpression) {
    if (currentToken == TokenType::NumberToken) {
        return parseNumberExpr(inExpression);
    } else if (currentToken == TokenType::IdentifierToken) {
        return parseIdentifier();
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
        auto rhs = parseElement(true);
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
    if (auto expr = parseElement(true)) {
        return parseBinOp(0, std::move(expr));
    }
    return nullptr;
}

void printExpr(const std::unique_ptr<ExprAst> &expr) {
    std::list<BinOpAst *> values;
    std::cout << "expr: " << expr->toString() << std::endl;
    auto ptr = dynamic_cast<BinOpAst *>(expr.get());
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
    std::vector<std::unique_ptr<ExprAst>> args;
    while (*stream) {
        auto arg = parseElement();
        if (arg != nullptr) {
            args.push_back(std::move(arg));
            if (lastChar == ',') {
                readNextToken();
            }
        } else {
            break;
        }
    }
    if (lastChar != ')') {
        return nullptr;
    }
    readNextToken(); // eat )
    return std::make_unique<ProtoFunctionAst>(name, std::move(args));
}

std::unique_ptr<ExprAst> parseDefinition() {
    readNextToken(); // eat def
    auto proto = parseProto();
    if (lastChar != '{') {
        return nullptr;
    }
    readNextToken();
    auto body = parseExpression();
    if (lastChar != '}') {
        return nullptr;
    }
    printExpr(body);
    return std::make_unique<FunctionAst>(std::move(proto), std::move(body));
}

void mainHandler() {
    do {
        readNextToken();

        if (currentToken == TokenType::DefinitionToken) {
            auto expr = parseDefinition();
            if (expr != nullptr) {
                printExpr(expr);
            }
        } else {
            auto expr = parseExpression();
            if (expr == nullptr) {
                continue;
            }
            printExpr(expr);
        }
    } while (currentToken != TokenType::EosToken);
}

void testParseBinExpression();

void testParseNumber();

void testDefinition();

void testIdentifier();

void testVarDefinition();

int main() {
    testParseBinExpression();
    testParseNumber();
    testDefinition();
    testIdentifier();
    testVarDefinition();
    return 0;
}

inline std::string makeTestFailMsg(const std::uint32_t line) {
    return std::string("test failed, line=").append(std::to_string(line));
}

void testVarDefinition() {
    stream = std::make_unique<std::istringstream>("varName=2*(1-2);");
    readNextToken();
    const auto varExprAst = parseIdentifier();
    printExpr(varExprAst);
    const auto *const var = dynamic_cast<VariableDefinitionAst *>(varExprAst.get());
    if (var == nullptr || var->name != "varName") {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
    const auto *const binOp = dynamic_cast<BinOpAst *>(var->expr.get());
    if (binOp == nullptr) {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
}

void testDefinition() {
    stream = std::make_unique<std::istringstream>("def foo(id1, (12.1+1), id2);");
    readNextToken();
    if (currentToken != TokenType::DefinitionToken) {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
    readNextToken(); // eat def
    auto proto = parseProto();
    if (proto == nullptr || proto->name != "foo" || proto->args.size() != 3) {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
}

void testParseNumber() {
    {
        stream = std::make_unique<std::istringstream>(" -123.123;");
        readNextToken();
        const auto expr = parseElement();
        const auto *numberAst = dynamic_cast<const NumberAst *>(expr.get());
        if (numberAst == nullptr || numberAst->value != -123.123) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        printExpr(expr);
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
        const auto *binOp = dynamic_cast<BinOpAst *>(expr.get());
        if (binOp == nullptr || binOp->binOp != '-') {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *lhsNumber = dynamic_cast<NumberAst *>(binOp->lhs.get());
        if (lhsNumber == nullptr || lhsNumber->value != -1) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        const auto *rhsNumber = dynamic_cast<NumberAst *>(binOp->rhs.get());
        if (rhsNumber == nullptr || rhsNumber->value != 21.2) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        printExpr(expr);
    }

    {
        stream = std::make_unique<std::istringstream>("(2*(1+2));");
        readNextToken();
        const auto expr = parseExpression();
        const auto *binOp = dynamic_cast<BinOpAst *>(expr.get());
        if (binOp == nullptr) {
            throw std::logic_error(makeTestFailMsg(__LINE__));
        }
        {
            const auto *lhsNumber = dynamic_cast<NumberAst *>(binOp->lhs.get());
            if (lhsNumber == nullptr || lhsNumber->value != 2) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        {
            const auto *binOpRhs = dynamic_cast<BinOpAst *>(binOp->rhs.get());
            if (binOpRhs == nullptr) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *lhs = dynamic_cast<NumberAst *>(binOpRhs->lhs.get());
            if (lhs == nullptr || lhs->value != 1) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
            const auto *rhs = dynamic_cast<NumberAst *>(binOpRhs->rhs.get());
            if (rhs == nullptr || rhs->value != 2) {
                throw std::logic_error(makeTestFailMsg(__LINE__));
            }
        }
        printExpr(expr);
    }
}

void testIdentifier() {
    stream = std::make_unique<std::istringstream>("foo(1, 12.1, id1, -1.2, (1+2));");
    readNextToken();
    auto expr = parseElement(true);
    if (expr == nullptr) {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
    printExpr(expr);
    const auto *const callFunc = dynamic_cast<CallFunctionExpr *>(expr.get());
    if (callFunc == nullptr || callFunc->callee != "foo") {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
    if (callFunc->args.size() != 5) {
        throw std::logic_error(makeTestFailMsg(__LINE__));
    }
    if (auto *number = dynamic_cast<NumberAst *>(callFunc->args[0].get()); number->value != 1) {
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