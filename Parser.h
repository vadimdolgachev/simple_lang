#ifndef PARSER_H
#define PARSER_H

#include <memory>

#include "Lexer.h"
#include "ast/BaseNode.h"
#include "ast/NumberNode.h"

/*
<Declaration> ::= <Identifier> <Initialization>
               | <FunctionDefinition>

<Identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*

<Initialization> ::= "=" <Expression>
                 | ε

<Parameters> ::= <Parameter> "," <Parameters>
             | <Parameter>
             | ε

<Parameter> ::= <Identifier>

<Block> ::= "{" <Declarations> "}"

<Declarations> ::= <Declaration> | <Declaration> <Declarations>

<Assignment> ::= <Identifier> "=" <Expression> ";"

<Expression> ::= <Literal>
             | <Identifier>
             | <FunctionCall>
             | <Expression> <AddOp> <Term>
             | <Expression> "++"
             | <Expression> "--"
             | <Term>

<FunctionCall> ::= <Identifier> "(" <Arguments> ")"

<Arguments> ::= <Expression> "," <Arguments>
             | <Expression>
             | ε

<AddOp> ::= "+" | "-"

<Literal> ::= <Integer>

<Integer> ::= [0-9]+

<Term> ::= <Factor>
        | <Term> <MulOp> <Factor>

<MulOp> ::= "*" | "/"

<Factor> ::= "(" <Expression> ")"
         | <Identifier>
         | <Number>
         | <FunctionCall>
         | <PrefixIncDec> <Identifier>
         | <Identifier> <PostfixIncDec>

<PrefixIncDec> ::= "++" | "--"

<PostfixIncDec> ::= "++" | "--"

<Statement> ::= "if" "(" <Expression> ")" <Block> ("else" <Block>)?
             | "while" "(" <Expression> ")" <Block>
             | "for" "(" <ForLoopInit> ";" <Expression> ";" <Expression> ")" <Block>
             | <Expression> ";"
             | <FunctionDefinition>

<FunctionDefinition> ::= "fn" <Identifier> "(" <Parameters> ")" <Block>

<ForLoopInit> ::= <Assignment>
              | <Declaration>
*/

class Parser {
public:
    explicit Parser(std::unique_ptr<Lexer> lexer);

    explicit operator bool() const;

    [[nodiscard]] std::unique_ptr<BaseNode> parseNextNode();

private:
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseExpr();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parsePrimary();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseFactor();
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseTerm();
    [[nodiscard]] std::unique_ptr<IdentNode> parseIdent() const;
    [[nodiscard]] std::unique_ptr<BaseNode> parseVarDefinition(std::string identName);
    [[nodiscard]] std::unique_ptr<NumberNode> parseNumber() const;
    [[nodiscard]] std::unique_ptr<ExpressionNode> parseFunctionCall(std::unique_ptr<IdentNode> ident);
    [[nodiscard]] std::unique_ptr<BaseNode> parseFunctionDef();

    std::unique_ptr<Lexer> lexer;
};

#endif // PARSER_H
