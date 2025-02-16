# SimpleASTParser


## BNF grammar:
```text
<Program> ::= <FunctionDefinition>*

(* Declarations *)
<Declaration> ::= <Identifier> <Initialization> ";"
| <FunctionDefinition>

<Initialization> ::= "=" <Expression> | ε

(* Functions *)
<FunctionDefinition> ::= "fn" <Identifier> "(" <Parameters> ")" <Block>

<Parameters> ::= <Parameter> ("," <Parameter>)* | ε
<Parameter> ::= <Identifier>

(* Blocks and Statements *)
<Block> ::= "{" <Statement>* "}"

<Statement> ::= "if" "(" <Expression> ")" <Block> ("else" <Block>)?
              | "while" "(" <Expression> ")" <Block>
              | "do" <Block> "while" "(" <Expression> ")" ";"
              | "for" "(" <ForLoopInit> ";" <Expression> ";" <Expression> ")" <Block>
              | "return" <Expression>? ";"
              | <Expression> ";"
              | <Declaration>
              | <Assignment> ";"

<ForLoopInit> ::= <AssignmentExpr> | <Declaration>

(* Expressions with Operator Precedence *)
<Expression> ::= <BoolLogicExpr>

<BoolLogicExpr> ::= <ComparisonExpr> ( ("&&" | "||") <ComparisonExpr> )*
<ComparisonExpr> ::= <AdditiveExpr> ( ("<" | "<=" | ">" | ">=" | "==" | "!=") <AdditiveExpr> )*
<AdditiveExpr> ::= <Term> ( ("+" | "-") <Term> )*
<Term> ::= <Factor> ( ("*" | "/") <Factor> )*
<Factor> ::= "(" <Expression> ")"
           | <UnaryOp> <Factor>
           | <Literal>
           | <Identifier>
           | <FunctionCall>
           | <PrefixIncDec> <Identifier>
           | <Identifier> <PostfixIncDec>

(* Unary Operators *)
<UnaryOp> ::= "!" | "+" | "-"

(* Increment/Decrement *)
<PrefixIncDec> ::= "++" | "--"
<PostfixIncDec> ::= "++" | "--"

(* Assignment *)
<AssignmentExpr> ::= <Identifier> "=" <Expression>
<Assignment> ::= <Identifier> "=" <Expression>

(* Function Calls *)
<FunctionCall> ::= <Identifier> "(" <Arguments> ")"
<Arguments> ::= <Expression> ("," <Expression>)* | ε

(* Literals *)
<Literal> ::= <Integer> | <Boolean> | <String>
<Integer> ::= [0-9]+
<Boolean> ::= "true" | "false"
<String> ::= "\"" ( [^"\\\n] | "\\" [btnfr"\\] )* "\""

(* Identifiers *)
<Identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
```

Base on tutorial https://llvm.org/docs/tutorial/MyFirstLanguageFrontend