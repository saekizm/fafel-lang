# Tech Specs

## Introduction

Fafel is a functional programming language designed for writing smart contracts for the Ethereum Virtual Machine (EVM). It aims to provide an expressive, safe, and efficient way to define contract logic using a syntax that is familiar to functional programmers.

The purpose of this document is to provide a detailed specification of the Fafel language, covering its syntax, semantics, type system, built-in types and functions, and the compilation process to Yul, the low-level intermediate language of the EVM. This document is intended for developers who want to write smart contracts in Fafel, as well as for tool builders who want to implement Fafel compilers or other related tools.

This document assumes a basic understanding of functional programming concepts and Ethereum smart contracts. We recommend that readers have some familiarity with Haskell, which is the language used to implement the Fafel compiler, and with the Solidity language, which is the most widely used language for writing EVM contracts.

The remainder of this document is organized as follows:

* Section 1 provides an overview of the Fafel language and its design goals.
* Section 2 defines the syntax and grammar of the Fafel language.
* Section 3 describes the semantics of the Fafel language, including its evaluation rules and operational semantics.
* Section 4 specifies the type system of Fafel, including the built-in types and functions and the rules for type checking and type inference.
* Section 5 explains the compilation process from Fafel to Yul, including the intermediate representations used and the optimizations applied.
* Section 6 provides examples of Fafel code and their corresponding Yul code to illustrate the translation process and the features of the language.
* Section 7 lists the differences between Fafel and Solidity, as well as the limitations of the language and the future work planned.

The goal of this document is to provide a comprehensive and accurate specification of the Fafel language, to enable developers to write secure and efficient smart contracts for the EVM, and to foster the growth of the Fafel community and ecosystem.


## Overview

Fafel is a functional programming language that is designed to provide an expressive, safe, and efficient way to define smart contract logic for the Ethereum Virtual Machine (EVM). It is written in
Haskell and takes a lot of design inspiration from the language.

The primary design goals of Fafel are:


1. Simplicity: Fafel aims to be a simple to understand smart contract language.

2. Functional: There doesn't seem to be any functional languages targeting the EVM, and we are on a mission to change this.

3. Gas Efficient: Fafel compiles to Yul, the low level assembly-like language for the EVM. In the future, figuring out how to optimise gas efficiency will be a key part in making Fafel a top
smart contract language.


Fafel is a statically typed language, meaning that the types of expressions and return types are checked at compile time.

Fafel is compiled to Yul, which is a low-level intermediate language for the EVM. Yul is a stack-based language that is similar in syntax to assembly language, but with higher-level constructs such as functions, variables, and loops.

In the next section, we will define the syntax and grammar of the Fafel language.

## Syntax

This section defines the syntax and grammar of the Fafel language. The syntax is defined using Extended Backus-Naur Form (EBNF), which describes the valid sequences of tokens and nonterminals that make up Fafel contracts.

## Lexical Structure
The lexical structure of Fafel is designed to follow similar patterns as other C-like languages. It consists of the following basic elements: keywords, operators, literals, identifiers, and comments.

#### Keywords
Fafel has a set of keywords that are reserved and cannot be used as identifiers. These keywords are:

`if, else, for, while, return, state, mapping, int, float, bool, address`

#### Operators
Fafel supports a range of binary and unary operators. The following binary operators are supported:   
`+, -, *, /, <, <=, >, >=, ==, &&, ||, :=`

#### Literals
Fafel supports four types of literals: integer, floating point, boolean, and hexadecimal address literals. An integer literal is a sequence of digits, a floating point literal is a decimal number, a boolean literal is either true or false, and a hexadecimal address literal is a sequence of exactly 40 hexadecimal digits, preceded by 0x.

#### Identifiers
An identifier is a sequence of letters, digits, and underscores that must start with a letter or an underscore. Fafel is a case-sensitive language, so a and A are different identifiers.

#### Comments
Fafel supports two types of comments: single-line comments and multi-line comments. Single-line comments begin with // and continue until the end of the line. Multi-line comments begin with /* and end with */.

#### Grammar
```
<contract> ::= "fafel" <identifier> "{" <state-type> {<function>}"}"

<state-type> ::= "state" "{" <state-variable> {"," <state-variable>} "}"

<state-variable> ::= <identifier> ":" <data-type>

<function> ::= <function-decl><function-expr>

<function-decl> ::= <identifier> ":" "(" {<data-type> ->} ")" <data-type>

<function-expr> ::= <identifier> "(" {identifier ","} ")" "=" <expr>

<function-call> ::= <identifier> "(" {<identifier>} ")"

<data-type> ::= <literal> | <list-type> | <map-type> | <state-type>

<list-type> ::= "[" {<literal> ","} "]"

<map-type> ::= "mapping" "("<literal> "->" <literal> ")"

<expr> ::= <literal> | <function-call> | <function-expr> | <if-expr> | <binary-expr>

<literal> ::= <integer> | <float> | <bool> | <address>

<if-expr> ::= "if" <expr> "then" <expr> "else" <expr>

<binary-expr> ::= <expr> <binary-operator> <expr>

<binary-operator> ::= "+" | "-" | "*" | "/" | "and" | "or" | "<" | ">" | "<=" | ">=" | "==" | "!="

<comment> ::= "--" {<any character>}

<identifier> ::= <letter> {<letter> | <digit>}

<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m"
| "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<address> ::= "0x" {<hexadecimal-digit>}

<hexadecimal-digit> ::= <digit> | "a" | "b" | "c" | "d" | "e" | "f"

<bool> ::= "true" | "false"
```
## Semantics

#### Overview

This section will cover the semantics of the Fafel programming language. Semantics are the meaning of programming language constructs and how they behave in different scenarios. This chapter will cover the basic building blocks of the Fafel language and describe how they function.

#### Data Types

Fafel has the following data types:

`int`: integers  
`float`: floating point numbers  
`bool`: boolean values  
`address`: hexadecimal numbers prefixed with '0x'  
`state`: type to represent the state variables of a contract  

Ints and floats are represented the same under the hood, as an `integer * 10^18`. This allows us to have the specificity for floating point numbers. However, in Fafel we still write floats in decimal form. Operations on ints and floats will return a float.

#### State

State can be thought of as a direct mapping to evm storage. We declare our state variables and write functions which can use the values or modify them depending on the return type.

#### Functions

Functions in Fafel are quite simple, and similar to Haskell in style. Functions in Fafel are an abstraction over an expression, with typed parameters. Currently, Fafel only supports one expression per function, but this will change eventually. In the future, ideally we would like to have pattern matching and other control flow expressions such as Haskell has.

#### Expressions

Expressions in Fafel are built up of operations on literals and different expression types. Fafel supports logical, comparison and binary operations on literals, as well as having separate constructs for getting values from lists or mappings, function calls, if expressions and so on. Expressions are the meat of a fafel program. 


## Type System

Fafel is a statically typed language, meaning that the type of each variable is known at compile time. Fafel's type system is designed to prevent runtime type errors by ensuring that each operation performed on variables is type-safe. Fafel uses a type inference system, which means that the type of each variable is inferred based on its usage.

Fafel's type system is implemented in the TypeChecker module. This module provides a SymbolTable data structure that keeps track of the type of each variable in the program. The SymbolTable is a map from variable names to a Symbol, which can either be a single DataType or a map from parameter names to their types. The DataType type is an enumeration that includes the basic types supported by Fafel: IntType, FloatType, BoolType, AddressType, StateType, ListType, and MapType.

The type checking process is performed by two functions: insertStateVars and insertFunctions. The insertStateVars function takes a list of StateVariable objects and adds them to the SymbolTable. For each StateVariable, the function extracts the variable name and type and inserts them into the SymbolTable as a single DataType.

The insertFunctions function takes a list of Function objects and adds them to the SymbolTable. For each Function, the function extracts the function name, argument types, and return type and inserts them into the SymbolTable as a map. The map contains the argument names as keys and their types as values.

The typecheckFunction function is responsible for type checking a Function object. The function extracts the argument names and types from the function body and adds them to the SymbolTable. Then it type checks the function body and returns the result. The typecheckExpr function is responsible for type checking an Expr object. It supports function calls, binary expressions, and function expressions.

Fafel's type system supports basic arithmetic and logical operations on integers and floats. The binary expressions supported by Fafel include addition, subtraction, multiplication, and division. If the operands of a binary expression are of the same type, the result is of the same type. If the operands are of different types, the result is promoted to the larger type.

Fafel's type system also supports lists and maps. Lists and maps can contain any type of value, but all the values in a list or map must be of the same type.

In summary, Fafel's type system is designed to prevent runtime type errors by ensuring that each operation performed on variables is type-safe. Fafel uses a type inference system that infers the type of each variable based on its usage. The TypeChecker module provides a SymbolTable data structure that keeps track of the type of each variable in the program. The type checking process is performed by the insertStateVars, insertFunctions, typecheckFunction, and typecheckExpr functions. Fafel's type system supports basic arithmetic and logical operations on integers and floats, as well as lists and maps.

## Fafel Compiler

#### Overview

The Fafel Compiler is broken up into five files:

1. `AST : This file is used to define the nodes of the Abstract Syntax Tree.`
2. `Lexer : This file is used to define the tokens of the language.`
3. `Parser : This file is used to define the functions for parsing source code.`
4. `TypeChecker : This file is used to ensure that fafel contracts are type safe.`
5. `Codegen : This file is used to turn the AST into Yul code.`
6. `Main : Entry point which takes the files and runs the compilation process.`

The compiler handles compilation from Fafel source to Yul. From there, we use solc to translate the Yul code into bytecode.

The compiler also generates a deploy script which you use to deploy the contract. You can call it with `--deploy` and you can also send txs with `--send`.

Running the fafel compiler calls the main function in `Main.hs`. This begins with our AST definitions in `AST.hs` and a rough language definitions in our `Lexer.hs` file. The actual work begins with our `Parser.hs` file. Here, we start checking every line of the fafel source code, making sure that everything is syntactically correct and parsing correctly. Compilation will fail if we run into a `parseError`.

Once we get passed the parse successfully, our `Parser.hs` file will have returned a `Abstract Syntax Tree` with all the relevant information about the program separated into nodes on a tree. From here, we begin our typechecking with `TypeChecker.hs`. We use a symbol table to store all state variables and function information, and then we run a pass over the functions and check against the symbol table as we evaluate expressions and return types. Compilation will fail if typechecking fails, however, a list of errors will be returned to the console for the developer to identify and debug said functions.  

Once we have successfully typechecked the file, we then move onto `Codegen.hs`. The code generator file takes in the AST and performs the translation to Yul code. Again, we use a symbol table to store any relevant data we need to refer back to when generating the Yul code.

From here, we use the `solc` compiler to translate this Yul IR to bytecode. 




## Example Code

## Comparisons