## Fafel User Manual

#### Introduction

Fafel is a rudimentary functional smart contract programming language for the Ethereum Virtual Machine. Fafel is written in Haskell and takes a lot of inspiration from it's syntax and programming style. It compiles to the intermediate representation language, Yul.

#### Installation

To install the Fafel Compiler, clone the repository, open up your terminal and run:

`cabal build`

then, run:

`cabal install`

Once you've done that, you can call the `fafel` compiler to compile your fafel code:

`fafel example.fafel`

Should you wish to deploy a compiled fafel contract:

`fafel example.fafel --deploy`

## Writing Fafel Contracts

Fafel contracts are defined with the keyword: `fafel`

example:

```
fafel {
    state {
        value : int
    }

    getValue : () -> int
    getValue = value
}
```

A fafel contract can be thought of as a list of state variables and a list of function definitions. We can access these variables through functions, but to make changes to state, we must return a state. If we want to just return values of the state, then the return type must be of that state variable type.

### Data Types

Fafel has the following data types:

`int`: integers  
`float`: floating point numbers  
`bool`: boolean values  
`address`: hexadecimal numbers prefixed with '0x'  
`state`: type to represent the state variables of a contract  

Fafel also has the following compound data types:  

`[type]`: this represents a list of type `type`  
`mapping(type->type)`: this represents a mapping of `type` to `type`. Note that the return type for this is the second `type`.

### Functions

Functions is Fafel are quite similar to functions in Haskell. The first line is of form:

`funcName : (argType, argType2) -> returnType`

where `funcName` is the function name, `(argType, argType2)` is the types of the function inputs
and `returnType` is the type of the function output.  


After we define this line, we then write out what our function does:

`funcName x y = x + y`

All together, a function definition would look like:

```
add : (int, int) -> int
add x y = x + y
```
As of time of writing, there can only be one function expression per function.

### Operations

Fafel supports the following operations:

`Arithmetic operators: +, -, *, /`  
`Comparison operators: ==, <, >, <=, >=`  
`Boolean operators: &&, ||`
`State Assignment operator: :=`  

We can perform arithmetic and comparison operations on ints and floats, and use boolean operators to create boolean expressions. The state assignment operator is used to modify a state variable.

### State

Fafel contracts have two parts: A `state` and some amount of function definitions.  
The `state` is used to represent the overall state of the fafel contract at any given time.  

All functions take state implicitly, so you can think of every function with a signature like `func : (int) -> int` to really be a function with a signature like `func : (state -> int) -> int`  
To make changes to the state however, a function must return a state. To make changes to a state variables, you need to use the `:=` operator and your function must return a `state` type.

State is defined as so at the top of a contract:

```
state {
    name : type
    name : type
}
```

he state is made up of state variables which can only be altered by state modifying functions.


### Control Flow

Fafel control flow consists of a simple `if-then-else` expression:

`if bool then this else this`

where the expression after the `if` keyword is a boolean expression, upon evaluation which leads to the execution of either the `then` expression or the `else` expression.

### Limitations

Fafel is under constant development and as of time of writing is in its very first iterations, so bugs and missing functionality is expected. 

Currently, there is no way to call other contracts in fafel code.

