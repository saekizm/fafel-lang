# Tech Specs

## Introduction

Fafel is a functional programming language designed for writing smart contracts for the Ethereum Virtual Machine (EVM). It aims to provide an expressive, safe, and efficient way to define contract logic using a syntax that is familiar to functional programmers.

The purpose of this document is to provide a detailed specification of the Fafel language, covering its syntax, semantics, type system, built-in types and functions, and the compilation process to Yul, the low-level intermediate language of the EVM. This document is intended for developers who want to write smart contracts in Fafel, as well as for tool builders who want to implement Fafel compilers or other related tools.

This document assumes a basic understanding of functional programming concepts and Ethereum smart contracts. We recommend that readers have some familiarity with Haskell, which is the language used to implement the Fafel compiler, and with the Solidity language, which is the most widely used language for writing EVM contracts.

The remainder of this document is organized as follows:

Section 1 provides an overview of the Fafel language and its design goals.
Section 2 defines the syntax and grammar of the Fafel language.
Section 3 describes the semantics of the Fafel language, including its evaluation rules and operational semantics.
Section 4 specifies the type system of Fafel, including the built-in types and functions and the rules for type checking and type inference.
Section 5 explains the compilation process from Fafel to Yul, including the intermediate representations used and the optimizations applied.

Section 6 provides examples of Fafel code and their corresponding Yul code to illustrate the translation process and the features of the language.
Section 7 lists the differences between Fafel and Solidity, as well as the limitations of the language and the future work planned.

The goal of this document is to provide a comprehensive and accurate specification of the Fafel language, to enable developers to write secure and efficient smart contracts for the EVM, and to foster the growth of the Fafel community and ecosystem.


## Overview

Fafel is a functional programming language that is designed to provide an expressive, safe, and efficient way to define smart contract logic for the Ethereum Virtual Machine (EVM). It is written in
Haskell and takes a lot of design inspiration from the language.

The primary design goals of Fafel are:

    1. Simplicity: Fafel aims to be a simple to understand smart contract language.
    2. Functional: There doesn't seem to be any functional languages targeting the EVM, and we are on a mission to change this.
    3. Gas Efficient: Fafel compiles to Yul, the low level assembly-like language for the EVM. In the future, figuring out how to optimise gas efficiency will be a key part in making Fafel a top
    smart contract language.

Fafel is a statically typed language, meaning that the types of expressions are return values are checked at compile time.

Fafel is compiled to Yul, which is a low-level intermediate language for the EVM. Yul is a stack-based language that is similar in syntax to assembly language, but with higher-level constructs such as functions, variables, and loops.

In the next section, we will define the syntax and grammar of the Fafel language.

## Syntax

This section defines the syntax and grammar of the Fafel language. The syntax is defined using Extended Backus-Naur Form (EBNF), which describes the valid sequences of tokens and nonterminals that make up Fafel contracts.

## Lexical Structure

Fafel programs consist of a sequence of Unicode characters, which are divided into tokens according to the following rules:

White space (spaces, tabs, and newlines) is ignored except to separate tokens.
Comments start with -- and continue until the end of the line. Comments are ignored by the compiler.
Identifiers are used to name variables, functions, and other constructs. An identifier starts with a letter or underscore, followed by zero or more letters, digits, or underscores. Identifiers are case-sensitive.
Keywords are reserved words that have a special meaning in the language. The following keywords are reserved in Fafel and cannot be used as identifiers: contract, function, if, else, true, false, state, int, float, bool, address, list, map, and, or, not, in.
Literals are values that can appear in expressions. Fafel supports integer, floating-point, boolean, and string literals, as well as address literals, which represent Ethereum addresses. Integer literals are written in decimal format, while floating-point literals are written in the form mantissa x 10^exponent, where mantissa and exponent are integers. Boolean literals are either true or false, and string literals are enclosed in double quotes (").
Operators are used to perform operations on values. Fafel supports arithmetic, logical, comparison, and assignment operators, as well as the in operator for testing membership in lists and maps.

#### Grammar

## Semantics

## Type System

## Fafel Compiler

## Example Code

## Comparisons