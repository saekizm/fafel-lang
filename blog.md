#### 27th of October

We have just pushed our proposal for our project, Fafel. 
We are building a functional smart contract language for evm compatible blockchains.
Fafel is short for "Finally a Functional Ethereum Language".

#### 3rd of December

It has been awhile since our last blog post, we have mainly been researching
the functional paradigm, compiler design and language design. This is quite a challenging
project, as we also need to learn about Yul, the IR language for the evm. A lot more research
is needed.

We have just pushed our functional specification, although we imagine, this requirements and what ends up working shall vary over the course of the development of Fafel.

#### 24th of January

We have begun working on the implementation of our Parser. Parsec is an invaluable tool for this job.
We basically build parsers for each element of our code, and then build parsers on top of parsers, until we are left with a contractParser, which outputs our AST (Abstract Syntax Tree).


#### 3rd of February

We have finished the implementation of our parser, and now we are working through our codegen file.
This is where a lot of thinking had to be done about how we are implementing this language. 

We consider our construct of state to be a direct mapping to storage on the evm. Therefore, with each
state variable declaration, we should map this to `sstore` calls in Yul. We have compound data types, such as `list` and `mapping` types, but we only imagine these as being used for storage data structures. 

Working with memory and storage in on the evm or even with Yul has been a challenge in its own, but necessary. We use clever tricks to be able to store lists and mappings and avoid collisions. Generally, we are taking a hash of some data to get a position.

Yul has no understanding of other types besides u256, so while we are converting our AST nodes into Yul strings, there is no typechecking done, and certain incorrect code would be possible to compile.

#### 10th of February

We have finally managed to get our code generator working, and have compiled a fafel contract to a Yul contract! We haven't gotten all of our node functions finished yet, but the core functions we do have, are working.

Our codegen file implements a symbol table to store slot positions for variables and the like, and replaces them with the correct slot number where appropriate in the yul code. The rest of the functions are used for translating a node, into a yul string.

From here, we have our basic implementation. Most of what we will be doing is refactoring and trying to improve where we can.

#### 22nd of February

We are almost at the finishing line. We have done a lot of refactoring since our last update. We have now split some files and added some new ones. We have an AST file, a Lexer file, a Parser file, a Typechecker file and a Codegen file.

We refactored our parser and created a lexer and AST file to abstract away some of the code using the relevant parsec libraries.

We managed to get our basic typechecker working which is great, now we can ensure that everything is typed correctly. Our typechecker uses a symbol table which is a mapping of a name to a `Either DataType Map String Int` type. We store variables as a DataType and we store all the function details we need in a mapping with the name.

We have added scripts for sending transactions to the contract and deploying the contract when using the compiler.