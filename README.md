# Rattlesnake 🐍

Statically typed imperative toy programming language, compiled to the JVM

## Example program
```
fn fib(n: Int) -> Int require n >= 0 {
    if n <= 1 {
        return n;
    };
    var prev = 0;
    var curr = 1;
    for var i = 2; i <= n; i += 1 invar curr > 0 invar prev >= 0 {
        val next = prev + curr;
        prev = curr;
        curr = next;
    };
    return curr;
} ensure result >= 0

fn main(args: arr String){
    print(intToString(fib(10)))
}

```

[Instructions for running the verifier](#notes-on-using-the-verifier)

[Report](https://github.com/FV-Rattlesnake-team/Rattlesnake/blob/main/Report.pdf)

[Slides](https://github.com/FV-Rattlesnake-team/Rattlesnake/blob/main/Slides.pdf)

## Command-line program

Run `help` to see the available commands and options

## Language description

The language does not support modules. All functions and data structures are top-level and identified by their textual name across the whole program. Functions and types can be referred to from inside the file that defines them or from another file in the exact same way.

### Code blocks

Statements are separated with `;`. `;` may be omitted after the last statement.

### Types and data structures

- Primitive types:
    - `Int`, `Double`, `Bool`, `Char`
    - `Void`: return type of a function that does not return any value
    - `Nothing`: return type of a function that terminates the program and thus never returns

    Subtyping: there is no subtyping relation, except that `Nothing` is a subtype of all other types

- `String`

- Arrays: `arr <element type>`, e.g. `arr Int`

    Creation: <p>
    - `arr <type>[<size>]`, e.g. `arr Int[0]` <p>
    - `[<elems>*], e.g. [-7, 31, 14, 11]`
    
    Access an element: `<array>[<index>]`, e.g. `xs[7]`
    
- Structures, e.g. `struct Foo { bar: Int }`

    Creation: `new Foo { 0 }`
    
    Access a field: `foo.bar`
    
### Functions

```
fn <function name>(<args>*) -> <return type> {
   ...
}
```
Return type can be omitted if it is `Void`
E.g.:
```
fn bar(i: Int, b: Bool) -> String {
   ...
   return "Hello"
}
```
A non-void function must contain a `return <value>` for each control-flow path in the function. If the function has return type `Void`, then `return` is not required but may be used (without a value) to exit the function early.

### Locals

```
val <name>: <type> = ...
```
Type may be omitted. `var`s are defined similarly.
E.g.:
```
val x: Int = 0;
val str = "Rattlesnake";
```
`var`s (but not `val`s) can be reassigned: `x = <new value>`

### Control structures

#### If-else
```
if <cond> {
   ...
}
if <cond> {
   ...
} else if <cond> {
   ...
} else {
   ...
}
```

#### While loop
```
while <cond> {
   ...
}
```

#### For loop
```
for <stat>;<cond>;<stat> {
   ...
}
```
E.g.:
```
for var i = 0; i < #array; i += 1 {
   ...
}
```

### Operators

#### Unary operators
- `-`: opposite of an `Int` or a `Double`
- `!`: logical negation of a `Bool`

#### Binary operators
- Mathematical: `+`, `-`, `*`, `/`, `%` (can be combined with `=`: `+=`, `/=`, etc.)
- Comparisons (between `Int`s or `Double`s): `<`, `>`, `>=`, `<=`
- Equality: `==`, `!=` (strings are compared by structure, `struct`s are compared by reference)
- Logical: `&&`, `||` (implement lazy evaluation of second operand)
- String concatenation: `+`

#### Ternary operator
```
when <cond> then <expr1> else <expr2>
```

### Cast/type conversion
The following conversions can be performed:
- `Int` <-> `Char`
- `Int` <-> `Double`

Syntax: `<expr> as <type>`, e.g. `10 as Double`

### Panic
Terminates the program with an exception
`panic <message>`, e.g. `panic "forbidden argument " + arg`

### Verification

#### Assertions

```
assert <predicate>
```

#### Preconditions and postconditions

`require` and `ensure`:
```
fn foo(x: Int, y: Int) -> Int
require x > 0
require y >= 0
{
    return 2*x + y
}
ensure result > y
```

#### Loop invariant

Checked at the beginning of each iteration as well as when the loop is exited

For loop:
```
var sum = 0;
for var i = 0; i < #xs; i += 1 invar sum >= 0 {
    sum += xs[i]
}
```

While loop:
```
while x < 20 && y < 100 invar x < y {
    ...
}
```

#### Ignore a function
A function will be ignored during verification if it is annotated with the `unchecked` keyword, e.g.:
```
fn main(args: arr String) unchecked -> Void
```

#### Notes on using the verifier

- Before verifying programs, make sure that z3 is installed and callable from the compiler. We tested it using z3 4.11.2, other versions might work as well but produce errors like "unable to parse z3 response" when an assertion does not hold (uncertain);

- Command for verifying a file: `verify -out-dir=<directory for smt files> <path to source file>` (this is the command for the Rattlesnake compiler, if the program is run from sbt this command must be given as a program argument).

    E.g. `verify -out-dir=testout src/test/res/verif/fibonacci1.rsn`;

- The verifier does not check termination. Precisely, it checks partial correctness: if the function terminates, then it must satisfy its specification. In some cases, obviously wrong functions might therefore be successfully verified because they don't terminate, e.g.:

    ```
    fn min(a: Int, b: Int) -> Int {
        return min(a, b)
    }
    ensure (result == a || result == b)
    ensure result <= a
    ensure result <= b
    ```

- In some cases, the verification of formulas relies on the correctness of other formulas that occur before them in the program. As a result, it can happen that an invalid formula is successfully verified if a formula asserted before it is invalid;

- When a formula is found to be invalid, the verifier tries to output an assignment of variables that makes the formula false. This messages are meant to help the user figure out why verification fails, but they can be difficult to read, especially when (mutable) variables are involved. The best way to use them is to look for the values that are impossible. E.g. if the variables assignment contains `x == -1` but it is obvious from the program that x is never negative, this probably means that `x >= 0` should be added as an invariant or precondition or postcondition;

- Some example programs can be found in the [examples](https://github.com/FV-Rattlesnake-team/Rattlesnake/tree/main/examples) folder. More examples (including some where verification finds bugs) can be found in the [test/res/verif](https://github.com/FV-Rattlesnake-team/Rattlesnake/tree/main/src/test/res/verif) folder (the buggy/failing examples are the one whose name contains "fail").

## Built-in functions

The compiler replaces calls to these functions with special instructions.

- `print(s: String)`: display `s` on the console
- `?ToString(...)` with `?` one of `int`, `double`, `char`, `bool`, and the corresponding parameter type: conversion to a string
- `toCharArray(s: String)`: converts a string into an array of all its characters

## References

Lexer and parser are inspired from https://github.com/epfl-lara/silex and https://github.com/epfl-lara/scallion, respectively.

Backend uses the ASM bytecode manipulation library (https://asm.ow2.io/).

Verifier uses Z3 (https://github.com/Z3Prover/z3) and Scala SMT-lib (https://github.com/epfl-lara/scala-smtlib).

