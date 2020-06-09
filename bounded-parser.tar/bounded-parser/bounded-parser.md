# Parsing with Bounded Lookahead

In HW3, you built a basic parser combinator library in Haskell and used it to
parse the Ruse language. While that parser is quite powerful, it has a
drawback---it has *unbounded lookahead*: the parser may need to look at an
unbounded number of characters before it backtracks and tries a different
alternative. For instance, if the grammar is something like:
```
  (1000 a's) 01
| (1000 a's) 02
| ...
| (1000 a's) 99
```
and the input is 1000 `a`s followed by `00`, the parser will try parsing 1000
`a`s a total of 99 times before it fails and returns parse error.

In this assignment, you will build a more refined parser that has *bounded,
one-character lookahead*: if it ever consumes at least one character
succesfully, then the parser will not backtrack to consider different
alternatives even if it fails. While this parser will not be as general as the
parser from HW3, it will be much more efficient. For the above grammar on input
1000 `a`s followed by `00`, the parser will only try the first alternative (1000
`a`s followed by `01`) before giving up since it parses (at least one) `a`.

All of your work will be in the file `src/Calc.hs`. We've included some tests in
`src/Tests.hs` and the main function in `app/Main.hs`; you should not need to
modify these files.

# Part 0: Target Language

As always, you'll be parsing a calculator language. There are four operators:
`+`, `-`, `*`, and `%`, all written in usual, infix notation. Numerical
constants are lists of 1-or-more digits; there are no negative numbers. You will
use the following grammar:
```
Expr  = Fact ExprR

ExprR = "+" Fact ExprR
      | "-" Fact ExprR
      | END 

Fact  = Term FactR

FactR = "*" Term FactR
      | "%" Term FactR
      | END

Term  = Num
      | "(" Expr ")"

Num   = Digit { Digit }

Digit = "0" | "1" | ... | "9"

END   =
```
This grammar seems complicated, but it is designed to handle order of operations
correctly. For instance, `1+2*3` is parsed as `1+(2*3)`, and `2*3+1` is parsed
as `(2*3)+1`. `Digit` is 1-or-more digit characters, and `END` is no characters
(empty string). You should assume that the program has already been lexed, so
there are no spaces. We've given you types for the AST at the top of `Calc.hs`.

# Part 1: Evaluator

Your first task is to write an evaluator, completing the functions `eval*`. Note
a few things:
- These expressions for `CExprR` and `CFactR` represent the "right-hand" bit of
  an expression, for instance the `+2*3` part of `1+2*3`, or the `*3` part of
  `2*3`. The additional `Int` argument to the evaluator for these cases holds
  the "left-hand" bit of an expression, for instance the `1` part of `1+2*3`, or
  the `2` part of `2*3`.
- Your evaluator should use the usual arithmetic operations for `+`, `-`, and
  `*`. For `%`, use Haskell's modulo operation (written `mod`). For instance,
  `7%4` should evaluate to `3`.

# Part 2: Bounded Parser

Next, the bounded parser. We've given you the types: `BParser`, `PResult`, and
`BPResult`. The main new bit is `BPResult`, which has two constructors.
`Consumed pr` signals that the parser has already consumed at least one
character; note that `pr` might be `ParseOk` or `ParseErr`. `NotConusumed pr`
signals that the parser has not consumed any characters successfully; again,
`pr` might be `ParseOk` or `ParseErr`.

You will need to write `pBind` and `pPlus` for sequencing and choice.

* In `pBind`, there are a few cases.
  + If the first parser returns `Consumed ParseOk`, the whole parser should
    return `Consumed` of the result of the second parser.
  + If the first parser returns `NotConsumed ParseOk`, the whole parser should
    return the result of the second parser (unchanged).
  + If the first parser returns `Consumed ParseErr`, the whole pasrer should
    return `Consumed ParseErr`.
  + If the first parser returns `NotConsumed ParseErr`, the whole pasrer should
    return `NotConsumed ParseErr`.
* In `pPlus`, the second parser should only be tried if the first parser returns
  `NotConsumed ParseErr`. If the first parser returns anything else, the result
  of the first parser should be used (and the second parser should not be run).

Then, write `satisfy`.

* The parser `satisfy pred` should parse any character `c` where `pred c` is
  true, consuming one character. If the first character doesn't match or the
  string is empty, then the parser should return `NotConsumed ParseErr`.

# Part 3: Parsing a Calculator

Finally, build a parser for the calculator language by completing the functions
`parse*`. You may write auxiliary functions/parsers if you want, though you
should not need to.

# Testing

We've included some tests in `src/Tests.hs`. To load these tests, use `cabal
v2-repl` and load the tests module: `:l Tests`. Then:

* To test the evaluator, run `testEval`.
* To test the parser, run `testParse`.

We've also included a Quickcheck geneator for testing parsing; to run this test,
do `quickCheck printParse`. 

Note that the generator **does not shrink testcases**---if it reports a failing
input, you can run it a few more times to try to get a smaller input, or you can
try to simplify the test case before trying to debug.
