# schlox

> "Who gets fish at a chicken place?  That's like getting chicken at a fish
> place!"
> - some guy in front of me in line at Harold's Chicken Shack

Schlox (pronounced like schlock) is a work-in-progress implementation of [the
Lox programming language](https://github.com/munificent/craftinginterpreters/)
from Bob Nystrom's book [Crafting
Interpreters](https://craftinginterpreters.com/), written in [Chicken
Scheme](https://call-cc.org/) (I'm currently working on Chapter 10).  It's
based on jlox, the tree-walk interpreter written in Java which he describes in
part II of the book.  There are a few differences, though:

- Running the interpreter with the flag `--pretty-print` will pretty-print the
  AST instead of evaluating it.

- Schlox supports C-style block comments, as described in challenge #4 of
  chapter 4 ("Scanning").

- Schlox supports Scheme's full numeric tower.  The syntax for number literals
  is `\d+(\.\d+)?i?`, where the trailing `i` indicates an imaginary number.
  There's no literal syntax for the real part of a complex number or for
  ratios, but if you write them the normal way, the syntax for addition and
  division will evaluate correctly.

- The parser uses continuations instead of exceptions to recover from errors.
  Where jlox has a `try`/`catch` block at the top of the grammar and throws a
  `ParseError` when it encounters invalid syntax, schlox saves the current
  continuation at the top of the grammar and calls it when it encounters
  invalid syntax.

- Instead of using the vistor pattern to mimic  a functional style, schlox
  walks the tree using multimethods.  The book hints at this in an aside in
  chapter 5, section 3.1 ("The Expression Problem").

- Schlox handles `NaN` as specified by IEEE 754, so all comparisons to `NaN`
  are false.  I chose not to follow the semantics from the book because jlox
  and clox are actually inconsistent with each other here.

# Requirements

- [Chicken Scheme](https://call-cc.org/) version 5 or greater
- The [COOPS egg](http://wiki.call-cc.org/eggref/5/coops).  This is necessary
  for multimethods.  It's also the biggest barrier to making schlox portable,
  since multimethods aren't in the R7RS standard.
- The [SRFI-69 egg](http://wiki.call-cc.org/eggref/5/srfi-69).  This provides
  hash tables, which I use for environments.
