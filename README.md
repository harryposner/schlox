# schlox

> "Who gets fish at a chicken place?  That's like getting chicken at a fish
> place!"
> - some guy in front of me in line at Harold's Chicken Shack

Schlox (pronounced like schlock) is an implementation of [the Lox programming
language](https://github.com/munificent/craftinginterpreters/) from Bob
Nystrom's book [Crafting Interpreters](https://craftinginterpreters.com/),
written in [Chicken Scheme](https://call-cc.org/).  It's based on jlox, the
tree-walk interpreter written in Java which he describes in part II of the
book.  There are a few differences in behavior and implementation, though:

- Running the interpreter with the flag `--pretty-print` will pretty-print the
  AST instead of evaluating it.

- Schlox supports C-style block comments, as described in challenge #4 of
  chapter 4 ("Scanning").

- Schlox supports Scheme's full numeric tower.  The syntax for number literals
  is `\d+(\.\d+)?i?`, where the trailing `i` indicates an imaginary number.
  There's no literal syntax for the real part of a complex number or for
  ratios, but if you write them the normal way, the syntax for addition and
  division will evaluate correctly.

- Everywhere that jlox uses exceptions for control flow, schlox uses
  continuations.

    - The jlox parser has a `try`/`catch` block at the top of the grammar
      and throws a `ParseError` when it encounters invalid syntax.  Schlox
      saves the current continuation at the top of the grammar and calls it
      when it encounters invalid syntax.

    - While jlox throws an exception when it reaches a return statement to
      unwind the call stack, schlox saves the continuation of the function when
      it's called and calls that continuation with the return value when it
      evaluates a return statement in that function.

- Instead of using the visitor pattern to mimic  a functional style, schlox
  walks the tree using multimethods.  The book hints at this in an aside in
  chapter 5, section 3.1 ("The Expression Problem").

- Schlox handles `NaN` as specified by IEEE 754, so all comparisons to `NaN`
  are false.  I chose not to follow the semantics from the book because [jlox
  and clox are actually inconsistent with each other
  here](https://github.com/munificent/craftinginterpreters/issues/269).

- Proper tail-call elimination!

## Requirements

- [Chicken Scheme](https://call-cc.org/) version 5 or greater
- The [COOPS egg](http://wiki.call-cc.org/eggref/5/coops).  This is necessary
  for multimethods.  It's also the biggest barrier to making schlox portable,
  since multimethods aren't in the R7RS standard.
- The [SRFI-69 egg](http://wiki.call-cc.org/eggref/5/srfi-69).  This provides
  hash tables, which I use for environments.

## Build instructions

1. [Install Chicken Scheme](http://code.call-cc.org/).  If you're on Ubuntu,
   `apt-get install chicken-bin` will work fine, but it doesn't include
   [Feathers](http://wiki.call-cc.org/man/5/Debugging), the graphical debugger
   for Chicken.

2. Install the COOPS and SRFI-69 eggs with `chicken-install coops srfi-69`.
   You may need to run that as the superuser depending on where you or your
   package manager installed `lib/chicken`.

3. Navigate to the top directory of this repository (i.e. where this README is
   located) and run `make`.  The executable for the interpreter will be
   `build/schlox`.  To make a debug build that you can use with Feathers, run
   `make debug`, and the interpreter will be `debug/schlox`.

## Next steps

Writing schlox was a whole lot of fun, but as far as I can tell, it's done.  It
passes almost all of the jlox test suite---the only exceptions are those that
arise from Schlox having a proper numeric tower instead of using floats for all
its numbers.  The only remaining task I see is getting
[LoxLox](https://benhoyt.com/writings/loxlox/) to run.
