# LispCompiler
ITMO CTD Assembler coursework.

This compiler supports custom subset of lisp (see tests/examples). Features:
  * List processing (see `test/lisp/test15.lisp`)
  * Recursive lambdas/functions
  * Basic IO (reading ints/lists/strings)
  * Closures and passing functions into functions
  * Tailcall optimization
  * Standart library, asm parsing and static linking
  * Simple macro support
  * Global defines (functions that don't use custom memory management can be safely called from outside)

#### Howto:
```bash
$ cabal build compiler
$ ./compiler --help
# running tests
$ ./tester.sh
# check consistency of stdlib
$ ./libtester.sh
# example (factorial)
$ ./compiler -o a.out ./test/lisp/test19.lisp && ./a.out <<< 6
```
```
Usage: ./compiler [Flag]* InputFile 
Simple one-char flags can be combined like -abcde.
    --help -h  show this help and exit
    -M         disable script mode (compile without main)
    -p         enable function labels prefix (for Darwin and Windows)
    -S         do not compile, link with libraries only, product .yasm
    -o output  specify output file
    -L path    load library specified by path
```
