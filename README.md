# gcc

Compiler for a small LISP down to GCC

## Usage

Interactively:

    $ lein repl
    > (use 'gcc.core)
    > (gcc '((defun x () 23)))

Command line:

    $ lein uberjar
    $ java -jar $PWD/target/gcc-0.1.0-SNAPSHOT-standalone.jar $INPUT1 $INPUT2