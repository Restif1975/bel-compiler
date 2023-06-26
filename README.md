Bel is a specification for a new dialect of Lisp written by Paul Graham.

Several implementations of Bel already exist where Bel code is interpreted i.e. analysed and directly executed by another high-level language.

This is a Bel compiler where Bel code is analysed and translated to a lower level language.

What is this lower level language?  
It is not machine language, it is a step above called intermediate code.

This intermediate code is the language of a virtual machine.   
Many virtual machines exist, like the Java Virtual Machine (JVM).  

The intermediate code used here is the language of the Emacs Lisp Virtual Machine.

For example, the Bel program to print "Hello, World!" is:

(print "Hello, World!")

The compilation of this program gives:

(byte-constant prin1 . 0)  
(byte-constant "Hello, World!" . 1)  
(byte-call . 1)  
(byte-return . 0)  

or in a more compact form:

[192 193 33 135]  
[prin1 "Hello, World!"]  

This compact form is called bytecode, as each instruction is represented by a byte (octet), an integer between 0 and 255.

This intermediate code can then be executed in Emacs by a bytecode interpreter (written in C).  
This is the runtime environment.

## Requirements

- GNU Guile (version >= 3.0.9)
- GNU Emacs (version >= 28.2)

## Getting started

```sh
git clone https://github.com/Restif1975/bel-compiler
```

Assuming a Bel file test.bel, typing:

```
guile -e main -s main.scm test
```

will produce two files:

- test.ebel, with Bel macros expanded
- test.elc, the corresponding Emacs Lisp bytecode.

To run the compiled file, type in Emacs:

```
M-x ielm
(load "start.el")
(load "test.elc")
```

Please note:

- Bel readtime abbreviations are currently not available
- Some Bel features are still missing e.g. streams, templates
- The Bel syntax for dyn was changed to dynamic-let and dynamic (see test.bel)

## Files description
<pre>
expand.scm     --- macroexpansion  
mac.scm        --- predefined macros  
object.scm     --- Sexp->object  
comp.scm       --- compilation  
comp-init.scm  --- opcodes  
lib.bel        --- library functions source code  
prim.el        --- primitives  
lib.el         --- library functions wrappers  
test.bel       --- examples  
</pre>
## Sources

- [Bel specification](http://www.paulgraham.com/bel.html)
- [Emacs Lisp Bytecode Reference Manual](https://rocky.github.io/elisp-bytecode.pdf)
- Dybvig, Friedman, Haynes, Expansion-Passing Style: Beyond Conventional Macros, 1986
- Christian Queinnec, Lisp in Small Pieces, Cambridge University Press, 1996
- Emacs source in Lisp: https://github.com/emacs-mirror/emacs/tree/master/lisp/emacs-lisp

