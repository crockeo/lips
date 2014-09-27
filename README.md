#**lips**

## Description

*lips* is an exercise in creating a programming language interpreter through
Haskell. It's based on (but not completely the same as) the work specified in
*Write Yourself a Scheme in 48 Hours*.

## Installation

```bash
>$ git clone http://github.com/Crockeo/lips.git
>$ cd lips

# To install into the local folder
>$ cabal sandbox init
>$ cabal install --only-dependencies
>$ cabal run [sourcefile path]

# Alternatively, to install
>$ cabal install
>$ lips [sourcefile path]

# Alternatively, to install globally
>$ cabal install --global
>$ lips [sourcefile path]
```

Both the local and global installations require you to put their respective
paths into your PATH variable.

## Notes

* Running `lips` without any arguments will bring you into a REPL where you
can interact with the language interactively.
* I personally suggest that this project not be used as a learning reference
for any of the material inside. It does what intended, but I cannot guarantee
that it is the best way to do it.
