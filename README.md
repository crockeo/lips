#**lips**

## Description

`lips` is an excercise in writing real-world projects in Haskell. `lips` itself
is a Scheme based on the eBook *Write Yourself a Scheme in 48 Hours*. It is
only a subset of scheme (due to the fact that this is a learning experience as
opposed to actually trying to create a workable language), but it is a very
functional programming language. Both in the sense that it is a Scheme (and
therefore functional), but also in usability.

## Installation

```bash
>$ git clone http://github.com/Crockeo/lips.git
>$ cd lips

# To install into the local folder
>$ cabal sandbox init
>$ cabal install --only-dependencies
>$ cabal run [sourcefile path]

# Alternatively, to install globally
>$ cabal install --global
>$ lips [sourcefile path]
```

## Notes

* Running `lips` without any arguments will bring you into a REPL where you
can interact with the language interactively.
* I personally suggest that this project not be used as a learning reference
for any of the material inside. It does what intended, but I cannot guarantee
that it is the best way to do it.
* Upon full implementation, documentation will be able to be found in a folder
named `doc` in the root project directory.
