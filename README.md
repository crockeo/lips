# lips

## Description

`lips` is an excercise in writing real-world projects in Haskell. `lips` itself
is a Scheme based on the eBook *Write Yourself a Scheme in 48 Hours*. It is
only a subset of scheme (due to the fact that this is a learning experience) as
opposed to actually trying to create a workable language.

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
