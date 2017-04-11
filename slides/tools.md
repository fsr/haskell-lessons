---
title: Tools
author: Justus Adam
date: 13.04.2017
---

# GHC

---

> - (Glorious) Glasgow Haskell Compiler is de-facto standard
> - generates native machine code
> - statically linked libraries
> - binaries include a small runtime (3 MB)

## Targets

> - Native
> - The browser (GHCJS)
> - The JVM ("eta" and GHCVM) (new)

## Compiler Reference

[downloads.haskell.org/~ghc/latest /docs/html/users_guide/](https://downloads.haskell.org/~ghc/latest /docs/html/users_guide/)

## GHCi

- allows interactive execution and inspection of code
- useful for debugging
- code is (as always) typechecked

---

`:?`

:   Probably the most important command. Displays the help menu. The
    help menu lists available commands (a selection of which will
    follow here) and what they do.

`<expr>`

:   Simply submitting a Haskell expression evaluates the code and tries
    to print the result value.

---

`let <name> = <expr>`

:   Binding the value of an expression to a name.

`:browse <module>`

:   Displays the contents of the module with the entered name.

---

`:type <expr>`

:   Prints the type of the expression `expr`.

`:info <name>`

:   Displays information about the name, such as the source module, or
    the type if it is a function.


# Documentation resources


## Hackage

[hackage.haskell.org](https://hackage.haskell.org)

--- 

- browsing of documentation
- listing dependencies
- searching for packages by names, keywords and tags
- anyone can create an account and upload packages

## Hoogle

- search multiple packages simultaneously
- search for names, types, modules and type signatures

## Stackage

- A hackage + hoogle for fixed package set
- I recommend using this rather than hackage (when possible)

# Editors

---

- [leksah.org](http://leksah.org)
- [yi-editor.github.io](http://yi-editor.github.io/)
- [haskellformac.com](http://haskellformac.com)

---

- [www.stackage.org/lts-8.9/package/ghcid](https://www.stackage.org/lts-8.9/package/ghcid)
- [github.com/tonsky/FiraCode](https://github.com/tonsky/FiraCode)
