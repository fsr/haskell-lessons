# Tools

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

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/

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

https://hackage.haskell.org

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

- http://leksah.org
- http://yi-editor.github.io/
- http://haskellformac.com

- https://www.stackage.org/lts-8.9/package/ghcid

# Structure of source files

---

- functions and data types are bundled into modules
- one source file per module

```haskell
module MyModule (function1, function3, DataTypeA(Constructor), constant6) where

-- here follows the code
```

---

- not everything must be exported
- items from other modules and whole other modules can be exported
