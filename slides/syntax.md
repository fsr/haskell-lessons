---
title: Syntax basics
author: Justus Adam
date: 13.04.2017
---

# Comments

---

- `--` line comment
- `{-` block comment `-}`

# Literals

## Types

- Always start with **upper case** letter

- `Int`
- `Integer`
- `Float`
- `Char`
- `String`
- `Bool`

## Signatures

Created with the `::` operator.

`expression :: Type`

In larger context often parenthesised: `big (nested with :: Type) expression`

## Values

Numbers

:   `1`, `3.0` can be overloaded and may have to be annotated `(3 :: Int)`, `(3 :: Integer)` or `(3.5 :: Float)`,
    `(3.5 :: Double)`.

<aside class="notes">
- explain double colon operator
</aside>

---

Characters

:   `'a'`, `'H'`, `'5'`, `'.'`, `'\n'`, `'\\'`

Strings

:   `"Hello World"`, `"Foo\nBar"`, `"with \"quotes\""`

---

Lists

:   `[1,2,3,4]`, `["Hello", "World"]`

Tuples

:   `(5, "A string", 'c')`

--- 

- No special literals for booleans
- Regular data constructors:  `True` and `False`

# Bindings

---

- start with **lower case** letter
- immutable but shadowing allowed

--- 

```haskell
myInt = 5
aBool = False
someString = "Hello World"
```

---

Annotation with type signature

```haskell
myInt :: Int
myInt = 5
```

---

The simplest binding construct is `let ... in`

```haskell
let x = 5 in x + 1
let x = 5
    y :: Int
    y = 6
in x + y
```

---

Or with semicolon and braces instead of newline and indentation

```haskell
let x = 5; y :: Int; y = 6 in x + y
let { x = 5; y :: Int; y = 6 } in x + y
let { x = 5; 
y :: Int
; y = 6 } in x + y
```

# `if` expressions

---

- not a statement but an expression
- has type, returns a value

---

```haskell
aBool = False

anInt = if aBool then 8 else 9
```

# Function application

---

- simple *juxtaposition* or *prefix notation*
- function followed by arguments separated by whitespace

```haskell
succ 5 == 6
takeDirectory "/etc/hosts"
elem (pred 6) [1..10]
not True
```

---

- binary operators
- examples: `(+)`, `(==)`
- bare form for application `+`
- refernce form with parentheses `(+)`

---

```haskell
4 + 5
[1,2,3] ++ [4,5,6]

map (uncurry (+)) [(1,2), (4,5)]
```

--- 

- prefix functions as infox operators: ``4 `elem` [1..10]``.
- infix operators as prefix functions: `(+)`.
- Function application *always binds stronger* than operator application.
- user defined operator prescedence
