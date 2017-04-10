# Syntax basics

# Comments

---

- `--` line comment
- `{-` block comment `-}`

# Literals

## Types

- Always start with **upper case** letter

- `Int` A fixed size integer
- `Integer` An unbounded integer.
- `Float` A floating point number.
- `Char` A character.
- `String` A sequence of characters.
- `Bool` A boolean.

## Values

Numbers

:   `1`, `3.0` can be overloaded and may have to be annotated `(3 :: Int)`, `(3 :: Integer)` or `(3.5 :: Float)`,
    `(3.5 :: Double)`.

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
let myInt = 5
let aBool = False
let someString = "Hello World"
```

---

``haskell
let myInt :: Int
    myInt = 5
```

# `if` expressions

---

- not a statement but an expression
- has type, returns a value

---

```haskell
let aBool = False

let anInt = if aBool then 8 else 9
```