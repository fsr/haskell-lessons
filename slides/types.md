---
title: Types
author: Justus Adam
date: 20.04.2017
---


# Type variables

---

- parameterizes a type
- similar to gernerics
- naming rules the same as bindings
- `TypeName t0 t1 ...`
- `TypeName (AnotherType t0) t1 ...`
- `a -> b -> AType c`

---

```haskell
data Either a b = Left a | Right b

x :: Either String b
x = Left "A String" 
y :: Either a Int
y = Right 1 

x_and_y :: Either String Int
x_and_y = if someBool then x else y 
```

---

```haskell
data Either a b = Left a | Right b

x :: Either String Int
x = Left "A String" 
y :: Either String Int
y = Right 1 

x :: Either String Bool
x = Left "A String" 
y :: Either (Either String String) Int
y = Right 1 

x_and_y :: Either String Int
x_and_y = if someBool then x else y
```

---

```haskell
x :: _
x = Left "A String"

y :: Either a _
y = Right 1
```

# User defined types

## Aliases

- defined using the `type` keyword
- Used to shorten long type names
- Abstract the api from a concrete type (eases refactoring)
- supports partial type parameter application

---

```haskell
type MakerM a = StateT (ALongStateName String Bool (HashMap Text Int)) (LoggingT IO) a

type MyMap key value = HashMap key value
-- or (omitting the `value` variable)
type MyMap key = HashMap key
-- or (omitting both the `value` and `key` variable)
type MyMap = HashMap

type MyMap = Map
```

## ADT's

- standard user defined datatype
- one type - multiple constructors
- defined with the `data` keyword
- followed by any number of `|` separated *constructor definitions*.
- same naming convention as type names

---

```haskell
data Coordinates = LongAndLat Int Int

data Maybe a = Nothing | Just a
```

<aside class="notes">
- explain construtor definition
</aside>    

---

- Constructors are used for
    
    1. *construction* through function application
    2. *deconstruction* through pattern matching
- like a regular function
- supports partial application

--- 

```haskell
LongAndLat :: Int -> Int -> Coordinates

LongAndLat 8 :: Int -> Coordinates

map (LongAndLat 9) [0,9,15] == [LongAndLat 9 0, LongAndLat 9 9, LongAndLat 9 15]
```

<aside class="notes">
- its impotant to know the difference between type and constructor
- type and constructor with same name can be in scope
- place of occurrence allows distinction
</aside>

## Newtypes

- Stricter version of `type`
- Wraps an inner type (hides it)
- only exists at compile time

1. The newtype must have exactly *one* constructor.
2. The constructor must have exactly *one* field.

--- 

- `type` can be used like the source type
- `newtype` cannot

- used to hide implementation or enforce constraints on creation

---

```haskell
newtype Email = Email String

createEmail :: String -> Either String Email
createEmail str =
    if conformsToEmailStandard str
        then Right (Email str)
        else Left "This is not a valid email"

sendEmail :: Email -> String -> IO ()
```

## Using type variables

- declare on the left
- use on the right
- can also be parameters to other types

---

```haskell
data Maybe a = Just a | Nothing

data Either a b = Left a | Right b

newtype SetWrapper a = SetWrapper (Set a)
````


# The `case` construct

---

- case and function application form core of Haskell
- used to deconstruct a type and gain access to the data within

---

```haskell
data MyType = Constr1 Int

aValue = Constr1 5 :: MyType
theIntWithin =
    case aValue of
        Constr1 i -> i

theIntWithin == 5
```

---

- any Haskell expression is allowed in the `case <expr> of` head 
- the body is a number of `matchclause -> expr` pairs
- match clauses are  a combination of constructors and bindings 

```haskell
case expr of
    x -> doSomething x
```

---

Used for default cases

```haskell
data MyType = Constr1 Int | Constr2 String

aValue = Constr1 5 :: MyType
theIntWithin =
    case aValue of
        Constr1 i -> i
        x -> 0
```

---

- match clauses are matched in sequence, from top to bottom
- no fallthrough 
- clause with only binding will always match
- used as default case (often `_` when value is unused)
- `case` is very powerful (can be used to define `if` for instance)

---

```haskell
if cond a b =
    case cond of
        True -> a
        False -> b
```

---

- pattern match allowed on all primitives
- `Char`, `[]`, `String`, `Int`, `Float` and so on

```haskell
case char of
    'c' -> True
    _ -> False

case n of 
    4 -> True
    _ -> False
```

---

We can exchange newlines for `;` and indentation for `{}`.

```hs
case expr of
    Constr1 field1 field2 -> resultExpr 
    Constr2 f -> resultExpr2

case expr of { Constr1 field1 field2 -> resultExpr; Constr2 f -> resultExpr2 }
```

---

We can use multiline result expressions with indentation

```haskell
case expr of
    Constr1 field1 field2 -> 
        resultExpr 
    Constr2 f -> 
        resultExpr2
```

# Special types

---

- the list type `[]` or `[a]`
- tuple type `(a,b)` for a 2-tuple
- `(a,b,c)`, `(a,b,c,d)` etc for n-tuples
- the function type `a -> b`

---

Some examples for concrete instances of special types:

```haskell
myIntBoolTriple :: (Int, Int, Bool)
myIntBoolTriple = (5, 9, False)

aWordList = ["Hello", "Foo", "bar"] :: [String] -- Note: A different way to annotate the type

-- Note: we can also nest these types
listOfTuples :: [(Int, String)]
listOfTuples = 
    [ (1, "Marco")
    , (9, "Janine")
    ]
```

# Record syntax

---

- convenience for defining field accessors
- type is the same as with the other `data` definition

```haskell
data MyType = 
    Constructor { field1 :: Int
                , field2 :: String
                }
```

---

Same pattern match as before.

```haskell
theData = Constructor 9 "hello" :: MyType
theInt = case theData of
            Constructor i _ -> i

theInt == 9
```

---

It also generates some accessor functions.

```haskell
data MyType = Constructor Int String

field1 :: MyType -> Int
field1 (Constructor i _) = i

field2 :: MyType -> String
field2 (Constructor _ s) = s
```

---

And enables record creation and update syntax

```haskell
data TyType = 
    Constructor { field1 :: Int
                , field2 :: String
                }

let v1 = Constructor 9 "Hello" :: MyType

let v2 = Constructor { field2 = "World", field1 = 4 } :: MyType

let v3 = v2 { field1 = 9 }
let v4 = v2 { field1 = 9, field2 "Hello" }

v1 == v4

v2 /= v3 /= v4
```

---

Record pattern match

```haskell
theData = Constructor 9 "hello" :: MyType

theInt = case theData of
            Constructor{ field1 = i } -> i

theInt == 9
```

## Syntactic sugar 


```haskell

data MyType = Constr1 Int | Constr2 String

-- before
getTheInt :: MyType -> Int
getTheInt t = 
    case t of
        Constr1 i -> i
        Consrt2 _ -> 0
```

---

```haskell
-- after
getTheInt2 :: MyType -> Int
getTheInt2 (Constr1 i) = i
getTheInt2 (Constr2 _) = 0

getTheInt2 :: MyType -> Int
getTheInt2 (Constr1 i) = i
getTheInt2 _ = 0
```

---

```haskell
addTheInts :: MyType -> MyType -> Int
addTheInts (Constr1 i1) (Constr1 i2) = i1 + i2
addTheInts (Constr i)   _            = i 
addTheInts _            (Constr i)   = i 
addTheInts _            _            = 0
```

# Misc

## Error

You can use the `error` function to throw an exception on Haskell.

Or use the `undefined` value (which itself is an `error`).

If these are ever evaluated an exception is raised.

```haskell
error "Something went wrong"
```

## Importing

You can import other module using the `import` keyword.

```haskell
import Data.List
import Data.Set (member, Set, singleton)
import Prelude as P
import qualified Data.HashMap as H
```

<aside class="notes">
Also mention how to load source files into ghci
</aside>
