---
title: Functions
author: Justus Adam
date: 20.04.2017
---

# Literals

---

- Called *lambdas* in other languages
- `\arguments -> body`
- no `return` keyword required

<aside class="notes">
- Function bodies are always expressions (becuase (almost) everything in haskell is an expression)
- Imagine an implicit `return` right after the arrow if that helps.
</aside>

---

```haskell
id = \x -> x
plusThree = \x -> x + 3
not = \b -> if b then False else True
```

## Function type


```haskell
plusThree :: Int -> Int
plusThree = \x -> x + 3
id :: a -> a
id = \x -> x
```

- `->` is the function type

---

- *left* yields *right*
- `Int -> Bool` to `Int` is `Bool`
- `a -> a` to `Int` is `Int`
- `a -> a` to `Bool` is `Bool`

## Application

```haskell
id :: a -> a
id = \x -> x

myBool = id True
myBool2 = (\x -> x) True
myInt = id 5

myBool == myBool2 == True && myInt == 5
```

## Multiple Parameters

```haskell
const :: a -> b -> a
const = \x _ -> x
```

---

- `_` is used to indicate an unused parameter
- `a -> b -> a` is a type for a two parameter function
- can be rewritten `a -> (b -> a)`

---

```haskell
const :: a -> (b -> a)
const = \x -> \_ -> x
```

```haskell
\x _ -> x
≈
\x -> \_ -> x
```

## Partial application

```haskell
const :: a -> b -> a
const = \x _ -> x

alwaysFive = const 5

alwaysFive "a string" == alwaysFive 6 == alwaysFive () == 5
```

---

```haskell
plusThree = (+ 3)

plusThree 5 == 8
plusThree 10 == 13
```

---

```haskell
map (+ 1) [4,5,8] == [5,6,9]
find (== 6) [3,6,8] == Just 2
```


# Syntactic sugar

---

```haskell
myFunction = \a b -> doSomthing
anotherFunction = \x -> expr

myFunction a b = doSomthing
anotherFunction x = expr
```
