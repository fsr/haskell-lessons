---
title: I/O and `do` notation
author: Justus Adam
date: 04.05.2017
---

# Tagged I/O

---

- Purity prevents arbitrary I/O
- Special type `IO` marks I/O computations
- `IO ()` is equivalent to `void`

```hs
putStrLn :: String -> IO ()
getLine :: IO String
```

## `do` ing IO

- `do` chains `IO` actions
- executed in sequence

```hs
main = do
    putStrLn "Starting work"
    writeFile "Output" "work work work"
    putStrLn "Finished work"
```

---

Extraction of values inside `IO`

```hs
main = do
    l <- getLine
    putStrLn ("You entered the line: " ++ l)
```

---

- `do` creates a new `IO`

```hs
action :: IO ()
actions = do
    l <- getLine
    putStrLn ("You entered the line: " ++ l)    
```

---

- `let` statement to handle pure values
- `return` to return pure values

```hs
action :: IO String
actions = do
    l <- getLine
    let computed = computeSomething l
    return computed
```

## Running `IO`

- type into GHCi
- the `main` function

# Overloaded `do`

---

- more container/tag types
    + `[a]`
    + `State state a`
    + `Reader env a`
- generalized via `Monad` typeclass

---

Any `Monad` can use the `do` notation.

```hs
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

## Libaries

- [monad-loops](https://www.stackage.org/haddock/lts-8.13/monad-loops-0.4.3/Control-Monad-Loops.html)
- [Control.Monad](https://www.stackage.org/haddock/lts-8.13/base-4.9.1.0/Control-Monad.html)
- Any `:: Monad m => ...`  function can be used with `IO`
