# Why you should learn Haskell

---

First off, well done for choosing this course! 😁

# Safety

---

A strong and versatile typesystem enables very expressive code:

```haskell
(+) :: Int -> Int -> Int
readFile :: FilePath -> IO String
getUser :: UserId -> PostgresM User
clear :: MutableVector a -> ST s ()
generateBinding :: State Generators Binding
```

# Expressive code

---

```haskell
filter (elem 'a') ["Marc", "Frank", "Tobias"]
sum [0..10]
```

## Higher order functions

```haskell
filter (elem 'a') ["Marc", "Frank", "Tobias"]
map (+1) [4,5,8]
zipWith (-) [0,8,6] [4,4,7]
```

## Composition

```haskell
not . (== 7)
filter (takeExtension >>> (== "exe")) >>> map dropExtension
```

# Imutability

---

```haskell
let l1 = [4]
let l2 = 5:l1
l1 == l2 -- is this true?
```


- Makes it very easy to write parallel applications
- Functions cannot implicitly manipulate your datastructures

# Concurrency

---

```haskell
-- sequential
do
    content <- readFile f
    writeFile g (modify content)

-- make it asynchronus
a <- async $ do
    content <- readFile f
    writeFile g (modify content)
wait a
```
