# Higher order functions

- understand higher-order functions
- map filter and foldl to avoid writing recursive functions
- implement many higher-order functions

## map

```
Prelude> map reverse ["dog", "cat", "moose"]
["god","tac","esoom"]
Prelude> map head ["dog", "cat", "moose"]
"dcm"
Prelude> map (take 2) ["dog", "cat", "moose"]
["do","ca","mo"]
```

## filter

```
Prelude> filter even [1,2,3,4]
[2,4]

Prelude> filter (\(x:xs) -> x == 'a') ["apple", "banana", "avocado"]
["apple","avocado"]
```

## foldl

```
Prelude> foldl (+) 0 [1,2,3,4]
10
```

## elem

```
Prelude Data.List> elem 1 [1..10]
True

Prelude Data.List> elem 'c' ['d'..'e']
False
```

## Exercises

```
myElem :: Eq a => a -> [a] -> Bool
myElem value list =
  any ((==) value) list
```

```
isPal :: String -> Bool
isPal sentence =
  reverse normalized == normalized
  where normalized = map (Data.Char.toUpper) $ filter ((/=) ' ') sentence
```

```
harmonic :: Int -> Float
harmonic n =
  foldr (+) 0 $ take n sequence
  where sequence = map (1 /) [1..]
```
