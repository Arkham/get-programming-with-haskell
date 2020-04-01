# The List Monad and list comprehensions

In this chapter, we'll learn to:

- Use do-notation to generate lists
- Filter results in do-notation by using _guard_
- Further simplify do-notation with list comprehensions

In the previous lesson we saw an example of using `List` as a `Monad`

```haskell
assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement
```

What is so interesting about this snippet is that when you use `<-` to
assign your list to a variable, you get to treat it as though it were a
single value. It returns back to being a list only after the `return`.

When we looked at `List` as an `Applicative`, we talked about it being an
example of nondeterministic computation (aka multiple computations being
executed at the same time).

```
Prelude> [(+), (*)] <*> [100, 200, 300] <*> [50, 2000]
[150,2100,250,2200,350,2300,5000,200000,10000,400000,15000,600000]
```

Instead using `List` as a `Monad` is way more familiar. It allows you to
build complicated lists in an easy way, like you would do in Python.

## Building lists with the List Monad

Let's generate a list of powers of 2 using the List Monad

```haskell
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
  value <- [1 .. n]
  return (2^value)
```

Taking it for a spin:

```
Prelude> powersOfTwo 0
[]
Prelude> powersOfTwo 1
[2]
Prelude> powersOfTwo 2
[2,4]
Prelude> powersOfTwo 3
[2,4,8]
Prelude> powersOfTwo 4
[2,4,8,16]
```

Note that we could have achieved the same result using `map`, but in that
case we would be treating `List` as a data structure, rather than as a
context. We can see that it's easy to generate more complicated structures

```haskell
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
  value <- [1 .. n]
  let powerOfTwo = 2^value
  let powerOfThree = 3^value
  return (powerOfTwo, powerOfThree)

Prelude> powersOfTwoAndThree 5
[(2,3),(4,9),(8,27),(16,81),(32,243)]
```

Or here's a function that generates all possible combination of odd and
even numbers up to `n`:

```haskell
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
  evenValue <- [2, 4 .. n]
  oddValue <- [1, 3 .. n]
  return (evenValue, oddValue)

Prelude> allEvenOdds 6
[(2,1),(2,3),(2,5),(4,1),(4,3),(4,5),(6,1),(6,3),(6,5)]
```

(Note that in the `[2, 4 .. n]` Haskell will look at the first two elements
and decide the step to apply for the rest of the range, pretty nifty!)

(Also note that you don't get `n` tuples back, but all combinations where
the values are less or equal than `n`)

#### Quick Check!

Use do-notation to generate pairs of numbers up to 10 and their squares!

```haskell
pairWithSquare :: Int -> [(Int, Int)]
pairWithSquare n = do
  x <- [1 .. n]
  return (x, x^2)
```

### The guard function

Another neat trick is to filter lists not by using `filter`, but instead by
using the `guard` function included in `Control.Monad`:

```
evensGuard :: Int -> [Int]
evensGuard n = do
  value <- [1 .. n]
  guard(even value)
  return value

Prelude> evensGuard 10
[2,4,6,8,10]
```

If we look at the type signature of the `guard` function we will see this:

```
Prelude> :t guard
guard :: GHC.Base.Alternative f => Bool -> f ()
```

The `Alternative` type class is a subclass of `Applicative`. Let's look at
it:

```
Prelude> :info GHC.Base.Alternative
class Applicative f => GHC.Base.Alternative (f :: * -> *) where
  GHC.Base.empty :: f a
  (GHC.Base.<|>) :: f a -> f a -> f a
  GHC.Base.some :: f a -> f [a]
  GHC.Base.many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
  -- Defined in ‘GHC.Base’
```

Uh, the minimal functions are `empty` and `(<|>)`.

`empty` seems straightforward:

```
Prelude> empty :: [Int]
[]

Prelude> empty :: Maybe Int
Nothing
```

What about `(<|>)`? For lists it's pretty easy too:

```
Prelude> [10] <|> [5]
[10,5]
```

But what about Maybes?

```
Prelude> Just 3 <|> Nothing
Just 3
Prelude> Just 3 <|> Just 5
Just 3
Prelude> Nothing <|> Just 5
Just 5
Prelude> Nothing <|> Nothing
Nothing
```

Oh cool, it's literally an OR for maybes :)

#### Quick Check!

Write `filter` using `guard` and do-notation.

```haskell
guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter fun elems = do
  value <- elems
  guard . fun $ value
  return value

Prelude> guardFilter even [1..10]
[2,4,6,8,10]
```

## List comprehensions

In Python you can use a special syntax for generating lists called list
comprehension. It looks like this:

```python
>>> [n**2 for n in range(10)]
[0, 1, 4, 9, 16, 25, 36, 49, 64, 81]
```

If you wanted to write the same in Haskell using do-notation it would look
like this:

```haskell
evenSquares :: [Int]
evenSquares = do
  n <- [0 .. 9]
  let nSquared = n^2
  guard . even $ nSquared
  return nSquared
```

But of course Haskell has a terser syntax for that!

```haskell
evenSquares :: [Int]
evenSquares = [n^2 | n <- [0 .. 9], even n]
```

List comprehensions are a nice way to make working with the list monad even
easier!

## Monads: much more than just lists

Next, creating a SQL-like interface for working with lists!

## Summary

In this lesson we explored how `List` behaves as a `Monad`. It may be
surprising for many people that list comprehensions are equivalent to
`Monads`. Any list comprehension can be written in do-notation, and any
code in do-notation can be rewritten using `>>=` and lambdas.

## Exercises

```haskell
monthsWithDays =
  [ ("jan", 31)
  , ("feb", 28)
  , ("march", 31)
  , ("apr", 30)
  , ("may", 31)
  , ("june", 30)
  , ("july", 31)
  , ("aug", 31)
  , ("sept", 30)
  , ("octo", 31)
  , ("nov", 30)
  , ("dec", 31)
  ]

-- list comprehension
[(day, month) | (month, lastDay) <- monthsWithDays, day <- [1 .. lastDay]]

-- do-notation
result :: [ (Int, String) ]
result =
  (month, lastDay) <- monthsWithDays
  day <- [1 .. lastDay]
  return (day, month)

-- list as container
concatMap (\(month, lastDay) -> map (\day -> (day, month)) [1..lastDay]) monthsWithDays

-- monadic bind and lambdas
monthsWithDays >>= (\(month, lastDay) -> [1..lastDay] >>= (\day -> return (month, day)))
```
