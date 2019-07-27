# Type Basics

- Understand basic types in Haskell (Int, String, Double)
- Read type signatures for functions
- Use simple type variables

## Types in Haskell

Haskell uses type inference to automatically determine the types of all
values at compile time based on the way they're used.

```haskell
x :: Int
x = 2
```

All types in Haskell start with a capital letter. An `Int` can represent up
to `9223372036854775807`. When we try to represent a bigger number this
happens:

```
Prelude> :{
Prelude| x :: Int
Prelude| x = 2
Prelude| :}
Prelude> x^2000
0
```

The value overflowed! Notice that an `Integer` does not have the same
issue

```
Prelude> :{
Prelude| x :: Integer
Prelude| x = 2
Prelude| :}
Prelude> x ^ 2000
<very long number>
```

Of course a lot of types are available

```haskell
letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

values :: [Int]
values = [1,2,3]

testScores :: [Double]
testScores = [0.99, 0.7, 0.8]

letters :: String
letters = ['a', 'b', 'c']
-- "abc"
```

### Tuples

```
ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

firstLastMiddle :: (String, String, Char)
firstLastMiddle = ("Oscar", "Grouch", 'D')
```

## Function types

```
double :: Int -> Int
double = n = n * 2
```

What if instead we wanted to write a function that halves a number?

```
half :: Int -> Double
half n = n / 2
```

This would trigger a compilation error!

```
<interactive>:22:1: error:
    • No instance for (Fractional Int) arising from a use of ‘/’
    • In the expression: (1 :: Int) / 2
      In an equation for ‘it’: it = (1 :: Int) / 2
```

So instead we would have to write

```
half :: Int -> Double
half n = (fromIntegral n) / 2
```

```
Prelude> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
```

Notice that you don't need to call `fromIntegral` from GHCi because literal
numbers are polymorphic: i.e. the type is determined by the compiler based
on the way they're used

```
Prelude> 5 / 2
2.5

Prelude> (5 :: Int) / 2
<interactive>:27:1: error:
```

### Functions converting to and from strings

```
Prelude> show 5
"5"
Prelude> show 'c'
"'c'"
Prelude> show 6.0
"6.0"
```

What if we wanted to do the opposite?

```
Prelude> read "6"
*** Exception: Prelude.read: no parse
Prelude> read "6" :: Int
6
Prelude> read "6" :: Integer
6
Prelude> read "6" :: Double
6.0
Prelude> read "[1,2,3]" :: [Int]
[1,2,3]
```

### Types for first-class functions

```
ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
             then f n
             else n
```

### Type variables

```
simple :: a -> a
simple x = x

makeTriple :: a -> b -> c -> (a, b, c)
makeTriple x y z = (x, y, z)
```
