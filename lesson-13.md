# Type Classes

- Understand the basics of type classes
- Read type class definitions
- Use common type classes (Num, Show, Eq, Ord, Bounded)

## An example

Let's look at the type of the `(+)` function

```
Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

The `Num a` typeclass generalizes the idea of a number. All things of class `Num` must have a function `(+)` defined on them. Let's find out more:

```
Prelude> :info Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
        -- Defined in ‘GHC.Num’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
```

Type classes allow you to define functions on a variety of types that you
can't even think of. Let's say you want to write a function that adds and
then doubles a number:

```haskell
addThenDouble :: Num a => a -> a ->
addThenDouble x y = (x + y) * 2
```

This function works not only on `Int` and `Double` but also on anything
that implements the `Num` type class!

## Defining a type class

```haskell
class Describable a where
  describe :: a -> String
```

## Common type classes

```
Prelude> :t (>)
(>) :: Ord a => a -> a -> Bool

Prelude> :info Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

The `Int` data type is an instance of the `Bounded` type class

```
Prelude> :info Int
instance Bounded Int -- Defined in ‘GHC.Enum’

Prelude> :info Bounded
class Bounded a where
  minBound :: a
  maxBound :: a

Prelude> maxBound :: Int
9223372036854775807
Prelude> minBound :: Int
-9223372036854775808

Prelude> maxBound :: Char
'\1114111'
Prelude> minBound :: Char
'\NUL'
```

## Deriving

There is a type class called `Show` that takes care of turning any value
into a `String`

```
Prelude> :info Show
class Show a where
  show :: a -> String
```

When we define our own datatypes, by default they can't be turned into a
`String`

```

Prelude> data Gelato = Chocolate | Vanilla
Prelude> Chocolate

<interactive>:15:1: error:
    • No instance for (Show Gelato) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

But if we derive the `Show` type class then Haskell will automatically do
the right thing™

```
Prelude> data Gelato = Chocolate | Vanilla deriving (Show)
Prelude> Chocolate
Chocolate
```

Not only that, but we could also automatically derive more type classes:

```
Prelude> data Gelato = Chocolate | Vanilla deriving (Show, Eq, Ord)
Prelude> Chocolate == Chocolate
True
Prelude> Chocolate > Vanilla
False
```
