# Using Type Classes

In this lesson you will:

- Implement your own type classes
- Understand polymorphism in Haskell
- Know when to use `deriving`
- Search for documentation with Hackage and Hoogle

## Example

Let's define a six-sided die deriving `Show`

```haskell
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show)
```

Now GHCi can print a simple text version of them

```
Prelude> S1
S1
Prelude> S2
S2
Prelude> S3
S3
```

But that's a bit boring, so we can instead implement the `Show` typeclass
ourselves:

```haskell
instance Show SixSidedDie where
show :: SixSidedDie -> String
show S1 = "one"
show S2 = "two"
show S3 = "three"
show S4 = "four"
show S5 = "five"
show S6 = "six"
```

And now GHCi will show this instead

```
Prelude> S1
one
Prelude> S2
two
Prelude> S3
three
```

## Default implementatio and minimum complete

If we want to compare two dices we will have to implement the `Eq`
typeclass. Let's take a look at it in the console

```
Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```

The minimal comment means that if you implement one of the two fumctions,
then Haskell will be able to figure out the other one automatically.

```
instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False
```

See it in action

```
Prelude> S1 == S2
False
Prelude> S1 /= S2
True
```

Another way to learn more about the `Eq` typeclass is to check out the
hackage page about the module:
https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Eq.html

## Implementing Ord

Let's look at the `Ord` typeclass

```
Prelude> :info Ord
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
```

The `compare` function takes two values and returns an `Ordering`

```
Prelude> :info Ordering
data Ordering = LT | EQ | GT    -- Defined in ‘GHC.Types’
```

Let's see an implementation for a two sided die

```
data TwoSidedDie = One | Two

instance Eq TwoSidedDie where
  (==) One One = True
  (==) Two Two = True
  (==) _ _ = False

instance Ord TwoSidedDie where
  compare Two Two = EQ
  compare Two One = GT
  compare One Two = LT
  compare One One = EQ
```

## To derive or not to derive?

Deriving is super useful because you can avoid writing repetitive code
which is error prone. Let's look at the `Enum` class

```
class Enum a where
  succ :: a -> a
  pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  {-# MINIMAL toEnum, fromEnum #-}
```

If we derive `Enum` for our six-sided die we can see this:

```
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord, Show, En
um)

Prelude> [S1 .. S3]
[S1,S2,S3]

Prelude> [S1 .. ]
[S1,S2,S3,S4,S5,S6]
```

## Tupe classes for more complex types

```
data Name = Name (String, String) deriving (Eq, Show)

names :: [Name]
names = [ Name ("Emil", "Cioran")
        , Name ("Frederich", "Nietzsche")
        , Name ("Eugene", "Thacker")
        ]
```

we could sort them by implementing the `Ord` typeclass

```
instance Ord Name where
  compare (Name (f1, l1)) (Name (f2, l2)) = compare (l1, f1) (l2, f2)

Prelude> sort names
[Name ("Emil","Cioran"),Name ("Frederich","Nietzsche"),Name ("Eugene","Thacker")]
```
