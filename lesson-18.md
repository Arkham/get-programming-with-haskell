# Parameterized Types

In this chapter you'll learn to

- Use parameterized types to make generic data types
- Understand kinds of types
- Write code using the `Data.Map` type to look up values

## Types that take arguments

The most basic parameterized type you can make is a `Box` that serves as a
container for any other type.

```haskell
data Box a = Box a deriving Show
```

and we can use it like this

```
Prelude> data Box a = Box a deriving Show
Prelude> n = 6 :: Int
Prelude> :t Box n
Box n :: Box Int
Prelude> word = "box"
Prelude> :t Box word
Box word :: Box [Char]
Prelude> f = \x -> x
Prelude> :t Box f
Box f :: Box (p -> p)
Prelude> :t Box (Box f)
Box (Box f) :: Box (Box (p -> p))
```

and we could write some helper functions to wrap and unwrap

```haskell
wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box a) = a
```

## A more useful parameterized type: Triple

Let's define a container that holds three values of the same type

```haskell
data Triple a = Triple a a a deriving Show
```

Note that this data structure is different from a tuple containing three
elements, as our `Triple` can only contain three items of the same type.
For example a point in a 3D space can be represented by a `Triple`.

```haskell
type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 30.2 12.4
```

and people's names can be represented as a `Triple` of strings

```haskell
type FullName = Triple String

aPerson :: FullName
aPerson = Triple "Howard" "Phillips" "Lovecraft"
```

In order to access each item in our triple we can define some helper
functions

```
first :: Triple a -> a
first (Triple a _ _) = a

second :: Triple a -> a
second (Triple _ a _) = a

third :: Triple a -> a
third (Triple _ _ a) = a
```

and we can convert our triple into a list

```
toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]
```

or define a function to transform a Triple

```
transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)
```

Here are some examples:

```
Prelude> transform (3 *) aPoint
Triple 0.3 90.6 37.2
Prelude> transform reverse aPerson
Triple "drawoH" "spillihP" "tfarcevoL"
```

### Lists

The most common parameterized type is a `List`. If we look up at its
definition in GHCi, it looks like this

```
Prelude> :info []
data [] a = [] | a : [a]
```

To better undersand how a list works, let's write our own!

```haskell
data List a = Empty | Cons a (List a) deriving Show
```

Now we can compare this to the built-in version of List

```
builtinList1 :: [Int]
builtinList1 = 1:2:3:[]

ourList1 :: List Int
ourList1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinList2 :: [Char]
builtinList2 = 'a':'b':'c':[]

ourList2 :: List Char
ourList2 = Cons 'a' (Cons 'b' (Cons 'c' Empty))
```

Now we can reimplement `map` for our list

```
ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)
```

And use it like this:

```
Prelude> ourMap (*2) ourList1
Cons 2 (Cons 4 (Cons 6 Empty))
```

## Types with more than one parameter

Just like functions, even types can take more than one argument.

### Tuples

Tuples are the most ubiquitous multiparameter type in Haskell; just like
lists they use a built-in type constructor. If you want to use `:info` on a
tuple you can do so by using a commas inside `()`. For example, a two-tuple
is defined like this:

```
Prelude> :info (,)
data (,) a b = (,) a b  -- Defined in ‘GHC.Tuple’
```

### Kinds: types of types

The kind of a type indicates the number of parameters the type takes, which
are expressed using an asterisk `(*)`. So:

- types that take no parameter have the kind `*`
- types that take one parameter have the kind `* -> *`
- types with two parameters have the kind `* -> * -> *`

in GHCi, you can use the `:kind` command to look up the kinds of any types
you're unsure of

```
Prelude> :kind Int
Int :: *
Prelude> :kind Triple
Triple :: * -> *
Prelude> :kind []
[] :: * -> *
Prelude> :kind (,)
(,) :: * -> * -> *
Prelude> import Data.Map
Prelude Data.Map> :kind Data.Map.Map
Data.Map.Map :: * -> * -> *
```

## Data.Map

Let's say you work at a mad scientist laboratory and have a list of numbers
that correspond to various organs used to create hideous monsters. You can
start by making a quick sum type of relevant body parts.

```
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)
```

Suppose you have an inventory of organs:

```
organs :: [Organ]
organs =
  [ Heart, Heart, Brain, Spleen, Spleen, Kidney ]
```

Now suppose that each organ is placed in a numbered drawer and that each
number is unique.

```
ids :: [Int]
ids =
  [ 2, 7, 13, 14, 21, 24 ]
```

Now we have everything that we need to build a map!

```
organCatalog :: Data.Map.Map Int Organ
organCatalog =
  Data.Map.fromList (zip ids organs)
```

And we can use it like this

```
Prelude Data.Map> Data.Map.lookup 7 organCatalog
Just Heart

Prelude Data.Map> Data.Map.lookup 17 organCatalog
Nothing
```

If we wanted to build an organ inventory, so we can ask for how many organs
we have of a certain type, then `Organ` would have to be changed so that we
can use it as a key.

```
data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord)

organInventory :: Data.Map.Map Organ Int
organInventory =
  foldl
    (\acc organ ->
      Data.Map.alter
        (\v ->
          case v of
            Just number ->
              Just (number + 1)

            Nothing ->
              Just 1
        )
        organ
        acc
    )
    (Data.Map.empty)
    organs

getOrganCount :: Organ -> Int
getOrganCount organ =
  Data.Map.findWithDefault 0 organ organInventory
```
