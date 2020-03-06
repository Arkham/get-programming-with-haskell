# A peek at the Applicative type class: using functions in a context

In this lesson we will:

- build an application that handles missing data
- extend the power of the `Functor` type class with the `Applicative` type
- use `Applicative` to use one data model in many contexts

In the previous lesson, we learned how to use the `Functor` type class to
perform computation inside a container such as a `List` or a context such
as `Maybe` or `IO`.

In this chapter, we will see how we can use functions that are themselves
in a context.

## A command line application for calculating the distance between cities

We want to build a CLI that calculates the distance between cities. The big
challenge is to ensure that your app fails gracefully when the user inserts
a city which is missing!

```haskell
type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB =
  Map.fromList
    [ ("Arkham", (42.6054, -70.7829))
    , ("Innsmouth", (42.8250, -70.8150))
    , ("Carcosa", (29.9714, -90.7694))
    , ("New York", (40.7776, -73.9691))
    ]
```

We'll use the _haversine distance_ to calculate the distance of two points
located on a sphere. We don't really care about the implementation, but we
do care about the type signature:

```haskell
haversine :: LatLong -> LatLong -> Double
```

Now we want to make that CLI! It should:
- ask the user to insert two cities
- if both cities are found in the db, print the distance
- otherwise, print out an error message

It's often easier to start at the end, so what's the shape of our final
function? We will likely get a `Maybe Double` that represents the final
distance, which we can use the send a message to the user:

```haskell
printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")
```

But now we need to take into consideration the fact that some cities
couldn't be found.

```haskell
magicFunction :: Maybe LatLong -> Maybe LatLong -> Maybe Double
```

Unfortunately, the shape of our function is different

```haskell
haversine :: LatLong -> LatLong -> Double
```

So, what can we do?

We could write a function that pattern matches on both cities

```
haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
haversineMaybe Nothing _ = Nothing
haversineMaybe _ Nothing = Nothing
haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)
```

What's the problem with this?

Well, it's boring AF. Also if we ever changed the wrapper (i.e. `IO`
instead of `Maybe`) we would have to write a `haversineIO`. Could we fix
this using `Functor`?

## The limitations of Functor

Let's look again at the type signature of `fmap`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

Now what we want to do with our function is

```
Maybe LatLong -> Maybe LatLong -> Maybe Double
```

So basically we would need another argument added

```haskell
magicFn :: Functor f => (a -> b -> c) -> f a -> f b -> f c
```

🥨 Guess what? Elm has this and is called `map2`

## Using <*> for partial application in a context

Let's look at this!

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

Quickly comparing it to `fmap`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

You can see that the difference is that now the function *itself* has been
wrapped in the same context!

So if we defined a function called `maybeInc`

```haskell
Prelude> maybeInc = (+) <$> Just 1
Prelude> :t maybeInc
maybeInc :: Num a => Maybe (a -> a)
```

We could apply it like this:

```
Prelude> maybeInc <*> Nothing
Nothing

Prelude> maybeInc <*> Just 1
Just 2
```

But that's not all, we also found a way to use normal binary functions
inside a `Maybe` context!

```
Prelude> :t (++)
(++) :: [a] -> [a] -> [a]


Prelude> (++) <$> Just "cats" <*> Just " and dogs"
Just "cats and dogs"
Prelude> (++) <$> Just "cats" <*> Nothing
Nothing
Prelude> (++) <$> Nothing <*> Just " and dogs"
Nothing
Prelude> (++) <$> Nothing <*> Nothing
Nothing
```

🎸 🎸 🎸

So now the haversine function that we need can be simply written as

```haskell
haversine <$> startingCity <*> destCity
```

which type is going to be

```
Maybe LatLong -> Maybe LatLong -> Maybe Double
```

Our final main looks like this

```haskell
main :: IO ()
main = do
  putStrLn "Enter the starting city name: "
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name: "
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance
```

### A quick test with IO

`IO` is also a member of `Applicative`. Let's try to make a simple command
line tool that returns the minimum of three numbers entered by the user.

Let's start with a pure function

```haskell
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)
```

Now we can build a simple function that reads an `Int`

```haskell
readInt :: IO Int
readInt = read <$> getLine
```

To combine these together we can just write

```haskell
minOfInts :: IO int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt
```

And the main would look like that

```
main :: IO ()
main = do
  putStrLn "Enter three numbers"
  result <- minOfInts
  putStrLn (show result ++ " in the smallest")
```

## Using <*> to create data in a context

Suppose you have user data for a video game:

```haskell
data User = User
  { name :: String
  , gamerId :: Int
  , score :: Int
  } deriving Show
```

And here's how we would create one

```
Prelude> User {name = "Sue", gamerId = 1337, score = 9001}
User {name = "Sue", gamerId = 1337, score = 9001}
Prelude> User "Sue" 1337 9001
User {name = "Sue", gamerId = 1337, score = 9001}
```

Now if we wanted to create a User when all fields could be empty, we would
like a function like this:

```
Maybe String -> Maybe Int -> Maybe Int -> Maybe User
```

So we would just do

```
main :: IO ()
main = do
  putStrLn "Enter a username, a gamerId and score"
  user <- User <$> getLine <*> readInt <*> readInt
  print user
```

Pretty 🕷
