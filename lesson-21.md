# IO

Haskell has a special parametrized type called `IO`. Any value in an `IO`
context must stay in this context. This prevents code that's _pure_ and code
that's necessarily impure from mixing.

In this lesson you'll learn to:

- understand how Haskell handles I/O by using `IO` types
- use do-notation to perform I/O
- write pure programs that interact with the real world

Here's a simple program that reads a name and prints it out

```haskell
helloPerson :: String -> String
helloPerson name = "Hello " ++ name ++ "!"

main :: IO ()
main = do
  putStrLn "Hello! What's your name?"
  name <- getLine
  let statement = helloPerson name
  putStrLn statement
```

And here's a test run

```
Prelude> main
Hello! What's your name?
Ju
Hello Ju!
```

This bit of Haskell code raises SO many questions!

- What is the type `IO`?
- Why is there a `do` after `main`?
- Does `putStrLn` return a value?
- Why are some variables assigned with `<-` and others with `let`?

## IO types -- dealing with an impure world

First of all, we can notice that like `Maybe`, `IO` is a parametrized type

```
Prelude> :kind IO
IO :: * -> *
```

Another thing that `Maybe` and `IO` have in common is that describe a
context for their parameters rather than a container. The context of the
`IO` type is that the value has come from an input/output operation.
Common examples of this include reading user input, printing to standard
out, and reading a file.

The type of the main function we had above is `IO ()`, where `()` is simply
a tuple with zero elements, also known as _unit_. How can that be useful?
Let's look at an example with maybes:

```
Prelude> :type Just (1)
Just (1) :: Num a => Maybe a

Prelude> :type Just (1,2)
Just (1,2) :: (Num a, Num b) => Maybe (a, b)

Prelude> :type Just ()
Just () :: Maybe ()
```

In this case, you could argue that there is very little difference between
`Just ()` and `Nothing`. But if we look again at our `main` function we can
see that the last statement is `putStrLn statement`. What should that
return? In a sense, the purpose of that line of code was sending something
to the outside world, so it's not clear that anything meaningful is going
to come back. But since Haskell needs a type to associate with your main,
you use the `()` tuple to parameterize your `IO` type.

Even with the type system satisfied, there are still some issues. We said
that there are three properties that make functional programming so safe:

- all functions must take a value
- all functions must return a value
- anytime the same argument is supplied the same value must be returned

But clearly `main` doesn't return a value, it simply performs an IO action.
Such actions might not return any value, might take no input and might not
even always return the same value given the same input.

### Examples of IO actions

If you look at `putStrLn` type you can see it also returns an IO action:

```
Prelude> :type putStrLn
putStrLn :: String -> IO ()
```

Just like our main function, it doesn't return any value, only unit. What
about `getLine`?

```
Prelude> :type getLine
getLine :: IO String
```

In this case we can see that we return a `IO String` action.

Let's look at another example:

```
module DiceRoll where

import System.Random

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
  dieRoll <- randomRIO (minDie, maxDie)
  print dieRoll
```

When you run this program it returns a new die roll each time!

```
*DiceRoll> main
4
*DiceRoll> main
6
*DiceRoll> main
2
```

Every time you call `randomRIO` you get a different result. So just like
`putStrLn` and `getLine`, this is another example of IO action.

### Keeping values in the context of IO

Because I/O is so unpredictable, after you have a value come from I/O,
Haskell doesn't allow you to use that value outside of the context of the
`IO` type. For example, if you get a random number using `randomRIO`, you
can't use that value outside `main`.

When we are dealing with `Maybe` values we can pattern match the `Just` and
`Nothing` cases to extract the value from the context.

With I/O, an endless variety of problems occur. Because of this, after
you're working with data in the context of `IO`, it must stay there.

## Do notation

Since we can't escape from the context of `IO`, it would be nice to have a
convenient way to perform a sequence of computations within that context.
This is exactly the purpose of the special `do` keyword.

This is also why some variables use `let` and others use `<-`. Variables
assigned with `<-` allow you to act as though a type `IO a` is just of type
`a`. Instead you use `let` statements whenever you create variables that
aren't `IO` types.

This explains why we can combine a function like `getLine` that returns a
`IO String` with the `helloPerson` function, which type is `String -> String`.

If we were to annotate exactly what happens at each stage of our `main`
function this is how it would look like:

```
main :: IO ()
main = do
  putStrLn "Hello! What's your name?" -- IO ()
  name <- getLine -- String <- IO String
  let statement = helloPerson name -- String
  putStrLn statement -- IO ()
```

What is powerful about this is that you can blend functions that work with
safe, non-`IO` values and use them seamlessly with data in an `IO` context.

## An example: command-line pizza cost calculator

Check out `Pizza.hs`

### A peek at Monad--do-notation in Maybe

Do-notation has nothing to do with `IO` in particular: we can use it
because `IO` is a member of a powerful typeclass called `Monad`. But even
`Maybe` is a member of the same typeclass, and therefore can use the same
notation.

Suppose that instead of getting your pizza values from user inputs, you had
to get them from two `Maps`: one with sizes and one with costs.

```
costData :: Map.Map Int Double
costData = Map.fromList [(1, 18.0), (2, 16.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 20.0), (2, 15.0)]

maybeMain :: Maybe String
maybeMain = do
  size1 <- Map.lookup 1 sizeData
  cost1 <- Map.lookup 1 costData
  size2 <- Map.lookup 2 sizeData
  cost2 <- Map.lookup 2 costData
  let pizza1 = ( size1, cost1 )
  let pizza2 = ( size2, cost2 )
  let betterPizza = cheaperPizza pizza1 pizza2
  return (describeBetterPizza betterPizza)
```

As you can see, the code is not much different to the previous version that
returned `IO ()`. The new thing is that you've added the `return` function,
which takes a value of a type and puts it back in the context of the
do-notation. This is how it looks:

```
*Pizza> Pizza.maybeMain
Just "The 15.0 pizza is cheaper at 9.054147873672269e-2 per square inch"
```

The `Monad` typeclass allows you to write general programs that can work in
a wide range of contexts. Because of do-notation, you could write a
different program, using all the same core functions as your original.

## Summary

The trouble with I/O is that it requires all features of functions that we
removed earlier in the book: it often changes the state of the world, and
likewise frequently causes values to return different results each time a
function is called. Haskell handles this by ensuring that all I/O logic is
contained in an `IO` type: once a value is inside it you can never remove
it. To make working with `IO` types easier, Haskell has a special
do-notation that allows you to write code as though you weren't in the
context of an `IO` type.
