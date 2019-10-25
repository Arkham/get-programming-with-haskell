# Interacting with the command line and lazy I/O

In this lesson we will:

- Access command-line arguments
- Use traditional approach to interact through I/O
- Write I/O code using lazy evaluation to make I/O easier

An I/O stream is nothing but a lazily evaluated list of characters.

## Interacting with the command line the nonlazy way

Let's design a program that reads an arbitrarily long list of numbers
entered by a user, and then adds them all up. The tricky bit is that we
don't know in advance how many items the user is going to enter.

One way to solve the problem is to ask!

```
$ ./sum 4
"enter your numbers"

3
5
9
25

"your total is 42"
```

To get arguments you can use the `getArgs` function from
`System.Environment`:

```haskell
getArgs :: IO [String]
```

Let's try out on a simple program:

```haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  map putStrLn args
```

Unfortunately `map putStrLn args` won't work because `args` isn't an
ordinary list and it will error out like this

```
Expected type: IO ()
Actual type: [IO ()]
```

So instead we can use `mapM` and write

```
mapM putStrLn args
```

Unfortunately this errors out as well with

```
Expected type: IO ()
Actual type: IO [()]
```

So we said that the final IO should be `()` but we actually return a list
of `()`. We can fix that by using a version of `mapM` that throws away the
result called `mapM_`

```haskell
mapM putStrLn args
```

Now we can read the first argument and keep asking for more numbers

```
module Main where

import Control.Monad
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  -- mapM_ putStrLn args
  let linesToRead =
        if length args > 0
          then read (head args)
          else 0 :: Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  print (sum ints)
```

and run it too

```
$ stack exec examples-exe 4
1
23
4
5
33
```

### Quick recap: functions from IO

- **mapM** takes an `IO` action and a regular list, performing the action
  on each item in the list and returning a list in the `IO` context
- **mapM\_** same as `mapM` but throws away the result
- **replicateM** take an `IO` action and an `Int` n, then repeats the
  action n times, returning the results in an `IO` list
- **replicateM\_** same as `replicateM` but throws away the result

## Interacting with lazy I/O

Our last program required the user to input the specific number of lines
needed. But what if the user didn't know that?

The trick is to think of the stream of data coming from the user as an
infinite list of `Chars`. Now we only one special action `getContents`:

```
main :: IO ()
main = do
  userInput <- getContents
  mapM_ print userInput
```

What happens when we run it?

```
$ stack exec examples-exe
hello
'h'
'e'
'l'
'l'
'o'
'\n'
foobar
'f'
'o'
'o'
'b'
'a'
'r'
'\n'
^D
```

The beauty of `getContents` is that we don't have to worry about `IO` any
longer, but we can just focus on solving a problem with a list of strings.

If we have something like:

```
Prelude> sample = ['6', '2', '\n', '2', '1', '\n']
Prelude> lines sample
["62","21"]
```

Now we can convert the strings into `Ints`

```haskell
toInts :: String -> [Int]
toInts = map read . lines
```

and our function becomes

```haskell
main :: IO ()
main = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)
```

which does this

```
$ stack exec examples-exe
10
2
34
<^D>
46
```

This is so much nicer than before, since your users don't need to remember
how many numbers they want to sum beforehand!

## Summary

In this lesson, we have seen how to use the standard way of treating I/O.
Then we saw how we can take advantage of Haskell's lazy evaluation to treat
I/O in a lazy way.
