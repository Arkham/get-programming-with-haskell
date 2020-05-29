# Property testing with QuickCheck

In this lesson, we will:

- use `stack ghci` to interact with a stack project
- run tests with `stack test`
- use QuickCheck for property testing
- install packages using `stack install`

## Starting a new project

```
stack new palindrome-testing
cd palindrome-testing
stack setup
stack build
```

We'll add a small module:

```
module Lib
  ( isPalindrome
  ) where

isPalindrome :: String -> Bool
isPalindrome text = text == reverse text
```

We can load it and play around with it using

```
stack ghci
```

Remember you can reload your code using `:r`

## A wild spec appears

```
import Lib

assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement =
  if test
     then putStrLn passStatement
     else putStrLn failStatement
```

```
main :: IO ()
main = do
  putStrLn "Running tests..."
  assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
  assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
  assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
  assert (not . isPalindrome $ "cat") "passed 'cat" "FAIL: 'cat"
  putStrLn "done!"
```

## Property testing with QuickCheck

```
import Lib
import Test.QuickCheck
import Data.Char (isPunctuation)

prop_punctuationInvariant text =
  preprocess text == preprocess noPuncText
    where noPuncText = filter (not . isPunctuation) text

main :: IO ()
main = do
  quickCheck prop_punctuationInvariant
  quickCheckWith stdArgs { maxSuccess = 1000} prop_punctuationInvariant
  putStrLn "done!"
```

In order to be able to generate the input values, the type has to be an
instance of `Arbitrary`. This is not included by default, which means that
few types will work out of the box.

The good news is that you can install a package which includes a lot of
them:

```
stack install quickcheck-instances
```

One of the included instances is for `Data.Text`, so let's try it out by
refactoring our existing program to operate on that type instead.

```
module Lib
    ( isPalindrome
    , preprocess
    ) where

import Data.Char (isPunctuation)
import Data.Text as T

preprocess :: T.Text -> T.Text
preprocess str =
  T.filter (not . isPunctuation) str

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
  where cleanText = preprocess text
```

and this is our new spec

```
import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char (isPunctuation)
import Data.Text as T

prop_punctuationInvariant text =
  preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000} prop_punctuationInvariant
  putStrLn "done!"
```

## Summary

In this lesson, we first started manually testing our code with `stack
ghci`. Then we wrote a series of simple unit tests that we could run with
`stack test`. Finally, we used QuickCheck to create property tests that
automatically shrink to the smallest reproductible example!

## Quiz

Complete this palindrome-testing project so it’s the same as the code in
the preceding lesson. Then implement property tests to fully test
`preprocess`, ensuring that whitespace doesn’t matter and that
capitalization doesn’t matter.
