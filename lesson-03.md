# Lambda functions

A lambda function is commonly known as an anonymous function.

```
\x -> x
 |    ↳  body of lambda function
 |
 ↳  function argument
```

To run it in GHCi, we have to pass a value

```
Prelude> (\x -> x) 4
4
```

## Writing your own `where`

```
sumSquareOrSquareSum :: Integer -> Integer -> Integer
sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

lambdaSumSquareOrSquareSum :: Integer -> Integer -> Integer
lambdaSumSquareOrSquareSum x y =
  (\sumSquare squareSum ->
     if sumSquare > squareSum
       then sumSquare
       else squareSum)
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)
```

## From lambda to let

A `let` expression allows to combine the readability of a `where` clause
with the power of your lambda function.

```
letSumSquareOrSquareSum :: Integer -> Integer -> Integer
letSumSquareOrSquareSum x y =
  let sumSquare = x ^ 2 + y ^ 2
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum
```

## Global variables are dangerous

Nuff said.

## Summary

Lambda functions are functions with no name. It makes easier to write
functions on the fly and they allow you to create a new scope whenever you
want.
