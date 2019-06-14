module Lesson03 where

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

letSumSquareOrSquareSum :: Integer -> Integer -> Integer
letSumSquareOrSquareSum x y =
  let sumSquare = x ^ 2 + y ^ 2
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum
        then sumSquare
        else squareSum
