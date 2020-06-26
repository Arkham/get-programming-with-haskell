module Primes where

primes :: [Int]
primes = [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where
    noFactors = filter (\e -> e `mod` nextPrime /= 0) rest
