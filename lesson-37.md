# Capstone: building a prime number library

In this chapter we will:

- build a new project using stack
- write basic library functions to work with prime numbers
- use stack test and quickCheck to check for bugs
- refactoring code to fix errors
- adding new functions and tests to your project as you go

We will focus on three problems:

1. List out primes less than a specified number
2. Determine whether a given number is prime
3. Break a number into its prime factors

We will generate prime numbers using a sieve, passing a list of integers
and expecting a list of primes in return:

```haskell
sieve :: [Int] -> [Int]
```

Then we will be able to check if a number is prime. Note that the question
doesn't make sense for negative numbers, so we return a Maybe

```haskell
isPrime :: Int -> Maybe Bool
```

And finally we'll want to factor numbers into primes.

```haskell
primeFactors :: Int -> Maybe [Int]
```
