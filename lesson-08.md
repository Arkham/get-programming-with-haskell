# Writing recursive functions

- see common patterns applying rules of recursion
- understand how to use recursion on lists
- learn to time functions in ghci
- reason about edge cases of recursion

## myTake and myDrop

```
myTake :: Int -> [a] -> [a]
myTake n list =
  myTakeAcc n list []

myTakeAcc :: Int -> [a] -> [a] -> [a]
myTakeAcc 0 _ acc = reverse acc
myTakeAcc _ [] acc = reverse acc
myTakeAcc n (x:xs) acc = myTakeAcc (n - 1) xs (x:acc)
```

```
myDrop :: Int -> [a] -> [a]
myDrop 0 list = list
myDrop _ [] = []
myDrop n (x:xs) = myDrop (n - 1) xs
```

## Rules of recursion

1. Identify end goals
2. Determine what happens when a goal is reached
3. List all alternate possibilities
4. Determine your "rinse and repeat" process
5. Ensure each alternative moves you toward the goal

## Recursion on lists

```
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs
```

```
myCycle :: [a] -> [a]
myCycle list = list ++ myCycle list
```

## Ackerman function and collatz conjecture

The Ackerman function takes two arguments m and n:

- if m = 0, return n + 1
- if n = 0, then A(m - 1, 1)
- if both m != 0 and n != 0 then A(m - 1, A(m, n - 1))

In Haskell this would be

```
ackerman :: Int -> Int -> Int
ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))
```

This all looks fine until you run the code

```
Prelude> :set +s
Prelude> ackerman 3 3
61
(0.00 secs, 386,008 bytes)
Prelude> ackerman 3 8
2045
(0.65 secs, 356,041,376 bytes)
Prelude> ackerman 3 9
4093
(2.66 secs, 1,433,240,584 bytes)
Prelude> ackerman 3 10
8189
(10.88 secs, 5,748,295,024 bytes)
```

The Collatz conjecture involves a recursive process given a starting n:

- if n is 1, you're done
- if n is even, repeat with n / 2
- if n is odd, repeat with n * 3 + 1

The boring bit is that it always return 1, so instead write a function that
returns how many steps it took to reach 1

```
collatz :: Integer -> Integer
collatz n = collatzAcc n 1

collatzAcc :: Integer -> Integer -> Integer
collatzAcc 1 steps = steps
collatzAcc n steps =
  case mod n 2 of
    0 ->
      collatzAcc (div n 2) (steps + 1)

    1 ->
      collatzAcc (n * 3 + 1) (steps + 1)
```

## Fibonacci

Naive implementation

```
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

Gets really slow

```
Prelude> fib 35
14930352
(7.86 secs, 3,822,289,344 bytes)
```

Fast implementation

```
fastFib :: Integer -> Integer
fastFib value =
  fastFibAcc 1 1 value

fastFibAcc :: Integer -> Integer -> Integer -> Integer
fastFibAcc n1 _  0 = n1
fastFibAcc _  n2 1 = n2
fastFibAcc n1 n2 2 = n1 + n2
fastFibAcc n1 n2 other = fastFibAcc n2 (n1 + n2) (other - 1)
```

No problemos

```
Prelude> fastFib 35
14930352
(0.00 secs, 125,096 bytes)
```
