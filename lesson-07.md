# Rules for recursion and pattern matching

- understand the definition of a recursive function
- learm the rules for writing recursive functions
- walk through examples of recursive funcction definitions
- use basic pattern amtching to solve recursive problems

## Recursion

Let's try to express how to find the greatest common divisor (GCD)

1. You start with two numbers, a and b
2. If `rem a b` is zero then b is the GCD
3. Otherwise, you change the value of a by assigning it the value of b.
   Then you assign to b the value you got in step 2.
4. Now you repeat until step 2 yields zero.

Let's try with an example

- a = 20, b = 16
- rem 20 16 == 4
- a = 16, b = 4
- rem 16 4 == 0
- GCD is 4

In code

```
gcd :: Int -> Int -> Int
gcd a b =
  case rem a b of
    0 -> b
    other -> gcd b other
```
