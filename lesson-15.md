# Secret messages!

In this lesson you will learn:

- Basics of cryptography
- Using basic types to model your data
- Making practical use of `Enum` and `Bounded`
- Writing and making instances of your own `Cypher` class

## Implement your own ROT cipher

```haskell
data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)
```

We want to write a function that, when applied twice, can get us back to
the original plaintext. So we have to:

- take the letter we want to encrypt
- figure out how many letters are in the alphabet
- find the 'opposite' letter in the alphabet by adding half of the size of
  the alphabet to the offset of the letter
- modulo the resulting number by the size of the alphabet (so we don't
  overflow)

```
rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN alphabetSize c = toEnum rotation
  where halfAlphabet = alphabetSize `div` 2
        offset = fromEnum c + halfAlphabet
        rotation = offset `mod` alphabetSize
```

If we want to rotate chars then we could avoid passing in the alphabet size

```
rotChar :: Char -> Char
rotChar c = rotN alphabetSize c
  where alphabetSize = 1 + fromEnum (maxBound :: Char)
```

Let's see it in action

```
Prelude> rotChar 'a'
'\557153'
Prelude> rotChar $ rotChar 'a'
'a'
```
