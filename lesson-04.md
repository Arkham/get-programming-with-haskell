footer: Funcy Friday Booc Club
build-lists: true

# First-Class Functions

![](images/first-class.jpg)

---

# What is this about

- Understand what's a first-class function
- Use functions as arguments to other function
- Abstract computation out of a function
- Return function as values

---

# Functions as arguments

A function that increases a number by 1 if it's even

```haskell
ifEvenInc n =
  if even n
    then n + 1
    else n
```

---

Functions that double and square the number if it's even

```haskell
ifEvenDouble n =
  if even n
    then n * 2
    else n

ifEvenSquare n =
  if even n
    then n ^ 2
    else n
```

---

We could abstract that by writing a `ifEven` function

```haskell
ifEven myFun x =
  if even x
    then myFun x
    else x
```

---

And use it like this

```haskell
inc n = n + 1
double n = n * 2
square n = n ^ 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n
```

---

Or using anonymous functions

```haskell
ifEvenInc n = ifEven (\x -> x + 1) n
ifEvenDouble n = ifEven (\x -> x * 2) n
ifEvenSquare n = ifEven (\x -> x ^ 2) n
```

---

Fun fact. Another way of writing that

```haskell
ifEvenInc = ifEven (+1)
ifEvenDouble = ifEven (*2)
ifEvenSquare = ifEven (^2)
```

---

Another fun fact. Operator precedence

```haskell
Prelude> 1 + 2 * 3
7

Prelude> add x y = x + y
Prelude> add 1 2 * 3
9
```

Functions have always precedence over operators!

---

Also check out operator precedence:

```haskell
Prelude> :info (+)
[... other stuff ...]
infixl 6 +

Prelude> :info (*)
[... other stuff ...]
infixl 7 *

Prelude> :info (^)
[... other stuff ...]
infixr 8 ^
```

Quiz!

```haskell
2 ^ 2 * 1 + 2
```

---

# Another example! Sorting 📚

---

Suppose you have a tuple with first name and last name:

```haskell
author = ("Judas", "Priest")

Prelude> fst author
"Judas"

Prelude> snd author
"Priest"
```

---

Now we have a list of them

```haskell
names = [ ("Judas", "Priest")
        , ("Black", "Sabbath")
        , ("Iron", "Maiden")
        , ("HallowedBe", "ThyName")
        ]
```

---

We could use the built-in `sort` function

```haskell
Prelude> Data.List.sort names
[("Black","Sabbath"),("HallowedBe","ThyName"),("Iron","Maiden"),("Judas","Priest")]
```

But what if we want to sort by the last name? 🤷‍♂️

---

Let's use `Data.List.sortBy` instead!

We need to provide a sorting function to it:

```haskell
Prelude> :t Data.List.sortBy
Data.List.sortBy :: (a -> a -> Ordering) -> [a] -> [a]

Prelude> :info Ordering
data Ordering = LT | EQ | GT 	-- Defined in ‘GHC.Types’
[... plus other stuff ...]
```

---

```haskell
compareLastNames name1 name2 =
  let
    lastName1 = snd name1
    lastName2 = snd name2
  in
    if lastName1 > lastName2
      then GT
      else if lastName1 < lastName2
        then LT
        else EQ

Prelude> Data.List.sortBy compareLastNames names
[("Iron","Maiden"),("Judas","Priest"),("Black","Sabbath"),("HallowedBe","ThyName")]
```

---

Or you could use `compare` and write

```haskell
Prelude> :t compare
compare :: Ord a => a -> a -> Ordering

compareLastNames (_, lastName1) (_, lastName2) =
  compare lastName1 lastName2

fixedCompareLastNames (firstName1, lastName1) (firstName2, lastName2) =
  compare (lastName1, firstName1) (lastName2, firstName2)
```

---

# Returning functions

__Q__
_When would I want to do that?_

__A__
_When I want to dispatch different behaviours_

---

Imagine we have multiple offices and we want to send letters to people in them

```haskell
locations = [ "PO Box 1234, San Francisco, CA, 94111"
            , "PO Box 789, New York, NY, 10013"
            , "PO Box 456, Reno, NV, 89523"
            ]
```

---

We can write a function like this

  ```haskell
  addressLetter name location =
    nameText ++ " - " location
    where
      nameText = (fst name) ++ " " ++ (snd name)
  ```

- And call it like this

  ```haskell
  Prelude> addressLetter ("Bob","Smith") "PO Box 1234 - San Francisco, CA, 94111"
  "Bob Smith - PO Box 1234 - San Francisco, CA, 94111"
  ```

---

# But the offices want changes!

---

San Francisco wants a different address for people with last names starting with `L`

```haskell
sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName
```

---

New York wants the name followed by a comma rather than a hyphen

```haskell
nyOffice name =
  nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = (fst name) ++ " " ++ (snd name)
```

---

Reno only wants the person's last name

```haskell
renoOffice name =
  nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name
```

---

But now we need a way to dispatch these different functions.  `case` to the rescue!

```haskell
getLocationFun location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))
```

The last case is the _wildcard_ case

---

Now the `addressLetter` function looks like this

```haskell
addressLetter name location =
  locationFun name
  where
    locationFun = getLocationFun location
```

---

# Summary 📝

---

First-class functions allow you to pass functions is as arguments or to return them as values. They also allow you to abstract out computation from your functions and dispatch different behaviours in your code.
