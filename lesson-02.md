# Functions and functional programming

All functions in Haskell follow three rules:

- All functions must take an argument
- All functions must return a value
- Anytime a function is called with the same argument, it must return the same value (referential transparency)

## Variables

Instead of

```
calcChange owed given =
  if given - owed > 0
    then given - owed
    else 0
```

Write

```
calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed
```


