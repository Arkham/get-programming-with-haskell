# Closures and partial application

Instead of writing

```
ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven square n
```

we can write a function that builds `ifEvenX` functions

```
genIfEven f = (\x -> ifEven f x)
```

Now you're passing in a function and returning a lambda function.

## Generating URLs for an API

Let's say we want to build a GET request with:

- hostname
- name of the resource you're requesting
- ID of the resource
- API key

It could look like this

`https://example.com/book/1234?token=1337hAsk3ll`

We could write a function like this

```haskell
getRequestURL host apiKey resource id =
  host ++ "/" ++ resource ++ "/" ++ id ++ "?token" ++ apiKey
```

Instead of requiring to pass all arguments every time, we could write a
function that stores the host inside a closure. Then we can build another
function that stores the apiKey inside a closure.

```
genHostRequestBuilder host = (\apiKey resource id ->
  getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
  hostBuilder apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "https://example.com"
exampleApiBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"

exampleApiBuilder "book" "1234" ==
  https://example.com/book/1234?token=1337hAsk3ll
```

## Partial application

In Haskell, if you pass less arguments than what the function has defined,
then the function will be partially applied. That means it will return a
function that will wait for the additional arguments.

```
Prelude> add4 a b c d = a + b + c + d
Prelude> mystery = add4 3
Prelude> mystery 2 3 4
12
Prelude> mystery 5 6 7
21
```

In this way we don't really need to write awkward lambda functions! Going
back to our previous example, we can just write

```
exampleApiBuilder = getRequestURL "https://example.com" "1337hAsk3ll"
```

Partial application is also the reason we created the rule that arguments
should be ordered from most to least general.
