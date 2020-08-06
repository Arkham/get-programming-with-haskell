# Working with JSON data by using Aeson

- Transform Haskell types into JSON
- Read JSON into a Haskell type
- Use the `DeriveGeneric` extension to implement needed classes
- Write your own instance of `ToJSON` and `FromJSON`

## Getting set up

JSON only supports a few simple types:

- objects
- strings
- numbers
- booleans
- lists

We will be using the Aeson library to translate back and forth between
Haskell's data types and JSON. We will rely on two key functions, `encode`
and `decode`.

**Setting up**

`app/Main.hs`

```haskell
module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

main :: IO ()
main = print "hi"
```

`package.yaml`

```
dependencies:
- base >= 4.7 && < 5
- aeson
- bytestring
- text

default-extensions:
- OverloadedStrings
- DeriveGeneric
```

## Using the Aeson library

First we want to take JSON and transform it into a haskell type.

```haskell
decode :: FromJSON a => ByteString -> Maybe a
```

You see that if the decoding 'fails' we just get `Nothing`. Instead we
could use:

```haskell
eitherDecode :: FromJSON a => ByteString -> Either String a
```

If we want to take a Haskell type and turn it into JSON we can use:

```haskell
encode :: ToJSON a => a -> ByteString
```

**Quick Check**
Why does `encode` return a `ByteString` instead of a `Maybe ByteString`

## Making your data types instances of FromJSON and ToJSON

Let's try this out with a simple data type

```haskell
data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving Show
```

The easy way to make the `Book` type both an instance of `FromJSON` and
`ToJSON` is to use a language extension called `DeriveGeneric`. We can do
so by deriving `Generic` and declaring our type an instance of those
typeclasses.

```haskell
data Book = Book
  { title :: T.Text
  , author :: T.Text
  , year :: Int
  } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book
```

Now we can use it straight away

```haskell
myBook :: Book
myBook = Book { author = "Ju", title = "Bottle", year = 2020 }

myBookJSON :: BC.ByteString
myBookJSON = encode myBook
-- "{\"year\":2020,\"author\":\"Ju\",\"title\":\"Bottle\"}"

bookFromJSON :: Maybe Book
bookFromJSON = decode myBookJSON
-- Just (Book {title = "Bottle", author = "Ju", year = 2020})
```

What if there is an error in the JSON and we can't decode it?

```haskell
*Main> decode "{\"author\":\"Ju\"}" :: Maybe Book
Nothing

*Main> eitherDecode "{\"author\":\"Ju\"}" :: Either String Book
Left "Error in $: parsing Main.Book(Book) failed, key \"title\" not found"
```

🎉

**Quick check**
Use Generic and Aeson to make this type work with JSON

```haskell
data Name = Name
  { firstName :: T.Text
  , lastName :: T.Text
  } deriving (Show)
```

### Writing your own instance of FromJSON and ToJSON

Let's say you get this type of error response from an API:

```haskell
sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"
```

We could try deriving generic again

```haskell
data ErrorMessage = ErrorMessage
  { message :: T.Text
  , error :: Int
  } deriving (Show, Generic)

instance FromJSON ErrorMessage
```

And then try using it:

```
*Main> decode sampleError :: Maybe ErrorMessage
Just (ErrorMessage {message = "oops!", error = 123})
```

All good so far! What if we try to read that error field?

```
*Main> fmap error $ (decode sampleError :: Maybe ErrorMessage)

<interactive>:52:6: error:
    Ambiguous occurrence ‘error’
    It could refer to
       either ‘Prelude.error’,
              imported from ‘Prelude’ at /Users/arkham/Desktop/get-programming-with-haskell/json-lesson/app/Main.hs:1:8-11
              (and originally defined in ‘GHC.Err’)
           or ‘Main.error’,
              defined at /Users/arkham/Desktop/get-programming-with-haskell/json-lesson/app/Main.hs:39:5
```

Uh oh. It seems we can't have a field with the same name of a builtin
function. So we have to change our type to look like this:

```haskell
data ErrorMessage = ErrorMessage
  { message :: T.Text
  , errorCode :: Int
  } deriving Show
```

But now our datatype doesn't match with the JSON payload any longer...

It seems we will have to write our own JSON parsing after all:

```haskell
instance FromJSON ErrorMessage where
  parseJSON (Object v) =
    ErrorMessage <$> v .: "message"
                 <*> v .: "error"
```

Quite a lot to unpack here!

`parseJSON (Object v)` v will be the value of the JSON object.

We use a combination of fmap and apply to keep building our Haskell type
until we suceed. If we encounter an error, that will stop the parsing. See
this:

```haskell
exampleMessage :: Maybe T.Text
exampleMessage = Just "OOOPSIE"

exampleError :: Maybe Int
exampleError = Just 123

*Main> ErrorMessage <$> exampleMessage <*> exampleError
Just (ErrorMessage {message = "OOOPSIE", error = 123})

*Main> ErrorMessage <$> exampleMessage <*> Nothing
Nothing
```

Now the last mystery is that `.:` operator!

```
*Main> :t (.:)
(.:) :: FromJSON a => Object -> Text -> Parser a
```

So this operator takes your JSON object and some text and returns a value
parsed into a `Parser` context. Let's try it now:

```haskell
*Main> decode sampleError :: Maybe ErrorMessage
Just (ErrorMessage {message = "oops!", errorCode = 123})

*Main> fmap errorCode $ (decode sampleError :: Maybe ErrorMessage)
Just 123
```

What if we want to convert it back to JSON?

```haskell
instance ToJSON ErrorMessage where
  toJSON (ErrorMessage m e) =
    object [ "message" .= m
           , "error" .= e
           ]

*Main> encode (ErrorMessage {message = "oops!", errorCode = 123})
"{\"error\":123,\"message\":\"oops!\"}"
````

Similar to the `.:` we have now the `.=` operator

```haskell
*Main> :t (.=)
(.=) :: (KeyValue kv, ToJSON v) => Text -> v -> kv
```

## Reading your NOAA data

Grab a copy of [this
json](https://gist.githubusercontent.com/willkurt/9dc14babbffea1a30c2a1e121a81bc0a/raw/12a2269815997466f2a7911efbfdf27775cfbdd1/data.json).

```json
{
   "metadata":{
      "resultset":{
         "offset":1,
         "count":11,
         "limit":25
      }
   },
   "results":[
      {
         "uid":"gov.noaa.ncdc:C00861",
         "mindate":"1763-01-01",
         "maxdate":"2017-02-01",
         "name":"Daily Summaries",
         "datacoverage":1,
         "id":"GHCND"
      },
```

We can see we have a `metadata` field and a `results` field. Let's start
modeling the latter:

```haskell
data NOAAResult = NOAAResult
  { uid :: T.Text
  , mindate :: T.Text
  , maxdate :: T.Text
  , name :: T.Text
  , datacoverage :: Int
  , resultId :: T.Text
  } deriving Show
```

Since we have to rename the `id` field into `resultId`, we have to write
our own instance of `FromJSON`:

```haskell
instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"
 ``` 

 Now let's deal with the metadata and the whole response:

 ```
data Resultset = Resultset
  { offset :: Int
  , count :: Int
  , limit :: Int
  } deriving (Show, Generic)

instance FromJSON Resultset

data Metadata = Metadata
  { resultset :: Resultset
  } deriving (Show, Generic)

instance FromJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata
  , results :: [NOAAResult]
  } deriving (Show, Generic)

instance FromJSON NOAAResponse
```

Now we need a function that prints the result of the parsing

```haskell
printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
  forM_ results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
```

Let's take it for a spin!

```
*Main> main
"error loading data"
```

Uh oh. That's why we should never use `Maybe` for parsing results :)

```
printResults :: Either String [NOAAResult] -> IO ()
printResults (Left err) = print err
printResults (Right results) = do
  forM_ results (print . name)

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
```

```
*Main> main
"Error in $.results[3].datacoverage: parsing Int failed, value is either floating or will cause over or underflow 0.95"
```

Ah cool, so `datacoverage` needs to be a float.

```
*Main> main
"Daily Summaries"
"Global Summary of the Month"
"Global Summary of the Year"
"Weather Radar (Level II)"
"Weather Radar (Level III)"
"Normals Annual/Seasonal"
"Normals Daily"
"Normals Hourly"
"Normals Monthly"
"Precipitation 15 Minute"
"Precipitation Hourly"
```

## Summary

We learned how to use Aeson to parse and encode JSON, either using
`DeriveGeneric` or implementing an instance of the `FromJSON` and `ToJSON`
typeclasses ourselves.
