# Working with Text and Unicode

After reading lesson 23, you'll be able to:

- Use the `Text` type for more efficient text processing
- Change Haskell's behaviour with language extensions
- Program by using common text functions
- Use `Text` to properly handle Unicode text

## The `Text` type

The problem with `String` is that it's super inefficient: representing a
string as a list of characters is expensive in terms of both time and
space.

In commercial Haskell code, `Text` is strongly preferred over `String`.
The `Text` type can be found inside the `Data.Text` module. Usually it's
imported in a qualified manner as such:

```
import qualified Data.Text as T
```

Under the hood, this string representation is implemented as an array. Also
it doesn't use lazy evaluation (if you really need it, you can use
`Data.Text.Lazy` instead).

## Using Data.Text

`Data.Text` has two functions, `pack` and `unpack`, which can be used to
convert from `String` to `Text` and vice versa. Remember that this isn't
computationally cheap because you have to traverse the whole string.

## OverloadedStrings and Haskell extensions

If you try to assign a normal string to a `Text` you get an error:

```
Prelude T> myWord :: T.Text; myWord = "dog"

<interactive>:9:28: error:
    • Couldn't match expected type ‘Text’ with actual type ‘[Char]’
    • In the expression: "dog"
      In an equation for ‘myWord’: myWord = "dog"
```

Notice that the same problem doesn't happen with numbers

```
Prelude T> num1 :: Int; num1 = 3
Prelude T> num1 :: Integer; num1 = 3
Prelude T> num1 :: Double; num1 = 3
```

As you can see the same literal `3` can be assigned to different types.
Luckily, there's a GHC _language extension_ that allows you to do that with
strings as well, `OverloadedStrings`. In order to use it, you have to add a
special comment at the top of your Haskell source file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

aWord :: T.Text
aWord = "Cheese"
```

or you can also do it in GHCi

```
Prelude T> :set -XOverloadedStrings
Prelude T> myWord :: T.Text; myWord = "dog"
```

Some other useful language extensions you might bump into are:

- `ViewPatterns` more sophisticated pattern matching
- `TemplateHaskell` metaprogramming tools
- `DuplicateRecordFields` fixes the annoying problem where using the same
  field name for different types using the record syntax causes a
  conflict
- `NoImplicitPrelude` allows to use a custom `Prelude`

### Basic Text utilities

```
Prelude T> sampleInput :: T.Text; sampleInput = "this is\ninput"

Prelude T> T.lines sampleInput
["this is","input"]

Prelude T> T.words sampleInput
["this","is","input"]

Prelude T> T.splitOn " " sampleInput
["this","is\ninput"]
Prelude T> T.splitOn "is" sampleInput
["th"," ","\ninput"]

Prelude T> T.unlines (T.lines sampleInput)
"this is\ninput\n"
Prelude T> T.unwords (T.words sampleInput)
"this is input"

Prelude T> T.intercalate ", " ["foo", "bar", "baz"]
"foo, bar, baz"

Prelude T> sampleInput <> " " <> sampleInput
"this is\ninput this is\ninput"
```

## Text and Unicode

The `Text` type has excellent support for working with Unicode text. To
demonstrate that, we can build a simple program that highlights words in
text.

Your program will take a text query and a body of text, and use curly
braces `{}` to highlight all cases of the word you're looking for. For
example if _dog_ is your query text and your main text is _a dog walking
dogs_ you'd expect this output:

```
a {dog} walking {dog}s
```

In this task, you will highlight the Sanskrit word _dharma_ in a sample
text from the _Bhavagad Gita_. The word _dharma_ has many meanings, ranging
from _duty_ to references of cosmic order and divine duty. Here's the word
written in Devaganari script:

```
dharma :: T.Text
dharma = "धर्म"
```

Here's the sample text:

```
bgText :: T.Text
bgText = "श्रेयान्स्वधर्मोविगुणःपरधर्मात्स्वनुष्ठितात्।स्वधर्मेनिधनंश्रेयःपरधर्मो"
```

In English, you might just want to use `T.words`, but Sanskrit is more
complicated: whenever words are combined when speaking a sentence, they end
up combined in text. So what we need to do is to split our text on the
target query, wrap the query with brackets, then put it all back together.

```
highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
  where pieces = T.splitOn query fullText
        highlighted = mconcat ["{", query, "}"]
```

## Text I/O

Now that we have our `highlight` function, we want to print the results.
The problem is that most functions in the `IO` module deal with strings,
and so we would need to unpack our `T.Text` in order to use them. Instead
we can use another module

```
import qualified Data.Text.IO as TIO
```

This module exposes functions which match the `IO` module, but instead work
with `T.Text` data types.

## Summary

In this chapter we saw how to efficiently process text in Haskell using
`Data.Text`. Although strings as a list of characters are a very nice
abstraction, in practice they can lead to poor performance. One possible
issue is that Haskell by default doesn't recognize that a double quoted
string can be a `Data.Text`, but this can be fixed by the
`OverloadedStrings` language extension.
