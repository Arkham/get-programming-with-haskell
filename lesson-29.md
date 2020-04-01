# Lists as Context

After reading this chapter, you'll be able to:

- explain the formal definition of the `Applicative` type class
- represent parameterized types as either containers or contexts
- use `List` as a context to explore nondeterministic computing

## Introducing the Applicative type class

The `Applicative` type class allows you to use functions that are inside a
context, such as `Maybe` or `IO`. Because of the way that `Applicative`
works with `Functor`, `Functor` is a superclass of `Applicative`. See:

```haskell
fmap  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Functor f => (a -> b) -> f a -> f b

(<*>) :: Applicative f => f (a -> b) -> f a -> f b
pure  :: Applicative f => a -> f a
```

Here are some examples of using `<$>` and `<*>`

```
Prelude> (*) <$> Just 6 <*> Just 7
Just 42
Prelude> (*) <$> Just 6 <*> Nothing
Nothing
Prelude> Nothing <*> Just 7
Nothing

Prelude> (++) <$> Just "foo" <*> Just "bar"
Just "foobar"
Prelude> (++) <$> Just "foo" <*> Nothing
Nothing
Prelude> (++) <$> Nothing <*> Just "bar"
Nothing
Prelude> (++) <$> Nothing <*> Nothing
Nothing
```

### The pure method

The `pure` method is required by the `Applicative` type class: it's a
useful function for taking an ordinary value and putting it into a context.
The best way to understand pure is to play around with it:

```
Prelude> pure (1) :: Maybe Int
Just 1
Prelude> pure (1) :: [Int]
[1]
Prelude> pure (1) :: Either String Int
Right 1
```

## Containers vs. contexts

- Parameterized types that represent a container are types that represent a
  data structure.
- When a type is a context, extra information is implied about the type,
  beyond its structure.

The best test whether a type is a container is wherter you can tell what it
does independently of its name. Let's consider the tuple `(a, b)`, you could implement it like this:

```haskell
data Blah a b = Blah a b
```

We can see that this is no different from the regular tuple `(a, b)`.

Another example would be `Data.Map`. Even if it was called `Dictionary`,
`BinarySearchTree`, what the type means would still be implied by the data
structure itself.

On the other hand, when a type is a context, extra information is implied
about the type beyond its structure. The most obvious example is the `IO`
type. If we had a type like this:

```haskell
data Box a = Box a
```

from the data constructor there would be no much difference with `IO`.
Instead we know that `IO` does much more that we can't immediately see.

A good example to understand the distinction between container and context
is to look at lists. We all know a `List` as a container. But what does
`List` mean as a context?

## List as a context

What does this line of code do?

```haskell
pure (+) <*> [100, 200, 300] <*> [50, 2000]
```

A good answer is that we don't know. We are using `pure` to embed the
numeric addition in a `List` structure and applying it with `<*>` to
a first list, and then to another list. If we run it we can see that the
result is:

```
Prelude> pure (+) <*> [100, 200, 300] <*> [50, 2000]
[150,2100,250,2200,350,2300]
```

Now let's look at the types:

```
Prelude> :t pure (+) <*> [100, 200, 300]
pure (+) <*> [100, 200, 300] :: Num a => [a -> a]
```

So it's a list of functions that turn an `a` into another `a`. So it's
basically equivalent to:

```haskell
[((+) 100), ((+) 200), ((+) 300)]
```

If we apply this to our second list, we will see the same result:

```haskell
Prelude> [((+) 100), ((+) 200), ((+) 300)]
[150,2100,250,2200,350,2300]
```

What would happen if we added another function?

```
Prelude> [(+), (*)] <*> [100, 200, 300] <*> [50, 2000]
[150,2100,250,2200,350,2300,5000,200000,10000,400000,15000,600000]
```

Now we can see that the functions are being applied to every combination of
the inputs. So in a way `List` as a context is describing
_nondeterministic_ computation: normally each step of the computation is
followed by another in a precise order that yields a final result. Instead,
in nondeterministic computation, we're computing multiple possibilities all
at once. That's why when we add values in the context of a list, we're
adding together all possible values from the two contexts.

To recap:

- A list as a _container_ is a sequence of values that can hold any type.
  Each item in the list points to the next one or to the empty list.
- A list as a _context_ represents a set of possibilities. We can think of
  it as being a single variable that can contain many possible values.

### A game show example
