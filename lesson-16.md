# Creating types with "and" and "ord"

In this lesson you'll be able to

- Understand product types in various programming languages
- Use sum types to model problems in new ways
- Think beyond hierarchical program design

_Algebraic data types_ are any types that can be made by combining other
types. You can combine types either with an _and_ (a name is a `String` and
another `String`) or with an _or_ (a `Bool` is composed of a `True` data
constructor and a `False` data constructor).

## Product types

Types composed with an _and_ are called _product types_.

Even though it sounds complicated, all programming languages implement some
sort of product type. For example, in C they're called structs.

### Hierarchical design

The problem with product types is that you're constrained to a top-down
design, starting with the most abstract representation of a type you can
imagine.

Let's say you sell books and a Book looks like this:

```java
public class Book {
    Author author;
    String isbn;
    String title;
    int  yearPublished;
    double price;
}
```

then one day you want to sell vinyl records:

```java
public class VinylRecord {
    String artist;
    String title;
    int  yearPublished;
    double price;
}
```

Now if you want to search for elements you have to come up with an
abstraction that defines both books and vinyl records:

```java
public class StoreItem {
    String title;
    int  yearPublished;
    double price;
}

public class Book extends StoreItem{
    Author author;
    String isbn;
}

public class VinylRecord extends StoreItem{
    String artist;
}
```

We can see that unfortunately `author` and `artist` are different and
therefore cannot be extracted.

But what if one day you want to sell toys?

```java
public class CollectibleToy {
    String name;
    String description;
    double price;
}
```

Now we would have to change `StoreItem` to only include the price.

☹️

## Sum types

Types composed with an _or_ are called _sum types_.

For example, we can define different types of names

```haskell
type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
   | NameWithMiddle FirstName MiddleName LastName
```

Having access to this _or_ operator dramatically changes how we write
software!

Let's see again the previous example where we had authors and artists.

```haskell
data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name
data Artist = Person Name | Band String
```

What about pesky names like H.P. Lovecraft?

```haskell
data Name = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
```

With that we can write this:

```haskell
hpLovecraft :: Creator
hpLovecraft = AuthorCreator
               (Author
                 (TwoInitialsWithLast 'H' 'P' "Lovecraft"))
```

Instead let's imagine how the structure would look in a language without
sum types:

```java
public class Name {
    String firstName;
    String lastName;
    String middleName;
    char firstInitial;
    char middleInitial;
    char lastInitial;
}
```

Let's see how `Book` and `VinylRecord` would look in Haskell

```haskell
data Book = Book {
      author    :: Creator
    , isbn      :: String
    , bookTitle :: String
    , bookYear  :: Int
    , bookPrice :: Double
    }

data VinylRecord = VinylRecord {
      artist
    , recordTitle
    , recordYear
    , recordPrice
    }

data StoreItem = BookItem Book | RecordItem VinylRecord
```

Now adding a collectible toy is super easy

```
data CollectibleToy = CollectibleToy {
      name :: String
    , descrption :: String
    , toyPrice :: Double
    }

data StoreItem = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
```

and we can define our `price` function that calculates the price of an item

```haskell
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
```

## Summary

In this lesson, our objective was to teach you about the two ways to create
types from existing types.

The first way is with product types. Product types work by combining types
using and, bundling two or more types together to define a new type. Nearly
every programming language supports product types, even if not by that
name.

The other way to combine types is with or. Sum types are much less common
than product types. The problem with product types alone is that you’re
forced to think in hierarchical abstractions. Sum types are a powerful
tool that allows you to be much more expressive in defining new types.
