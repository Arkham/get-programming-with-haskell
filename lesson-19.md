# Maybes

In this lesson you'll:

- understand the `Maybe` type
- use the `Maybe` type to handle missing values
- design programs with `Maybe` types

## Maybes: solving missing values with types

Let's take the code from Lesson 18:

```
import qualified Data.Map as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs
```

When we decided to use `Map.lookup`, we bumped into a `Maybe` value, here's
the definition

```
data Maybe a = Nothing | Just a
```

## The problem with null

In most programming languages when you ask for a map to fetch a value which
isn't there, there are two general responses:

- handle missing values with errors
  - the programmer has to remember to catch the error
  - the error must be handled at the time the exception is thrown
- return null values
  - the programmer has to remember to check for null values wherever that
    value is being used
  - there is no way for the program to force the programmer to remember to
    check

Instead, the advantages of using a `Maybe` are:

- it makes impossible to forget that a value might be null
- the programmer never has to worry about this until absolutely necessary

So let's say we are the assistant of the aforementioned mad scientist:
periodically you need to do inventory, so you must go through all drawers
and lookup every ID in the range of 1 to 50.

```
possibleDrawers :: [Int]
possibleDrawers = [1..50]
```

Then you need a function to get the contents of each drawer:

```
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where getContents = \id -> Map.lookup id catalog
```

Notice that the return value is a list of `Maybe Organ`. If now we wanted
to get a count of a particular organ, we would have to deal with the
`Maybe`

```
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available =
  length (filter (\x -> x == Just organ) available)
```

Which we would use like this

```
*Organ> availableOrgans = getDrawerContents possibleDrawers organCatalog
*Organ> countOrgan Brain availableOrgans
1
*Organ> countOrgan Heart availableOrgans
2
```

## Computing with Maybe

Since we derived `Show` we can print our organs to the console, but they
look a bit funky

```
*Organ> availableOrgans
[Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Just Heart,Nothing,Nothing,Nothing,Nothing,Nothing,Just Brain,Just Spleen,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just Spleen,Nothing,Nothing,Just Kidney,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
```

Instead we could filter out all `Nothing` values and only show the
available organs:

```
justTheOrgans :: [Maybe Organ] -> [Organ]
justTheOrgans = Data.Maybe.catMaybes

*Organ> justTheOrgans availableOrgans
[Heart,Heart,Brain,Spleen,Spleen,Kidney]
```

## More complext computation with Maybe

Let's say the mad scientist gives us a new project.

- You'll be given a drawer ID
- You will retrieve an item from the drawer
- Then you'll put the organ in the appropriate container (a vat, a cooler,
  a bag)
- Finally, you'll put the container in the correct location

Here are the rules for containers:

- brains go in a vat
- hearts go in a cooler
- spleens and kidneys go in a bag

For locations:

- vats and coolers to to the lab
- bags go to the kitchen

Here's what the code looks like:

```
data Container
  = Vat Organ
  | Cooler Organ
  | Bag Organ

instance Show Container where
  show (Vat organ) = show organ ++ " in a vat"
  show (Cooler organ) = show organ ++ " in a cooler"
  show (Bag organ) = show organ ++ " in a bag"

data Location
  = Lab
  | Kitchen
  | Bathroom
  deriving (Show)

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)
```

Then a `process` function will take an `Organ` and put it in the proper
container and location

```
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

report :: (Location, Container) -> String
report (location, container) = show container ++ " in the " ++ show location
```

And here is what is looks:

```
*Organ> process Brain
(Lab,Brain in a vat)
*Organ> process Heart
(Lab,Heart in a cooler)
*Organ> process Spleen
(Kitchen,Spleen in a bag)
*Organ> process Kidney
(Kitchen,Kidney in a bag)
*Organ> report (process Brain)
"Brain in a vat in the Lab"
```
