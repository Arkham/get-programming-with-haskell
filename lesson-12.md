# Creating your own types

- Define type synonyms to clarify code
- Create your own data type
- Build types from other types
- Work with complex types by using record syntax

## Using type synonyms

```haskell
type FirstName = String
type LastName = String
type Age = Int
type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo firstName lastName age height =
  "Do something"
```

We could also define a synonym for the patient's name

```haskell
type PatientName = (String, String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient
```

## Creating new types

If we want to describe the sex of our patient

```haskell
data Sex = Male | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'
```

Or their blood type

```haskell
data RhType = Pos | Neg

data ABOType = A | B | AB | O

data BloodType = BloodType ABOType RhType
```

Or what if a patient has a middle name?

```haskell
type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMIddle FirstName MiddleName LastName
```

## Records

We could encapsulate all information about a patient in a record

```haskell
data Patient = Patient { name :: Name
                       , sec :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }
```

And here's how you would use it:

```haskell
ju = Patient { name = "Ju Ju"
             , age = 23
             , sex = Male
             , height = 54
             , weight = 100
             , bloodType = BloodType O Pos }

updatedJu = ju { height = 45 }
```
