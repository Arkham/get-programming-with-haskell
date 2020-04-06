module HINQ where

import Control.Applicative (Alternative, (<|>))
import Control.Monad (guard)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Name =
  Name
    { firstName :: String
    , lastName :: String
    }
  deriving (Eq, Ord)

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel
  = Freshman
  | Sophomore
  | Junior
  | Senior
  deriving (Eq, Ord, Enum, Show)

data Student =
  Student
    { studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
    }
  deriving (Show, Eq, Ord)

students :: [Student]
students =
  [ Student 1 Senior (Name "Audre" "Lorde")
  , Student 2 Junior (Name "Leslie" "Silko")
  , Student 3 Freshman (Name "Judith" "Butler")
  , Student 4 Senior (Name "Guy" "Debord")
  , Student 5 Sophomore (Name "Jean" "Baudrillard")
  , Student 1 Junior (Name "Julia" "Kristeva")
  ]

{-|
  Now we want to build some basic operations like select and where.

  Select should have a type signature like `(a -> b) -> [a] -> [b]`,

  Where should have one like `(a -> Bool) -> [a] -> [a]`.

  What about join? It's bit more complicated:
  `Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]`
-}
_select :: Monad m => (a -> b) -> m a -> m b
_select prop values = prop <$> values

{-|
  Some examples:

  *HINQ> _select (firstName . studentName) students
  ["Audre","Leslie","Judith","Guy","Jean","Julia"]
  *HINQ> _select (lastName . studentName) students
  ["Lorde","Silko","Butler","Debord","Baudrillard","Kristeva"]
  *HINQ> _select gradeLevel students
  [Senior,Junior,Freshman,Senior,Sophomore,Junior]
-}
_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where cond values = do
  value <- values
  guard (cond value)
  return value

{-|
  *HINQ> _select studentName (_where (\x -> gradeLevel x == Junior) students)
  [Leslie Silko,Julia Kristeva]
-}
startsWith :: Char -> String -> Bool
startsWith char string = char == head string

{-|
  *HINQ> _where (startsWith 'J' . firstName) (_select studentName students)
  [Judith Butler,Jean Baudrillard,Julia Kristeva]
-}
data Teacher =
  Teacher
    { teacherId :: Int
    , teacherName :: Name
    }
  deriving (Show)

teachers :: [Teacher]
teachers =
  [ Teacher 100 (Name "Simone" "De Beauvoir")
  , Teacher 200 (Name "Susan" "Sontag")
  ]

data Course =
  Course
    { courseId :: Int
    , courseTitle :: String
    , teacher :: Int
    }
  deriving (Show)

courses :: [Course]
courses = [Course 101 "French" 100, Course 201 "English" 200]

_join ::
     (Monad m, Alternative m)
  => Eq c =>
       m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join xs ys xFun yFun = do
  x <- xs
  y <- ys
  guard (xFun x == yFun y)
  return (x, y)

{-|
  *HINQ> _join teachers courses teacherId teacher
  [(Teacher {teacherId = 100, teacherName = Simone De Beauvoir},Course {courseId = 101, courseTitle = "French", teacher = 100}),(Teacher {teacherId = 200, teacherName = Susan Sontag},Course {courseId = 201, courseTitle = "English", teacher = 200})]

  *HINQ> joinData = (_join teachers courses teacherId teacher)
  *HINQ> whereResult = _where ((== "English") . courseTitle . snd) joinData
  *HINQ> selectResult = _select (teacherName . fst) whereResult
  *HINQ> selectResult
  [Susan Sontag]

  But this doesn't look like the normal way you would write a SQL query

  SELECT teacherName from teachers 
  JOIN courses ON teachers.teacherId == courses.teacher
  WHERE courseTitle == 'English'

  So let's write a helper function to do that
-}
_hinq selectQuery joinQuery whereQuery = (selectQuery . whereQuery) joinQuery

finalResult :: [Name]
finalResult =
  _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

{-|
  There is one annoyance left. If you didn't want to pass a where clause,
  you'd still have to pass something. So we could write:
-}
withoutWhere :: [Name]
withoutWhere =
  _hinq
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where (const True))

{-|
  This is ok, but not very nice to have to remember to pass that option.

  So let's make a type that describes that we can have queries with:
  - select, where and join (or plain data)
  - select and join (or plain data)
-}
data HINQ m a b
  = HINQ (m a -> m b) (m a) (m a -> m a)
  | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (const True))

{-|
  Let's take it for a spin now!
-}
query1 :: HINQ [] (Teacher, Course) Name
query1 =
  HINQ
    (_select (teacherName . fst))
    (_join teachers courses teacherId teacher)
    (_where ((== "English") . courseTitle . snd))

query2 :: HINQ [] Teacher Name
query2 = HINQ_ (_select teacherName) teachers

{-|
  It's easy to imagine that could end up with a `Maybe Teacher` or a
  `Maybe Course`
-}
possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 =
  HINQ
    (_select (teacherName . fst))
    (_join possibleTeacher possibleCourse teacherId teacher)
    (_where ((== "French") . courseTitle . snd))

{-|
  Now let's look at getting course enrollment information.
-}
data Enrollment =
  Enrollment
    { student :: Int
    , course :: Int
    }
  deriving (Show)

enrollments :: [Enrollment]
enrollments =
  [ Enrollment 1 101
  , Enrollment 2 101
  , Enrollment 2 201
  , Enrollment 3 101
  , Enrollment 4 201
  , Enrollment 4 101
  , Enrollment 5 101
  , Enrollment 6 201
  ]

studentEnrollmentsQuery :: HINQ [] (Student, Enrollment) (Name, Int)
studentEnrollmentsQuery =
  HINQ_
    (_select (\(st, en) -> (studentName st, course en)))
    (_join students enrollments studentId student)

studentEnrollments :: [(Name, Int)]
studentEnrollments = runHINQ studentEnrollmentsQuery

englishStudentsQuery :: HINQ [] ((Name, Int), Course) Name
englishStudentsQuery =
  HINQ
    (_select (fst . fst))
    (_join studentEnrollments courses snd courseId)
    (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQuery

getEnrollments :: String -> [Name]
getEnrollments courseName =
  runHINQ
    (HINQ
       (_select (fst . fst))
       (_join studentEnrollments courses snd courseId)
       (_where ((== courseName) . courseTitle . snd)))

{-|
  There you go!

  With the power of monads we have built a query engine that's reasonably
  similar to SQL and LINQ. Not only the queries are easier to read, but you
  also get lazy evaluation and a powerful type system. Furthermore, for any
  new type you have, if you implement `Monad` and `Alternative`, it works!

  Let's implement Semigroup and Monoid for HINQ.
-}
combineHINQ ::
     (Monad m, Alternative m) => HINQ m a b -> HINQ m a b -> HINQ m a b
combineHINQ (HINQ s1 j1 w1) (HINQ s2 j2 w2) =
  let s values = s1 values <|> s2 values
      j = j1 <|> j2
      w values = w1 values <|> w2 values
   in HINQ s j w
combineHINQ (HINQ_ s1 j1) (HINQ_ s2 j2) =
  combineHINQ
    (HINQ s1 j1 (_where (const True)))
    (HINQ s2 j2 (_where (const True)))

instance (Monad m, Alternative m) => Semigroup (HINQ m a b) where
  (<>) = combineHINQ

studentMap :: Map Int Student
studentMap = Map.fromList (map (\s -> (studentId s, s)) students)
