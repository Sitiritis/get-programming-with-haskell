module Hinq where

import Control.Monad ( MonadPlus, mplus, mzero, guard )
import Control.Applicative ( Alternative, (<|>), empty )


data Name = Name {
    firstName :: String,
    lastName :: String
  }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

data GradeLevel = Freshman
                | Sophmore
                | Junior
                | Senior deriving (Eq, Ord, Enum, Show)

data Student = Student {
    studentId :: Int,
    gradeLevel :: GradeLevel,
    studentName :: Name
  } deriving Show

data Teacher = Teacher {
    teacherId :: Int,
    teacherName :: Name
  } deriving Show

data Course = Course {
    courseId :: Int,
    courseTitle :: String,
    teacher :: Int
  } deriving Show

data Enrollment = Enrollment {
    student :: Int,
    course :: Int
  } deriving Show


students :: [Student]
students = [
    Student 1 Senior (Name "Audre" "Lorde"),
    Student 2 Junior (Name "Leslie" "Silko"),
    Student 3 Freshman (Name "Judith" "Butler"),
    Student 4 Senior (Name "Guy" "Debord"),
    Student 5 Sophmore (Name "Jean" "Baudrillard"),
    Student 6 Junior (Name "Julia" "Kristeva")
  ]

teachers :: [Teacher]
teachers = [
    Teacher 100 (Name "Simone" "De Beauvior"),
    Teacher 200 (Name "Susan" "Sontag")
  ]

courses :: [Course]
courses = [
    Course 101 "French" 100,
    Course 201 "English" 200
  ]

enrollments :: [Enrollment]
enrollments = [
    Enrollment 1 101,
    Enrollment 2 101,
    Enrollment 2 201,
    Enrollment 3 101,
    Enrollment 4 201,
    Enrollment 4 101,
    Enrollment 5 101,
    Enrollment 6 201
  ]


_selectList :: (a -> b) -> [a] -> [b]
_selectList s vals = do
  v <- vals
  return $ s v

{-

>>> _selectList (firstName . studentName) students
>>> _selectList gradeLevel students
["Audre","Leslie","Judith","Guy","Jean","Julia"]
[Senior,Junior,Freshman,Senior,Sophmore,Junior]

>>> _selectList (\s -> (studentName s, gradeLevel s)) students
[(Audre Lorde,Senior),(Leslie Silko,Junior),(Judith Butler,Freshman),(Guy Debord,Senior),(Jean Baudrillard,Sophmore),(Julia Kristeva,Junior)]

-}

_whereList :: (a -> Bool) -> [a] -> [a]
_whereList test vals = do
  v <- vals
  guard $ test v
  return v

startsWith :: Char -> String -> Bool
startsWith c [] = False
startsWith c (s1:_) = c == s1

{-

>>> _whereList (startsWith 'J' . firstName) (_selectList studentName students)
[Judith Butler,Jean Baudrillard,Julia Kristeva]

-}

_joinList :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_joinList la lb sa sb = do
  a <- la
  b <- lb
  guard $ sa a == sb b
  return (a, b)

{-

>>> _joinList teachers courses teacherId teacher
[(Teacher {teacherId = 100, teacherName = Simone De Beauvior},Course {courseId = 101, courseTitle = "French", teacher = 100}),(Teacher {teacherId = 200, teacherName = Susan Sontag},Course {courseId = 201, courseTitle = "English", teacher = 200})]

>>> let joinData = _joinList teachers courses teacherId teacher
>>> let whereResult = _whereList ((== "English") . courseTitle . snd) joinData
>>> let selectResult = _selectList (teacherName . fst) whereResult

-}

_hinq selectQuery joinQuery whereQuery =
  (\joinData ->
    (\whereResult ->
        selectQuery whereResult
    ) (whereQuery joinData)
  ) joinQuery

finalResult :: [Name]
finalResult = _hinq
  (_selectList (teacherName . fst))
  (_joinList teachers courses teacherId teacher)
  (_whereList ((== "English") . courseTitle . snd))

{-

>>> finalResult
[Susan Sontag]

-}

teacherFirstName :: [String]
teacherFirstName = _hinq
  (_selectList firstName)
  finalResult
  (_whereList (\_ -> True))

{-

>>> teacherFirstName
["Susan"]

-}

_select :: (Monad m) => (a -> b) -> m a -> m b
_select = fmap

_where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where f ma = do
  a <- ma
  guard $ f a
  return a

_join :: (Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a, b)
_join ma mb sa sb = do
  a <- ma
  b <- mb
  guard $ sa a == sb b
  return (a, b)

data HINQ m a b = HINQEmpty
                | HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

instance Alternative m => Semigroup (HINQ m a b) where
  HINQ sClauseL jClauseL wClauseL <> HINQ sClauseR jClauseR wClauseR = HINQ sClauseR (jClauseL <|> jClauseR) (wClauseL . wClauseR)
  HINQ_ sClauseL jClauseL         <> HINQ_ sClauseR jClauseR         = HINQ_ sClauseR (jClauseL <|> jClauseR)
  HINQ sClauseL jClauseL wClauseL <> HINQ_ sClauseR jClauseR         = HINQ sClauseR (jClauseL <|> jClauseR) wClauseL
  HINQ_ sClauseL jClauseL         <> HINQ sClauseR jClauseR wClauseR = HINQ sClauseR (jClauseL <|> jClauseR) wClauseR
  HINQEmpty                       <> q                               = q
  q                               <> HINQEmpty                       = q


instance Alternative m => Monoid (HINQ m a b) where
  mempty = HINQEmpty


runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))
runHINQ HINQEmpty = empty


query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ
  (_select (teacherName . fst))
  (_join teachers courses teacherId teacher)
  (_where ((== "English") . courseTitle . snd))

{-

>>> runHINQ query1
[Susan Sontag]

-}

query2 :: HINQ [] Teacher Name
query2 = HINQ_
  (_select teacherName)
  teachers

{-

>>> runHINQ query2
[Simone De Beauvior,Susan Sontag]

-}

possibleTeacher :: Maybe Teacher
possibleTeacher = Just (head teachers)

possibleCourse :: Maybe Course
possibleCourse = Just (head courses)

maybeQuery1 :: HINQ Maybe (Teacher, Course) Name
maybeQuery1 = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher possibleCourse teacherId teacher)
  (_where ((== "French") . courseTitle . snd))

{-

>>> runHINQ maybeQuery1
Just Simone De Beauvior

-}

missingCourse :: Maybe Course
missingCourse = Nothing 

maybeQuery2 :: HINQ Maybe (Teacher, Course) Name
maybeQuery2 = HINQ
  (_select (teacherName . fst))
  (_join possibleTeacher missingCourse teacherId teacher)
  (_where ((== "French") . courseTitle . snd))

{-

>>> runHINQ maybeQuery2
Nothing

-}

-- studentsEnrollmentQ :: HINQ [] (Student, Enrollment) (Name, Int)
studentsEnrollmentQ = HINQ_
  (_select (\(st, en) -> (studentName st, course en)))
  (_join students enrollments studentId student)

studentsEnrollment :: [(Name, Int)]
studentsEnrollment = runHINQ studentsEnrollmentQ

{-

>>> studentsEnrollment
[(Audre Lorde,101),(Leslie Silko,101),(Leslie Silko,201),(Judith Butler,101),(Guy Debord,201),(Guy Debord,101),(Jean Baudrillard,101),(Julia Kristeva,201)]

-}

englishStudentsQ = HINQ
  (_select (fst . fst))
  (_join studentsEnrollment courses
         snd                courseId)
  (_where ((== "English") . courseTitle . snd))

englishStudents :: [Name]
englishStudents = runHINQ englishStudentsQ

{-

>>> englishStudents
[Leslie Silko,Guy Debord,Julia Kristeva]

-}

getEnrollments :: String -> [Name]
getEnrollments courseName = runHINQ courseQuery
  where courseQuery = HINQ (_select (fst . fst))
                           (_join studentsEnrollment courses
                                  snd                courseId)
                           (_where ((== courseName) . courseTitle . snd))

{-

>>> getEnrollments "English"
>>> getEnrollments "French"
>>> getEnrollments "Haskell"
[Leslie Silko,Guy Debord,Julia Kristeva]
[Audre Lorde,Leslie Silko,Judith Butler,Guy Debord,Jean Baudrillard]
[]

-}

emptyQ :: HINQ [] Int Int
emptyQ = HINQEmpty <> HINQEmpty 

{-

>>> runHINQ emptyQ
[]

-}
