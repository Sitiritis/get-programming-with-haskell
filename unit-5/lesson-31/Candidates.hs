module Candidates where

import qualified Data.Map as Map


data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate {
    candidateId :: Int,
    codeReview :: Grade,
    cultureFit :: Grade,
    education :: Degree
  } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    passedCoding = codeReview candidate > B
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS
    tests = [
        passedCoding,
        passedCultureFit,
        educationMin
      ]

{-

>>> let candidateZero = Candidate 0 A B PhD
>>> viable candidateZero
True

-}

readFromCommandLine :: (Read a) => IO a
readFromCommandLine = read <$> getLine

readInt :: IO Int
readInt = readFromCommandLine

readGrade :: IO Grade
readGrade = readFromCommandLine

readGradeDo :: IO Grade
readGradeDo = do
  gradeStr <- getLine
  return $ read gradeStr

readDegree :: IO Degree
readDegree = readFromCommandLine

readCandidate :: IO Candidate
readCandidate = do
  putStrLn "Enter id:"
  cId <- readInt
  putStrLn "Enter code grade:"
  codeGrade <- readGrade
  putStrLn "Enter culture fit grade:"
  cultureGrade <- readGrade
  putStrLn "Enter education:"
  degree <- readDegree
  return $ Candidate {
      candidateId = cId,
      codeReview = codeGrade,
      cultureFit = cultureGrade,
      education = degree
    }

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

candidate1 :: Candidate
candidate1 = Candidate {
    candidateId = 1,
    codeReview = A,
    cultureFit = A,
    education = BA
  }

candidate2 :: Candidate
candidate2 = Candidate {
    candidateId = 2,
    codeReview = C,
    cultureFit = A,
    education = PhD
  }

candidate3 :: Candidate
candidate3 = Candidate {
    candidateId = 3,
    codeReview = A,
    cultureFit = B,
    education = MS
  }

candidatesDB :: Map.Map Int Candidate
candidatesDB = Map.fromList [
    (candidateId candidate1, candidate1),
    (candidateId candidate2, candidate2),
    (candidateId candidate3, candidate3)
  ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidatesDB
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

failPassOrElse :: Maybe String -> String
failPassOrElse Nothing = "Error id not found"
failPassOrElse (Just val) = val

candidates :: [Candidate]
candidates = [
    candidate1,
    candidate2,
    candidate3
  ]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

assessCandidate :: (Monad m) => m Candidate -> m String
assessCandidate mCandidate = do
  candidate <- mCandidate
  let passed = viable candidate
  let statement = if passed
                  then "passed"
                  else "failed"
  return statement

{-

>>> assessCandidate $ Map.lookup 1 candidatesDB
>>> assessCandidate $ Map.lookup 2 candidatesDB
>>> assessCandidate $ Map.lookup 3 candidatesDB
>>> assessCandidate $ Map.lookup 4 candidatesDB
Just "failed"
Just "failed"
Just "passed"
Nothing

>>> assessCandidate candidates
["failed","failed","passed"]

-}
