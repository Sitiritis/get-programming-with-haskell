module Robot where

import qualified Data.Map as Map
import Text.Read (readMaybe)


data RobotPart = RobotPart {
    name        :: String,
    description :: String,
    cost        :: Double,
    count       :: Int
  } deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
    name        = "left arm",
    description = "left arm for face punching",
    cost        = 1000.00,
    count       = 3
  }

rightArm :: RobotPart
rightArm = RobotPart {
    name        = "right arm",
    description = "right arm for kind hand gestures",
    cost        = 1025.00,
    count       = 5
  }

robotHead :: RobotPart
robotHead = RobotPart {
    name        = "robot head",
    description = "this head looks mad",
    cost        = 5092.25,
    count       = 2
  }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
    "<h2>", partName, "</h2>",
    "<p><h3>desc</h3>", partDesc,
    "</p><p><h3>cost</h3>", partCost,
    "</p><p><h3>count</h3>", partCount, "</p>"
  ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList [
    (1, leftArm),
    (2, rightArm),
    (3, robotHead)
  ]

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = map renderHtml allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml<$> leftArmIO


getPartByInput :: String -> Maybe RobotPart
getPartByInput input = do
    maybeInt <- readMaybe input :: Maybe Int
    Map.lookup maybeInt partsDB

cheapestPart :: RobotPart -> RobotPart -> RobotPart
cheapestPart part1 part2
  | cost part1 <= cost part2 = part1
  | otherwise = part2

printMaybePart :: Maybe RobotPart -> IO ()
printMaybePart Nothing = print "Robot part with such id not found"
printMaybePart (Just part) = print part


main :: IO ()
main = do
  putStrLn "Enter 2 IDs of parts (separated by new lines):"
  part1Id <- getLine
  part2Id <- getLine
  let part = cheapestPart <$> getPartByInput part1Id <*> getPartByInput part2Id
  printMaybePart part
