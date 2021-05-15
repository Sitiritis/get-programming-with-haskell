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

-- insertSnippet :: Maybe Html -> IO ()
-- insertSnippet maybeHtml = _

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

getPartStringByInput :: String -> Maybe String
getPartStringByInput input = show <$> maybePart
  where maybePart = getPartByInput input

processInput :: String -> IO Bool
processInput input
  | input `elem` exitCommands = do 
      putStrLn "Goodbye!"
      return False
  | otherwise = do
      case getPartStringByInput input of
        (Just partString) -> putStrLn partString
        _ -> putStrLn $ "Part with such id not found, try entering 1, 2 or 3.\n" <>
                        "Type exit, quit, e or q to quit the program"
      return True
  where exitCommands = ["exit", "quit", "e", "q"]

main :: IO ()
main = do
  putStr "λ "
  input <- getLine
  evaluationResult <- processInput input
  if evaluationResult then
    main
  else
    return ()
