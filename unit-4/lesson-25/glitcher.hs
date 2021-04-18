{-# LANGUAGE OverloadedStrings #-}

import System.Environment ( getArgs )
import System.Random ( randomRIO )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Monad ( foldM )
import Safe ( headMay )

-- Tools

intToChar :: Int -> Char
intToChar int = toEnum $ int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

randomChar :: IO Char
randomChar = toEnum <$> randomRIO (0, 255)

-- Glitch functions

-- Can add a byte at the end of the bytes sequence, when
-- loc is >= BC.length bytes
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc replacingChar bytes = mconcat [before, newChar, after]
  where (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC replacingChar

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  randomLocation <- randomRIO (1, bytesLength)
  randChar <- randomRIO (0, 255)
  return (replaceByte randomLocation randChar bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let size = 25
  let bytesLength = BC.length bytes
  randomStart <- randomRIO (0, bytesLength)
  return $ sortSection randomStart size bytes

-- Q25.2 Add another glitching technique, randomReverseBytes, that randomly
-- reverses a section of bytes in your data.

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString 
reverseSection start size bytes = mconcat [before, changed, after]
  where (before, rest) = BC.splitAt start bytes
        (target, after) = BC.splitAt size rest
        changed = BC.reverse target

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
  let size = 25
  let bytesLength = BC.length bytes
  randomStart <- randomRIO (0, bytesLength)
  return $ reverseSection randomStart size bytes

-- Main

mainFactory :: (BC.ByteString -> IO BC.ByteString) -> IO ()
mainFactory glitcher = do
  args <- getArgs
  case headMay args of
    Nothing -> print "The first argument must be file name which will be glitched."
    Just fileName -> do
      fileContent <- BC.readFile fileName
      glitchedContent <- glitcher fileContent
      let glitchedFileName = "glitched_" ++ fileName
      BC.writeFile glitchedFileName glitchedContent
      print $ fileName ++ " successfully glitched!"

-- main :: IO ()
-- main = mainFactory randomReplaceByte

-- main :: IO ()
-- main = mainFactory randomSortSection

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [
    randomReplaceByte,
    randomSortSection,
    randomReverseSection,
    randomReplaceByte,
    randomSortSection,
    randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  case headMay args of
    Nothing -> print "The first argument must be file name which will be glitched."
    Just fileName -> do
      fileContent <- BC.readFile fileName
      glitchedContent <- foldM
        (\bytes transformation -> transformation bytes)
        fileContent
        glitchActions
      let glitchedFileName = "glitched_" ++ fileName
      BC.writeFile glitchedFileName glitchedContent
      print $ fileName ++ " successfully glitched!"
