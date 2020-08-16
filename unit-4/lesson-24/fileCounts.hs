{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Data.Maybe

import System.Environment

import TextShow
import Safe

data TextFileStats =
  TextFileStats {
                  numCharacters :: Int
                , numWords      :: Int
                , numLines      :: Int
                }

getCounts :: T.Text -> TextFileStats
getCounts input = TextFileStats charCount wordCount lineCount
  where charCount = (fromIntegral . T.length) input
        wordCount = (length . T.words) input
        lineCount = (length . T.lines) input

instance TextShow TextFileStats where
  showb (TextFileStats nc nw nl) = mconcat [
      "chars: ",
      showb nc,
      " words: ",
      showb nw,
      " lines: ",
      showb nl
    ]

instance Show TextFileStats where
  show = toString . showb

getFileStats :: FilePath -> IO TextFileStats
getFileStats path = do
  input <- TIO.readFile path
  return $ getCounts input

appendFileStatsToResultFile :: FilePath -> IO ()
appendFileStatsToResultFile path = do
  stats <- getFileStats path
  let output = mconcat [
          T.pack path,
          " ",
          showtl stats,
          "\n"
        ]
  TIO.putStrLn $ showtl stats
  TIO.appendFile "stats.dat" output

main :: IO ()
main = do
  args <- getArgs

  let maybeFileName = headMay args
  let maybeWriteStatsEffect = appendFileStatsToResultFile <$> maybeFileName

  fromMaybe (TIO.putStrLn "The first argument must be a file name for which stats will be calculated.") maybeWriteStatsEffect
