{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe


type Author = T.Text
type Title = T.Text

data Book = Book {
    author :: Author,
    title :: Title
  } deriving Show

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n",
    {-"Title: ", -} titleInTags,
    {-"Author: ", -} authorInTags,
  "</p>\n"]
  where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
    authorInTags = mconcat ["<em>", author book, "</em>\n"]

book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race",
    author = "Ligotti, Thomas"
  }

book2 :: Book
book2 = Book {
    title = "A Short History of Decay",
    author = "Cioran, Emil"
  }

book3 :: Book
book3 = Book {
    title = "The Tears of Eros",
    author = "Bataille, Georges"
  }

{-|

>>> bookToHtml book1
"<p>\nTitle: <strong>The Conspiracy Against the Human Race</strong>\nAuthor: <em>Ligotti, Thomas</em>\n</p>\n"

|-}

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n",
    "<head>\n",
      "<title>Books</title>\n",
      "<meta charset='utf-8' />\n",
    "</head>\n",
    "<body>",
      booksHtml,
    "</body>",
  "</html>\n"]
  where
    booksHtml = mconcat $ map bookToHtml books

myBooks :: [Book]
myBooks = [book1, book2, book3]

{-|

>>> booksToHtml myBooks
"<html>\n<head>\n<title>Books</title>\n<meta charset='utf-8' />\n</head>\n<body><p>\n<strong>The Conspiracy Against the Human Race</strong>\n<em>Ligotti, Thomas</em>\n</p>\n<p>\n<strong>A Short History of Decay</strong>\n<em>Cioran, Emil</em>\n</p>\n<p>\n<strong>The Tears of Eros</strong>\n<em>Bataille, Georges</em>\n</p>\n</body></html>\n"

|-}

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString
type FieldText = T.Text

data FieldMetadata = FieldMetadata {
    tag         :: T.Text, -- 3 characters
    fieldLength :: Int,    -- 4 characters
    fieldStart  :: Int     -- 5 characters, address relative to base address
  } deriving Show

leaderLength :: Int
leaderLength = 24

dirEntryLength :: Int
dirEntryLength = 12

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int -- Can throw an exception
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength = rawToInt . B.take leaderRecordSizeLength
  where leaderRecordSizeLength = 5

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream
  | marcStream == B.empty = []
  | otherwise = record : allRecords rest
  where (record, rest) = nextAndRest marcStream

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress = rawToInt . extractBaseAddress
  where baseAddressLength = 5
        baseAddressLeaderOffset = 12
        extractBaseAddress = B.take baseAddressLength . B.drop baseAddressLeaderOffset

getDirectoryLength :: MarcLeaderRaw -> Int
-- getDirectoryLength leader = baseAddress - (leaderLength + 1) -- looks like + 1 is incorrect
getDirectoryLength leader = baseAddress - leaderLength
  where baseAddress = getBaseAddress leader

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    leader = getLeader record
    directoryLength = getDirectoryLength leader
    afterLeader = B.drop leaderLength record

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory dir
  | dir == B.empty = []
  | otherwise = currentEntry : splitDirectory restEntries
  where (currentEntry, restEntries) = B.splitAt dirEntryLength dir

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata rawDirEntry = FieldMetadata tag fieldLength fieldAddress
  where
    tagSize = 3
    fieldLengthSize = 4
    -- fieldAddressSize = 5
    (tagRaw, dirEntryWithoutTag) = B.splitAt tagSize rawDirEntry
    tag = E.decodeUtf8 tagRaw
    (fieldLengthRaw, fieldAddressRaw) = B.splitAt fieldLengthSize dirEntryWithoutTag
    fieldLength = rawToInt fieldLengthRaw
    fieldAddress = rawToInt fieldAddressRaw

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    recordLength = getRecordLength record
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata targetTag record
  | null results = Nothing
  | otherwise = Just (head results)
  where
    metadata = getFieldMetadata $ splitDirectory $ getDirectory record
    results = filter ((== targetTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text 
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetadata) subfieldId record
  | null results = Nothing 
  | otherwise = Just (T.drop 1 $ head results)
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimiter) rawField
    results = filter ((== subfieldId) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue tag subfieldId record = lookupSubfield entryMetadata subfieldId record
  where entryMetadata = lookupFieldMetadata tag record

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {
    title = fromJust title,
    author = fromJust author
  }) justPairs
  where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords recordsToProcess = booksToHtml . pairsToBooks . take recordsToProcess . marcToPairs


main :: IO ()

-- main = TIO.writeFile "books.html" (booksToHtml myBooks)

-- main = do
--   marcData <- B.readFile "sample.mrc"
--   let marcRecords = allRecords marcData
--   print (length marcRecords)

main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 500 marcData
  TIO.writeFile "books.html" processed
