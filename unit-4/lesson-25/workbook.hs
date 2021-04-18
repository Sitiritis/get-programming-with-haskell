{-# LANGUAGE OverloadedStrings #-}

import Text.Read ( readMaybe )

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


tatsuhikoTakimoto :: T.Text
tatsuhikoTakimoto = "滝本 竜彦"


sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- sampleString :: String
-- sampleString = B.unpack sampleBytes

{-|

>>> B.unpack sampleBytes :: String
Couldn't match type ‘Word8’ with ‘Char’
Expected type: String
  Actual type: [Word8]

>>> :type B.unpack
B.unpack :: ByteString -> [Word8]

>>> :type BC.unpack
BC.unpack :: ByteString -> [Char]

|-}

{-|

>>> let bcInt = "4567" :: BC.ByteString

>>> :type BC.unpack bcInt
BC.unpack bcInt :: [Char]

>>> readMaybe $ BC.unpack bcInt :: Maybe Integer
Just 4567

|-}

bcToInt :: BC.ByteString -> Maybe Integer
bcToInt = readMaybe . BC.unpack


nagarjunaBC :: BC.ByteString
nagarjunaBC = "नागर्जुनॅ"

nagarjunaText :: T.Text
nagarjunaText = "नागर्जुनॅ"

nagarjunaB :: B.ByteString
nagarjunaB = BC.pack $ T.unpack nagarjunaText

nagarjunaSafe :: B.ByteString
nagarjunaSafe = E.encodeUtf8 nagarjunaText
