data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show, Enum, Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN s l = toEnum shiftedIntRepr
  where intRepr = fromEnum l
        shiftAmount = s `div` 2
        shiftedIntRepr = (intRepr + shiftAmount) `mod` s

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
  where halfN = n `div` 2
        offset = fromEnum c + halfN +
                 if even halfN
                 then 0
                 else 1
        rotation = offset `mod` n

--universalEncoder :: (Bounded a, Enum a) => [a] -> [a]
--universalEncoder = map rotHalf
--  where alphabetSize = 1 + fromEnum (maxBound :: a)
--        rotHalf = rotN alphabetSize

largestCharNumber :: Int
largestCharNumber = fromEnum (maxBound :: Char)

rotChar :: Char -> Char
rotChar = rotN numChars
  where numChars = 1 + largestCharNumber

message :: [FourLetterAlphabet]
message = [L1, L3, L4, L1, L1, L2]

fourLetterAlphabetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetterAlphabetEncoder = map rot4l
  where alphaSize = 1 + fromEnum (maxBound :: FourLetterAlphabet)
        rot4l = rotN alphaSize

rotEncoder :: String -> String
rotEncoder = map rotChar
  where numChars = 1 + fromEnum (maxBound :: Char)
        rotChar = rotN numChars

rotDecoder :: String -> String
rotDecoder = map rotCharDecoder
  where numChars = 1 + fromEnum (maxBound :: Char)
        rotCharDecoder = rotNdecoder numChars

-- XOR
xorBool :: Bool -> Bool -> Bool
xorBool b1 b2 = (b1 || b2) && not (b1 && b2)

xorPair :: (Bool, Bool) -> Bool
xorPair (b1, b2) = xorBool b1 b2

xor :: [Bool] -> [Bool] -> [Bool]
xor l1 l2 = map xorPair (zip l1 l2)

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if remainer == 0
               then False : intToBits' nextVal
               else True : intToBits' nextVal
  where remainer = n `mod` 2
        nextVal = n `div` 2

intToBits :: Int -> Bits
intToBits n = leadingZeroes ++ reversedBits
  where reversedBits = reverse (intToBits' n)
        maxBits = length (intToBits' (maxBound :: Int))
        missingBits = maxBits - length reversedBits
        leadingZeroes = replicate missingBits False

charToBits :: Char -> Bits
charToBits = intToBits . fromEnum

stringToBits :: String -> [Bits]
stringToBits = map charToBits

bitsToInts :: Bits -> Int
bitsToInts bits = sum nonZeroPowersOf2
  where size = length bits
        indices = [size - 1, size - 2 .. 0]
        bitsIndices = zip indices bits
        contributingIndices = filter (\indexBit -> snd indexBit == True) bitsIndices
        nonZeroPowersOf2 = map ((2^) . fst) contributingIndices

bitsToChar :: Bits -> Char
bitsToChar = toEnum . bitsToInts

bitsToString :: [Bits] -> String
bitsToString = map bitsToChar

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext = zipWith xor padBits plaintextBits
  where padBits = stringToBits pad
        plaintextBits = stringToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar encodedBits
  where encodedBits = applyOTP' pad plaintext

class Cipher a where
  encode :: a -> String -> String
  decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
  encode Rot = rotEncoder
  decode Rot = rotDecoder

newtype OneTimePad = OTP String

instance Cipher OneTimePad where
  encode (OTP key) = applyOTP key
  decode = encode

myOTP :: OneTimePad
myOTP = OTP (cycle [minBound .. maxBound])

prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG = prng 1337 7 100

randomString :: (Int -> Int) -> Int -> String
randomString rng seed = toEnum currentRandomNumber : randomString rng currentRandomNumber
  where currentRandomNumber = rng seed

data StreamCipher = StreamCipher { rng :: Int -> Int
                                 , seed :: Int
                                 }
instance Cipher StreamCipher where
  encode (StreamCipher rng seed) = applyOTP (randomString rng seed)
  decode = encode

sampleStreamCipher :: StreamCipher
sampleStreamCipher = StreamCipher examplePRNG 10
