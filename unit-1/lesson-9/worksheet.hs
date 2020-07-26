import Data.Char

myRemove p [] = []
myRemove p (x : xs) =
  if p x
  then myRemove p xs
  else x : myRemove p xs

myProduct l = foldl (*) 1 l

concatAll l = foldl (++) "" l

myReverse l = foldl (\res x -> x : res) [] l

myFoldl f s [] = s
myFoldl f s (x : xs) = myFoldl f (s `f` x) xs

myFoldr f s [] = s
myFoldr f s (x : xs) = x `f` myFoldr f s xs

myElem x l = 0 < length (filter (x ==) l)

isPalindrome x = processed == reverse processed
  where processed = map toLower (filter (' ' /=) x)

harmonic n = sum (take n terms)
  where terms = map (1.0 /) [1.0, 2.0 ..]
