myGCDIf a b =
  if remainder == 0
  then b
  else myGCDIf b remainder
  where remainder = a `mod` b

myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

sayAmountCase n = case n of
  1 -> "one"
  2 -> "two"
  n -> "a bunch"

sayAmount 1 = "one"
sayAmount 2 = "two"
sayAmount n = "a bunch"

isEmpty [] = True
isEmpty _  = False

myHead (x:_) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = []
