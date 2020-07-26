ifEven f x =
  if even x
  then f x
  else x

genIfXEven x = (\f -> ifEven f x)
ifEvenInc = ifEven (+ 1)
ifEvenDouble = ifEven (* 2)
ifEvenSquare = ifEven (^ 2)

getRequestURL host apiKey resource id =
  host ++
  "/" ++
  resource ++
  "/" ++
  id ++
  "?token=" ++
  apiKey

subtract2 = flip (-) 2

binaryPartialApplication f x = (\y -> f x y)
