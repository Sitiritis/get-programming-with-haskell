teams = ["red", "yellow", "orange", "blue", "purple"]

assignToGroups n l = groups `zip` l
  where groups = cycle [1..n]

simple x = x
longList = [1..]
stillLongList = simple longList

backwardsInfinity = reverse [1..]

respond phrase =
  if '!' `elem` phrase
  then "wow!"
  else "uh.. okay"

takeLast n aList = reverse (take n (reverse aList))

ones n = take n (cycle [1])

myRepeat v = cycle [v]

subseq s e l = take (e - s) (drop s l)

inFirstHalf x l = x `elem` take (length l `div` 2) l
