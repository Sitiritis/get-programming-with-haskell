myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x : myTake (n - 1) xs

myDrop 0 l = l
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n - 1) xs

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myCycle l = l ++ myCycle l

ackerman 0 n = n + 1
ackerman m 0 = ackerman (m - 1) 1
ackerman m n = ackerman (m - 1) (ackerman m (n - 1))

collatz 1 = 1
collatz n = 1 +
  if even n
  then collatz (n `div` 2)
  else collatz (3 * n + 1)

myReverseHelper [] s = s
myReverseHelper (x:xs) s = myReverseHelper xs (x : s)

myReverse l = myReverseHelper l []

myReverseDirect [] = []
myReverseDirect (x:xs) = myReverseDirect xs ++ [x]

fastFib _ b 0 = b
fastFib a b n = fastFib b (a + b) (n - 1)

fib = fastFib 0 1
