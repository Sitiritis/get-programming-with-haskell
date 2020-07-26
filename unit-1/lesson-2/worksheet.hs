x = 2
--x = 3 -- produces a compilation error, due to reassignment

calcChange owned given =
  if change > 0
  then change
  else 0
  where change = owned - given

doublePlusTwo x = doubleX + 2
  where doubleX = x * 2

inc x = x + 1
double x = x * 2
square x = x ^ 2

q2p3 n =
  if even n
  then n - 2
  else 3 * n + 1
