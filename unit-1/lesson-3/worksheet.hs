double = \x -> x * 2

sumOfSquaresOrSquareSumWithWhere x y =
  if sumSquare > squareSum
  then sumSquare
  else squareSum
  where
    sumSquare = x^2 + y^2
    squareSum = (x + y)^2

body sumSquare squareSum =
  if sumSquare > squareSum
  then sumSquare
  else squareSum
sumOfSquaresOrSquareSum x y = body (x^2 + y^2) ((x + y)^2)

sumOfSquaresOrSquareSumWithLambda x y =
  (\sumSquare squareSum ->
    if sumSquare > squareSum
    then sumSquare
    else squareSum) (x^2 + y^2) ((x + y)^2)

doubleDouble x = dubs * 2
  where dubs = x * 2
doubleDoubleWithLambda x = (\y -> y * 2) (x * 2)

sumOfSquaresOrSquareOfSumWithLet x y =
  let
    sumSquare = (x^2 + y^2)
    squareSum = (x + y)^2
  in
    if sumSquare > squareSum
    then sumSquare
    else squareSum

overwrite x =
  let x = 2
    in
      let x = 3
        in
          let x = 4
            in
              x
overwriteWithLambdas x = (\x -> (\x -> (\x -> x) 4) 3) 2

counter x =
  let x = x + 1
    in
      let x = x + 1
        in
          x
counterWithLambda x =
  (\x -> (\x -> x) x + 1) x + 1
