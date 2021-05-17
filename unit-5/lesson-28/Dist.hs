{-# LANGUAGE OverloadedStrings #-}

module Dist where

import qualified Data.Map as Map
import Data.List ( intercalate )


type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [
    ("Arkham", (42.6054, -70.7829)),
    ("Innsmouth", (42.8250, -70.8150)),
    ("Carcosa", (29.9714, -90.7694)),
    ("New York", (40.7776, -73.9691))
  ]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
  where
    rlat = toRadians lat
    rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
  where
    (rlat1, rlong1) = latLongToRads coords1
    (rlat2, rlong2) = latLongToRads coords2
    dlat = rlat2 - rlat1
    dlong = rlong2 - rlong1
    a = (sin (dlat / 2)) ^ 2 + cos rlat1 * cos rlat2 * (sin (dlong / 2)) ^2
    c = 2 * atan2 (sqrt a) (sqrt  (1 - a))
    earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn $ "Please, enter cities from the following list:\n" ++ citiesListStr
  where
    cities = fst <$> Map.toList locationDB
    citiesListStr = intercalate ", " cities
printDistance (Just distance) = putStrLn $ show distance ++ " miles"

-- haversineMaybe :: Maybe LatLong -> Maybe LatLong -> Maybe Double
-- haversineMaybe Nothing _ = Nothing
-- haversineMaybe _ Nothing = Nothing
-- haversineMaybe (Just val1) (Just val2) = Just (haversine val1 val2)

{-|

>>> startingCity = Map.lookup "Carcosa" locationDB
>>> destCity = Map.lookup "Innsmouth" locationDB
>>> haversine <$> startingCity <*> destCity
Just 1415.7942372467567

|-}

main :: IO ()
main = do
  putStrLn "Enter the starting city name:"
  startingInput <- getLine
  let startingCity = Map.lookup startingInput locationDB
  putStrLn "Enter the destination city name:"
  destInput <- getLine
  let destCity = Map.lookup destInput locationDB
  let distance = haversine <$> startingCity <*> destCity
  printDistance distance


-- haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO ioSource ioDestination = do
--   source <- ioSource
--   destination <- ioDestination
--   return $ haversine source destination

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ioSource ioDestination = haversine <$> ioSource <*> ioDestination
