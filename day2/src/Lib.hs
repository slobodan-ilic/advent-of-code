module Lib
  ( calculateFuelFrom
  , displayResult
  , getFuel
  ) where

getFuel :: Integer -> Integer
getFuel mass =
  if fuel <= 8
    then fuel
    else fuel + getFuel fuel
  where
    fuel = mass `div` 3 - 2

calculateFuelFrom :: String -> Integer
calculateFuelFrom contents = foldr (+) 0 fuels
  where
    modules = getMassesFrom contents
    fuels = map getFuel modules

getMassesFrom :: String -> [Integer]
getMassesFrom contents = map getMassFrom entries
  where
    getMassFrom = toInteger . read
    entries = words contents

fuelForModule :: Integer -> Integer
fuelForModule m = m `div` 3 - 2

displayResult :: Integer -> IO ()
displayResult fuel = do
  putStrLn "Total fuel needed: "
  putStrLn $ show fuel
