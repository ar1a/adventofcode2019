module Main where

main :: IO ()
main = do
  input <- getContents
  let input'  = lines input
  let numbers = read <$> input'
  print $ sum $ day1part2 <$> numbers


day1part1 :: Float -> Int
day1part1 mass = floor (mass / 3) - 2

day1part2 :: Integer -> Integer
day1part2 mass = go $ fuelcalc mass
 where
  fuelcalc :: Integer -> Integer
  fuelcalc x = floor (fromInteger x / 3) - 2
  go :: Integer -> Integer
  go x | fuelcalc x > 0 = x + go (fuelcalc x)
       | otherwise      = x
