module Main where

main :: IO ()
main = do
  input <- getContents
  let input'  = lines input
  let numbers = read <$> input'
  print $ sum $ calculate <$> numbers

calculate :: Integer -> Integer
calculate mass = go $ fuelcalc mass
 where
  fuelcalc :: Integer -> Integer
  fuelcalc x = floor (fromInteger x / 3) - 2
  go :: Integer -> Integer
  go x | fuelcalc x > 0 = x + go (fuelcalc x)
       | otherwise      = x
