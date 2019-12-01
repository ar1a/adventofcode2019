module Main where

main :: IO ()
main = do
  input <- getContents
  let input'  = lines input
  let numbers = read <$> input'
  print $ sum $ calculate <$> numbers

calculate :: Float -> Int
calculate mass = floor (mass / 3) - 2
