module Main where

import           Data.List                      ( foldl' )

import           Types

readInput :: String -> [Mass]
readInput = map (Mass . read) . lines

solvePart1 :: [Mass] -> Int
solvePart1 = getFuel . foldl' (+) 0 . map calculateFuel

main :: IO ()
main = do
  inp <- readInput <$> getContents
  print $ solvePart1 inp

tests :: Bool
tests = all (uncurry (==))
  [ (calculateFuel (Mass 12), 2)
  , (calculateFuel (Mass 14), 2)
  , (calculateFuel (Mass 1969), 654)
  , (calculateFuel (Mass 100765), 33586)
  ]
