-- -*- dante-target: "D4" -*-
module Main where

import           Data.List                      ( sort
                                                , group
                                                )

validPassword :: String -> Bool
validPassword pwd = length pwd == 6 && allIncreasing pwd && doubleLetters pwd
 where
  allIncreasing :: String -> Bool
  allIncreasing s = sort s == s

  doubleLetters :: String -> Bool
  doubleLetters s = foldr (\l b -> b || length l > 1) False $ group s

part1test :: Bool
part1test = validPassword "111111" && not (validPassword "223450") && not
  (validPassword "123789")

validPart2 :: String -> Bool
validPart2 pwd = length pwd == 6 && allIncreasing pwd && doubleLetters pwd
  where
    allIncreasing :: String -> Bool
    allIncreasing s = sort s == s

    doubleLetters :: String -> Bool
    doubleLetters s = foldr (\l b -> b || length l == 2) False $ group s

part2test :: Bool
part2test = validPart2 "112233" && not (validPart2 "123444") && validPart2 "111122"

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn $ "Answer: " <> (show . length $ [ pwd | pwd <- [min' .. max'], validPassword (show pwd) ])
  putStrLn "Part 2"
  putStrLn $ "Answer: " <> (show . length $ [ pwd | pwd <- [min' .. max'], validPart2 (show pwd)])
  where min' = 284639 :: Integer
        max' = 748759 :: Integer
