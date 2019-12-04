-- -*- dante-target: "D4" -*-
module Main where

import           Data.List                      ( sort
                                                , group
                                                )

generatePasswords :: Int -> Int -> [Int]
generatePasswords min' max' =
  [ pwd | pwd <- [min' .. max'], validPassword (show pwd) ]

validPassword :: String -> Bool
validPassword pwd = length pwd == 6 && allIncreasing pwd && doubleLetters pwd
 where
  allIncreasing :: String -> Bool
  allIncreasing s = sort s == s

  doubleLetters :: String -> Bool
  doubleLetters s = foldr (\l b -> b || length l > 1) False $ group s

test :: Bool
test = validPassword "111111" && not (validPassword "223450") && not
  (validPassword "123789")

main :: IO ()
main = do
  putStrLn "Part 1"
  putStrLn $ "Answer: " <> (show . length $ generatePasswords 284639 748759)
