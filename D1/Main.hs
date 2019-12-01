module Main where

import           Data.List                      ( foldl' )
import           System.Environment             ( getArgs )

import           Types

readInput :: String -> [Mass]
readInput = map (Mass . read) . lines

part1 :: [Mass] -> Int
part1 = getFuel . foldl' (+) 0 . map calculateFuel

part2 :: [Mass] -> Int
part2 = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("test" : _) | tests     -> putStrLn "All tests passed!"
                 | otherwise -> putStrLn "At least one test failed"
    ("part1" : _) -> solve part1
    ("part2" : _) -> solve part2
    ("help" : _) -> printHelp
    [] -> solve part1 -- default is to solve part1
    _  -> printHelp
  where
    solve :: ([Mass] -> Int) -> IO ()
    solve f = print =<< f . readInput <$> getContents

    printHelp :: IO ()
    printHelp = putStrLn $ unlines
      [ "Usage: Day1 [OPTION], if no option is given part1 is assumed"
      , "For both part1 and part2 input is supposed to be given on standard input"
      , "redirecting from a file is recommended"
      , ""
      , "Valid options:"
      , "test\t\tRun test suite"
      , "part1\t\tSolve part 1"
      , "part2\t\tSolve part 2"
      , "help\t\tPrint this text"
      ]

tests :: Bool
tests =
  all
      (uncurry (==))
      [ (calculateFuel (Mass 12)    , 2)
      , (calculateFuel (Mass 14)    , 2)
      , (calculateFuel (Mass 1969)  , 654)
      , (calculateFuel (Mass 100765), 33586)
      ]
    && (part1 (map Mass [12, 14, 1969, 100765]) == 34244)
