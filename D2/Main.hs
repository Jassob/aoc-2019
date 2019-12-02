{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow                  ( (>>>) )
import           Data.Char                      ( isDigit )
import           System.Environment             ( getArgs )

part1 :: [Int] -> Int
part1 = undefined

part2 :: [Int] -> Int
part2 = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("test"  : _) -> runTestSuite
    ("part1" : _) -> solve part1
    ("part2" : _) -> solve part2
    ("help"  : _) -> printHelp
    []            -> solve part1 -- default is to solve part1
    _             -> printHelp
 where
  solve :: ([Int] -> Int) -> IO ()
  solve f = print =<< f . readInput <$> getContents

  printHelp :: IO ()
  printHelp = putStrLn $ unlines
    [ "Usage: Day2 [OPTION], if no option is given part1 is assumed"
    , "For both part1 and part2 input is supposed to be given on standard input"
    , "redirecting from a file is recommended"
    , ""
    , "Valid options:"
    , "test\t\tRun test suite"
    , "part1\t\tSolve part 1"
    , "part2\t\tSolve part 2"
    , "help\t\tPrint this text"
    ]

readInput :: String -> [Int]
readInput = span isDigit >>> \case
  (n , _ : is) -> read n : readInput is
  ("", ""    ) -> []
  (n , ""    ) -> [read n]

runTestSuite :: IO ()
runTestSuite = either putStrLn (const $ putStrLn "All tests passed!") $
  runTests inputTests

data TestCase i o = TC
  { input :: i
  , expected :: o
  , fun :: i -> o
  }

inputTests :: [TestCase String [Int]]
inputTests = map (mkTC readInput)
  [ ("1,2,3,4,5", [1..5])
  , ("1", [1])
  ]

mkTC :: (i -> o) -> (i, o) -> TestCase i o
mkTC f (inp, expec) = TC { input = inp, expected = expec, fun = f }

runTest :: Eq o => TestCase i o -> Bool
runTest tc = fun tc (input tc) == expected tc

runTests :: (Eq o, Show i, Show o) => [TestCase i o] -> Either String ()
runTests = foldr go (Right ())
 where
  go
    :: (Eq o, Show i, Show o)
    => TestCase i o
    -> Either String ()
    -> Either String ()
  go tc res = res >> if runTest tc then Right () else Left (failMsg tc)

  failMsg :: (Eq o, Show i, Show o) => TestCase i o -> String
  failMsg (TC inp expec f) = unlines
    [ "Test case \"" <> show (inp, expec) <> "\" failed."
    , "Expected result " <> show expec <> ", actual result: " <> show (f inp)
    ]
