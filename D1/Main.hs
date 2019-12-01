module Main where

import           System.Environment             ( getArgs )

import           Types

readInput :: String -> [Mass]
readInput = map (Mass . read) . lines

part1 :: [Mass] -> Int
part1 = getFuel . sum . map calculateFuel

part2 :: [Mass] -> Int
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

runTestSuite :: IO ()
runTestSuite = either putStrLn (const $ putStrLn "All tests passed!") $ do
  runTests calcFuelTests
  runTests [part1Test]

data TestCase i o = TC
  { input :: i
  , expected :: o
  , fun :: i -> o
  , name :: String
  }

mkTC :: (Mass -> Fuel) -> (String, Mass, Fuel) -> TestCase Mass Fuel
mkTC f (n, inp, expec) =
  TC { name = n, input = inp, expected = expec, fun = f }

runTest :: Eq o => TestCase i o -> Bool
runTest tc = fun tc (input tc) == expected tc

runTests :: (Eq o, Show o) => [TestCase i o] -> Either String ()
runTests = foldr go (Right ())
 where
  go :: (Eq o, Show o) => TestCase i o -> Either String () -> Either String ()
  go tc res = res >> if runTest tc then Right () else Left (failMsg tc)

  failMsg :: (Show o, Eq o) => TestCase i o -> String
  failMsg (TC inp expec f nam) = unlines
    [ "Test case \"" <> nam <> "\" failed."
    , "Expected result " <> show expec <> ", actual result: " <> show (f inp)
    ]

calcFuelTests :: [TestCase Mass Fuel]
calcFuelTests = map
  (mkTC calculateFuel)
  [ ("calcFuel 12"    , Mass 12    , 2)
  , ("calcFuel 14"    , Mass 14    , 2)
  , ("calcFuel 1969"  , Mass 1969  , 654)
  , ("calcFuel 100765", Mass 100765, 33586)
  ]

part1Test :: TestCase [Mass] Int
part1Test = TC { name     = "Part1"
               , expected = 34244
               , input    = map Mass [12, 14, 1969, 100765]
               , fun      = part1
               }
