{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow                  ( (>>>) )
import           Data.Char                      ( isDigit )
import           System.Environment             ( getArgs )

data Machine = IntCode
  { mem :: [Int]
  , pc :: Int
  } deriving (Show, Eq)

mkMachine :: [Int] -> Machine
mkMachine is = IntCode { mem = is, pc = 0 }

runMachine :: Machine -> Machine
runMachine = fromLeft . eval
 where
  fromLeft (Left a) = a
  fromLeft _        = error "fromLeft: Right"

  eval :: Machine -> Either Machine Machine
  eval m = case fetch m of
    Nothing                -> Left m
    Just (op, i1, i2, out) -> eval $ incPC (setMem m out (op i1 i2))

   -- | Fetch opcode and operands if these are well formed and not Halt (99)
  fetch :: Machine -> Maybe (Int -> Int -> Int, Int, Int, Int)
  fetch (IntCode mem' pc') = case drop pc' mem' of
    (1 : i1 : i2 : out : _) -> pure ((+), mem' !! i1, mem' !! i2, out)
    (2 : i1 : i2 : out : _) -> pure ((*), mem' !! i1, mem' !! i2, out)
    _                       -> Nothing

  -- | Update memory in machine with value
  setMem :: Machine -> Int -> Int -> Machine
  setMem m' i val = m' { mem = update i val (mem m') }

  -- | Increment the program counter
  incPC :: Machine -> Machine
  incPC m' = m' { pc = pc m' + 4 }

update :: Int -> a -> [a] -> [a]
update _ _ [] = error "update: empty list"
update i v xs
  | length xs > i = let (hs, _ : ts) = splitAt i xs in hs <> (v : ts)
  | otherwise     = error "update: index out of bounds"

part1 :: [Int] -> Int
part1 = head . mem . runMachine . mkMachine . prepare
 where
    -- | Set the initial state to when the computer crashed (1202 gravity error)
  prepare :: [Int] -> [Int]
  prepare = update 1 12 . update 2 2

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
runTestSuite = either putStrLn (const $ putStrLn "All tests passed!") $ do
  runTests inputTests
  runTests updateTests
  runTests part1Tests

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

updateTests :: [TestCase (Int, Int, [Int]) [Int]]
updateTests = map
  (mkTC (\(i, v, xs) -> update i v xs))
  [ ((2, 1, [1, 2, 3, 4, 5]), [1, 2, 1, 4, 5])
  , ((0, 2, [1])            , [2])
  , ((2, 100, [1, 2, 3])    , [1, 2, 100])
  ]

part1Tests :: [TestCase Machine [Int]]
part1Tests = map
  (mkTC (mem . runMachine))
  [ ( mkMachine [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
    , [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    )
  , (mkMachine [1, 0, 0, 0, 99]             , [2, 0, 0, 0, 99])
  , (mkMachine [2, 3, 0, 3, 99]             , [2, 3, 0, 6, 99])
  , (mkMachine [2, 4, 4, 5, 99, 0]          , [2, 4, 4, 5, 99, 9801])
  , (mkMachine [1, 1, 1, 4, 99, 5, 6, 0, 99], [30, 1, 1, 4, 2, 5, 6, 0, 99])
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
