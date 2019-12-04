{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow                  ( (>>>) )
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromMaybe )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           System.Environment             ( getArgs )
import           Text.Read                      ( readMaybe )

data Direction = U Int
               | R Int
               | D Int
               | L Int
               deriving (Eq, Show)

instance Read Direction where
  readsPrec _ r = mconcat
    [ [ (U m, s) | ('U' : n, s) <- lex r, (m, "") <- reads n ]
    , [ (R m, s) | ('R' : n, s) <- lex r, (m, "") <- reads n ]
    , [ (D m, s) | ('D' : n, s) <- lex r, (m, "") <- reads n ]
    , [ (L m, s) | ('L' : n, s) <- lex r, (m, "") <- reads n ]
    ]

data St = St
  { stMap :: Set Pos
  , stCurrPos :: Pos
  , stSteps :: Map Pos Int
  , stCurrSteps :: Int
  } deriving (Eq, Show)

type Pos = (Int, Int)

addDirection :: Pos -> Direction -> Pos
addDirection (x, y) = \case
  R i -> (x + i, y)
  L i -> (x - i, y)
  U i -> (x, y + i)
  D i -> (x, y - i)

-- | Returns the points between two other points, excluding the
-- endpoints
pointsBetween :: Pos -> Pos -> [Pos]
pointsBetween (x1, y1) (x2, y2)
  | x1 /= x2 && y1 /= y2
  = error "No common axis"
  | otherwise
  = let xPoints =
            [ (x, y1) | x <- [(min x1 x2) .. (max x1 x2)], x /= x1 && x /= x2 ]
        yPoints =
            [ (x1, y) | y <- [(min y1 y2) .. (max y1 y2)], y /= y1 && y /= y2 ]
    in  xPoints ++ yPoints

posDist :: Pos -> Int
posDist (x, y) = abs x + abs y

emptySt :: St
emptySt = St Set.empty (0, 0) Map.empty 0

eval :: St -> [Direction] -> St
eval = foldl' go
 where
  go :: St -> Direction -> St
  go st' d =
    let curr   = addDirection (stCurrPos st') d
        points = curr : pointsBetween (stCurrPos st') curr
    in  setCurrPos curr $ foldl' addPoint st' points

  addPoint :: St -> Pos -> St
  addPoint st' p = incCurrStep . setCurrPos p $ st'
    { stMap   = Set.insert p (stMap st')
    , stSteps = Map.insert p (stCurrSteps st') (stSteps st')
    }

  setCurrPos :: Pos -> St -> St
  setCurrPos p st' = st' { stCurrPos = p }

  incCurrStep :: St -> St
  incCurrStep st' = st' { stCurrSteps = stCurrSteps st' + 1 }

evalBoth :: [[Direction]] -> (St, St)
evalBoth [d1, d2] = (eval emptySt d1, eval emptySt d2)
evalBoth _        = error "evalBoth: Only two direction lists allowed"

part1 :: [[Direction]] -> Int
part1 =
  fromMaybe (error "part1: No overlap distance greater than 0")
    . Set.lookupGT 0
    . Set.map posDist
    . (\(st1, st2) -> Set.intersection (stMap st1) (stMap st2))
    . evalBoth

part2 :: [[Direction]] -> Int
part2 =
  minimum
    . Map.elems
    . uncurry (Map.unionWith (+))
    . (\(st1, st2) ->
        let overlaps  = Set.intersection (stMap st1) (stMap st2)
            filterKey = Map.filterWithKey (\k _ -> k `Set.member` overlaps)
        in  (filterKey $ stSteps st1, filterKey $ stSteps st2)
      )
    . evalBoth

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
  solve :: ([[Direction]] -> Int) -> IO ()
  solve f = print =<< f . readInput <$> getContents

  printHelp :: IO ()
  printHelp = putStrLn $ unlines
    [ "Usage: Day3 [OPTION], if no option is given part1 is assumed"
    , "For both part1 and part2 input is supposed to be given on standard input"
    , "redirecting from a file is recommended"
    , ""
    , "Valid options:"
    , "test\t\tRun test suite"
    , "part1\t\tSolve part 1"
    , "part2\t\tSolve part 2"
    , "help\t\tPrint this text"
    ]

readInput :: String -> [[Direction]]
readInput = lines >>> map readInput'
 where
  readInput' :: String -> [Direction]
  readInput' = break (== ',') >>> \case
    (n, _ : is) ->
      maybe (error $ "failed to read " <> show n) (: readInput' is)
        $ readMaybe n
    ("", "") -> []
    (n , "") -> [read n]

runTestSuite :: IO ()
runTestSuite = either putStrLn (const $ putStrLn "All tests passed!") $ do
  runTests addDirectionTests
  runTests readDirectionTests
  runTests part1Tests
  runTests part2Tests

data TestCase i o = TC
  { input :: i
  , expected :: o
  , fun :: i -> o
  }

mkTC :: (i -> o) -> (i, o) -> TestCase i o
mkTC f (inp, expec) = TC { input = inp, expected = expec, fun = f }

addDirectionTests :: [TestCase (Pos, Direction) Pos]
addDirectionTests = map
  (mkTC (uncurry addDirection))
  [ (((0, 0), U 0) , (0, 0))
  , (((0, 0), R 0) , (0, 0))
  , (((0, 0), D 0) , (0, 0))
  , (((0, 0), L 0) , (0, 0))
  , (((0, 0), U 10), (0, 10))
  , (((0, 0), R 10), (10, 0))
  , (((0, 0), D 10), (0, -10))
  , (((0, 0), L 10), (-10, 0))
  ]

readDirectionTests :: [TestCase String (Maybe Direction)]
readDirectionTests = map
  (mkTC readMaybe)
  [ ("R10", pure $ R 10)
  , ("D10", pure $ D 10)
  , ("L10", pure $ L 10)
  , ("U10", pure $ U 10)
  , ("U"  , Nothing)
  , ("10" , Nothing)
  ]

part1Tests :: [TestCase [[Direction]] Int]
part1Tests = map
  (mkTC part1)
  [ ([[R 8, U 5, L 5, D 3], [U 7, R 6, D 4, L 4]], 6)
  , ( [ [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72]
      , [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83]
      ]
    , 159
    )
  , ( [ [R 98, U 47, R 26, D 63, R 33, U 87, L 62, D 20, R 33, U 53, R 51]
      , [U 98, R 91, D 20, R 16, D 67, R 40, U 7, R 15, U 6, R 7]
      ]
    , 135
    )
  ]

part2Tests :: [TestCase [[Direction]] Int]
part2Tests = map
  (mkTC part2)
  [ ([[R 8, U 5, L 5, D 3], [U 7, R 6, D 4, L 4]], 30)
  , ( [ [R 75, D 30, R 83, U 83, L 12, D 49, R 71, U 7, L 72]
      , [U 62, R 66, U 55, R 34, D 71, R 55, D 58, R 83]
      ]
    , 610
    )
  , ( [ [R 98, U 47, R 26, D 63, R 33, U 87, L 62, D 20, R 33, U 53, R 51]
      , [U 98, R 91, D 20, R 16, D 67, R 40, U 7, R 15, U 6, R 7]
      ]
    , 415
    )
  ]

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
