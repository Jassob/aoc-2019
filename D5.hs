-- -*- dante-target: "D5" -*-
{-# LANGUAGE LambdaCase, OverloadedLists #-}
module Main where

import           Control.Arrow                  ( (>>>) )
import           Data.Char                      ( isDigit )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

data Machine = IntCode
  { mem :: Vector Int
  , pc :: Int
  } deriving (Show, Eq)

data Op
  = Add Int Int Int
  | Mul Int Int Int
  | ReadInt Int
  | PrintInt Int
  | Halt
  deriving (Eq, Show)

mkMachine :: Vector Int -> Machine
mkMachine is = IntCode { mem = is, pc = 0 }

run :: Machine -> IO Machine
run m = case fetch m of
  Just op | op == Halt -> pure m
          | otherwise  -> incPC op <$> evalOp m op >>= run
  Nothing -> error $ unwords
    [ "eval: Malformed instruction at"
    , show (pc m)
    , "in"
    , show (V.zip [0 .. V.length (mem m)] (mem m))
    ]
 where
  evalOp :: Machine -> Op -> IO Machine
  evalOp m' = \case
    Add a b out  -> pure $ setMem m' out (a + b)
    Mul a b out  -> pure $ setMem m' out (a * b)
    ReadInt  out -> setMem m' out . read <$> getLine
    PrintInt v   -> m <$ print v
    _            -> error "Not implemented"

  -- | Fetch opcode and operands if these are well formed and not Halt (99)
  fetch :: Machine -> Maybe Op
  fetch (IntCode mem' pc') = case unsnoc $ V.drop pc' mem' of
    (0001, as) -> pure $ Add (pos as 0) (pos as 1) (imm as 2)
    (0101, as) -> pure $ Add (imm as 0) (pos as 1) (imm as 2)
    (1001, as) -> pure $ Add (pos as 0) (imm as 1) (imm as 2)
    (1101, as) -> pure $ Add (imm as 0) (imm as 1) (imm as 2)
    (0002, as) -> pure $ Mul (pos as 0) (pos as 1) (imm as 2)
    (0102, as) -> pure $ Mul (imm as 0) (pos as 1) (imm as 2)
    (1002, as) -> pure $ Mul (pos as 0) (imm as 1) (imm as 2)
    (1102, as) -> pure $ Mul (imm as 0) (imm as 1) (imm as 2)
    (03  , as) -> pure $ ReadInt (imm as 0)
    (004 , as) -> pure $ PrintInt (pos as 0)
    (104 , as) -> pure $ PrintInt (imm as 0)
    (99  , _ ) -> pure Halt
    _          -> Nothing
   where
    imm :: Vector Int -> Int -> Int
    imm as i = as V.! i

    pos :: Vector Int -> Int -> Int
    pos as i = mem' V.! (as V.! i)

  unsnoc :: Vector a -> (a, Vector a)
  unsnoc v = (V.head v, V.drop 1 v)

  -- | Update memory in machine with value
  setMem :: Machine -> Int -> Int -> Machine
  setMem m' i val = m' { mem = mem m' V.// [(i, val)] }

  -- | Increment the program counter
  incPC :: Op -> Machine -> Machine
  incPC op m' = m' { pc = pc m' + amount }
   where
    amount = case op of
      Add{}      -> 4
      Mul{}      -> 4
      ReadInt{}  -> 2
      PrintInt{} -> 2
      Halt       -> 1

readInput :: String -> Vector Int
readInput = span (\c -> isDigit c || c == '-') >>> \case
  (n , _ : is) -> read n `V.cons` readInput is
  ("", ""    ) -> V.empty
  (n , ""    ) -> V.singleton $ read n

tests :: IO Bool
tests = do
  m <- mem <$> (run . mkMachine $ readInput "1002,4,3,4,33")
  pure $ m == [1002, 4, 3, 4, 99]

main :: IO ()
main = do
  inp <- readInput <$> readFile "d5.txt"
  print (V.length inp)
  putStrLn "Part 1: Insert 1"
  _ <- run $ mkMachine inp
  putStrLn "Part 2: TBD"
