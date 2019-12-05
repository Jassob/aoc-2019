-- -*- dante-target: "D5" -*-
{-# LANGUAGE LambdaCase, OverloadedLists #-}
module Main where

import           Control.Arrow                  ( (>>>) )
import           Data.Char                      ( isDigit )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Prelude                 hiding ( LT
                                                , EQ
                                                )

data Machine = IntCode
  { mem :: Vector Int
  , pc :: Int
  } deriving (Show, Eq)

data Op
  = Add Int Int Int
  | Mul Int Int Int
  | ReadInt Int
  | PrintInt Int
  | JEQ Int Int
  | JNE Int Int
  | LT Int Int Int
  | EQ Int Int Int
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
    JEQ b pc'    -> pure $ if b /= 0 then setPC pc' m' else m'
    JNE b pc'    -> pure $ if b == 0 then setPC pc' m' else m'
    LT a b out   -> pure $ setMem m' out $ if a < b then 1 else 0
    EQ a b out   -> pure $ setMem m' out $ if a == b then 1 else 0
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
    (0005, as) -> pure $ JEQ (pos as 0) (pos as 1)
    (0105, as) -> pure $ JEQ (imm as 0) (pos as 1)
    (1005, as) -> pure $ JEQ (pos as 0) (imm as 1)
    (1105, as) -> pure $ JEQ (imm as 0) (imm as 1)
    (0006, as) -> pure $ JNE (pos as 0) (pos as 1)
    (0106, as) -> pure $ JNE (imm as 0) (pos as 1)
    (1006, as) -> pure $ JNE (pos as 0) (imm as 1)
    (1106, as) -> pure $ JNE (imm as 0) (imm as 1)
    (0007, as) -> pure $ LT (pos as 0) (pos as 1) (imm as 2)
    (0107, as) -> pure $ LT (imm as 0) (pos as 1) (imm as 2)
    (1007, as) -> pure $ LT (pos as 0) (imm as 1) (imm as 2)
    (1107, as) -> pure $ LT (imm as 0) (imm as 1) (imm as 2)
    (0008, as) -> pure $ EQ (pos as 0) (pos as 1) (imm as 2)
    (0108, as) -> pure $ EQ (imm as 0) (pos as 1) (imm as 2)
    (1008, as) -> pure $ EQ (pos as 0) (imm as 1) (imm as 2)
    (1108, as) -> pure $ EQ (imm as 0) (imm as 1) (imm as 2)
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

  setPC :: Int -> Machine -> Machine
  setPC pc' m' = m' { pc = pc' }

  -- | Increment the program counter
  incPC :: Op -> Machine -> Machine
  incPC op m' = setPC (pc m' + amount) m'
   where
    amount = case op of
      Add{}      -> 4
      Mul{}      -> 4
      ReadInt{}  -> 2
      PrintInt{} -> 2
      JEQ b _ | b /= 0    -> 0
              | otherwise -> 3
      JNE b _ | b == 0    -> 0
              | otherwise -> 3
      LT{} -> 4
      EQ{} -> 4
      Halt -> 1

readInput :: String -> Vector Int
readInput = span (\c -> isDigit c || c == '-') >>> \case
  (n , _ : is) -> read n `V.cons` readInput is
  ("", ""    ) -> V.empty
  (n , ""    ) -> V.singleton $ read n

tests :: IO ()
tests = do
  putStrLn "================================================="
  putStrLn "= Part 1"
  m <- mem <$> (run . mkMachine $ readInput "1002,4,3,4,33")
  putStrLn $ "First test " <> if m == [1002, 4, 3, 4, 99]
    then "succeeded!"
    else "failed."

  putStrLn "================================================="
  putStrLn "= Part 2"
  putStrLn "Enter any number:"
  _ <-
    ( run
    . mkMachine
    $ readInput
        "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    )
  putStrLn "The output should be: "
  putStrLn "* 999, if your input was < 8"
  putStrLn "* 1000, if your input was 8"
  putStrLn "* 1001, if your input was > 8"


main :: IO ()
main = do
  inp <- readInput <$> readFile "d5.txt"
  putStrLn "================================================"
  putStrLn "= Welcome to TEST diagnostic program!"
  putStrLn "= ----------------------------------------------"
  putStrLn "="
  putStrLn "= Enter which system you want to test."
  putStrLn "= * System ID 1: Heater system (Day 5, part 1)"
  putStrLn "= * System ID 5: Cooler system (Day 5, part 2)"
  putStrLn "================================================"
  putStr "Enter system ID: "
  _ <- run $ mkMachine inp
  pure ()
