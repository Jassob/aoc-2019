{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
  ( Mass(..)
  , Fuel
  , getFuel
  , calculateFuel
  )
where

newtype Mass = Mass { getMass :: Int }
  deriving (Eq, Show)

newtype Fuel = Fuel { getFuel :: Int }
  deriving (Eq, Num)

instance Show Fuel where
  show (Fuel i) = show i

calculateFuel :: Mass -> Fuel
calculateFuel (Mass m) = Fuel $ m `div` 3 - 2
