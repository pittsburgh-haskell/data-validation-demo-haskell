module Seats ( Seats
             -- * Accessor
             , getNum
             -- * Our errors
             , Error(..)
             -- * Smart constructor
             , make
             ) where

import Data.Validation

-- | Wrapper around 'Int' that ensures always positive.
newtype Seats = Seats { getNum :: Int }
  deriving (Show, Eq)

data Error = BadCount Int -- ^ attempted number of seats
             deriving (Eq)

instance Show Error where
  show (BadCount seats) = "number of seats was " ++ show seats ++
    ", but must be positive"

-- | Smart constructor for 'Seats' that
-- ensures always positive.
make :: Int -> Validation Error Seats
make seats | seats <= 0 = Failure $ BadCount seats
           | otherwise = Success $ Seats seats
