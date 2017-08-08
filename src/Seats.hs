module Seats ( Seats     -- ^ don't export constructor
             , getNum    -- ^ accessor
             , Error(..) -- ^ our errors
             , make      -- ^ smart constructor
             ) where

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
make :: Int -> Either Error Seats
make seats | seats <= 0 = Left $ BadCount seats
           | otherwise = Right $ Seats seats
