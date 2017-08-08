module Date ( Date(..)
            -- * Our errors
            , Error(..)
            -- * Smart constructor
            , parse
            , isBefore
            ) where

import Text.Read (readMaybe)
import Data.Validation

-- | Dummy 'Date' type for illustration only.
newtype Date = Date { getSeconds :: Int }
               deriving (Eq)

instance Show Date where
  show (Date s) = show s

data Error = BadParse String -- ^ what failed to parse
             deriving (Eq)

instance Show Error where
  show (BadParse s) = "failed to parse date string " ++ s

-- | Try to parse @s@ into a valid Date, using 'readMaybe'.
parse :: String -> Validation Error Date
parse s = case readMaybe s of
            Nothing -> Failure $ BadParse s
            Just seconds -> Success $ Date seconds

-- | Return where @date1@ is before @date2@.
isBefore :: Date -> Date -> Bool
isBefore (Date d1) (Date d2) = d1 < d2
