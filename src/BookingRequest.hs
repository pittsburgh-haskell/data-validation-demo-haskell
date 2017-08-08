module BookingRequest ( BookingRequest
                      , getDate
                      , getSeats
                      , make
                      , Error(..)
                      ) where

import Control.Arrow ((>>>))
import Data.Either.Validation
import Data.Either.Combinators (mapLeft)

import qualified Seats
import qualified Date

data BookingRequest =
  BookingRequest { getDate :: Date.Date
                 , getSeats :: Seats.Seats
                 }
  deriving (Show, Eq)

-- | All the errors in our application.
--
-- An alternate design: could keep notion of error "open" by
-- other means.
data Error =
    DateError Date.Error
  | SeatsError Seats.Error
  | Missing String       -- ^ label
  | DateBefore Date.Date
               Date.Date -- ^ date attempted, current date at attempt
               deriving (Eq)

instance Show Error where
  show (DateError e) = show e
  show (SeatsError e) = show e
  show (Missing label) = label ++ " is missing"
  show (DateBefore date1 date2) = "date " ++ show date1 ++
    " cannot be before " ++ show date2

-- | Smart constructor that accumulates all validation errors.
make :: Date.Date    -- ^ time when attempting request
     -> Maybe String -- ^ optional date string for event
     -> Maybe Int    -- ^ optional number of seats
     -> Validation [Error] BookingRequest
make now optDateString optSeats = BookingRequest
  <$> eitherToValidationL (makeTimelyBookingDate now optDateString)
  <*> eitherToValidationL (makeSeats optSeats)

makeTimelyBookingDate :: Date.Date
                      -> Maybe String
                      -> Either Error Date.Date
makeTimelyBookingDate now optDateString = do
  dateString <- optDateString `maybeToEither` Missing "date"
  date <- mapLeft DateError $ Date.parse dateString
  timelyBookingDate date now

timelyBookingDate :: Date.Date -- ^ attempted booking
                  -> Date.Date -- ^ now
                  -> Either Error Date.Date
timelyBookingDate date now
  | not $ Date.isBefore date now = Right date
  | otherwise = Left $ DateBefore date now

makeSeats :: Maybe Int -> Either Error Seats.Seats
makeSeats = maybe (Left $ Missing "seats")
            (Seats.make >>> mapLeft SeatsError)

-- | Utility to convert 'Maybe' to 'Either' with a
-- custom error.
maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e  = Left e
maybeToEither (Just a) _ = Right a

-- | Utility.
eitherToValidationL :: Either e a -> Validation [e] a
eitherToValidationL =
  mapLeft pure >>> eitherToValidation
