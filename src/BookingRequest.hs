module BookingRequest ( BookingRequest
                      , getDate
                      , getSeats
                      , make
                      , Error(..)
                      ) where

import qualified Seats
import qualified Date
import Data.Validation

-- For GHC < 7.10
import Control.Applicative ((<$>), (<*>))

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
  | DateBefore Date.Date -- ^ date that was attempted
               Date.Date -- ^ the current date at attempt
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
     -> AccValidation [Error] BookingRequest
make now optDateString optSeats = BookingRequest
  <$> vToAccVList (makeTimelyBookingDate now optDateString)
  <*> vToAccVList (makeSeats optSeats)

makeTimelyBookingDate :: Date.Date
                      -> Maybe String
                      -> Validation Error Date.Date
makeTimelyBookingDate now optDateString = do
  dateString <- optDateString `maybeToV` Missing "date"
  date <- mapFailure DateError $ Date.parse dateString
  timelyBookingDate date now

timelyBookingDate :: Date.Date -- ^ attempted booking
                  -> Date.Date -- ^ now
                  -> Validation Error Date.Date
timelyBookingDate date now
  | not $ Date.isBefore date now = Success date
  | otherwise = Failure $ DateBefore date now

makeSeats :: Maybe Int -> Validation Error Seats.Seats
makeSeats optSeats = do
  num <- optSeats `maybeToV` Missing "seats"
  mapFailure SeatsError $ Seats.make num

-- | Utility to convert 'Maybe' to 'Validation' with a
-- custom error.
maybeToV :: Maybe a -> e -> Validation e a
maybeToV Nothing e  = Failure e
maybeToV (Just a) _ = Success a

mapFailure :: (e -> e') -> Validation e a -> Validation e' a
mapFailure f (Failure e) = Failure $ f e
mapFailure _ (Success a) = Success a

vToAccV :: Validation e a -> AccValidation e a
vToAccV (Failure e) = AccFailure e
vToAccV (Success a) = AccSuccess a

vToAccVList :: Validation e a -> AccValidation [e] a
vToAccVList = vToAccV . mapFailure pure
