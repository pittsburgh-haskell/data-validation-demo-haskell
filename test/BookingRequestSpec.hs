module BookingRequestSpec (main, spec) where

import Test.Hspec

import qualified BookingRequest
import qualified Date
import qualified Seats

import Data.Either.Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Failing" $ do
    it "junk" $ do
      let now = Date.Date 2
      BookingRequest.make now (Just "1") (Just (-3)) `shouldSatisfy`
        (== Failure [ BookingRequest.DateBefore (Date.Date 1) (Date.Date 2)
                    , BookingRequest.SeatsError (Seats.BadCount (-3))
                    ]
        )
  describe "Succeeding" $ do
    it "All good" $ do
      let now = Date.Date 2
      BookingRequest.make now (Just "3") (Just 5) `shouldSatisfy`
        isSuccess

isSuccess :: Validation e a -> Bool
isSuccess (Success _) = True
isSuccess _ = False
