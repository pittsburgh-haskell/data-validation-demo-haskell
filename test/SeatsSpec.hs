module SeatsSpec (main, spec) where

import Test.Hspec

import qualified Seats
import Data.Either.Validation

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Failing" $ do
    it "No zero seats" $ do
      Seats.make 0 `shouldBe`
        Left (Seats.BadCount 0)
