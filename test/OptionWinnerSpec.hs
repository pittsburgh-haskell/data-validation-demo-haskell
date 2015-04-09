module OptionWinnerSpec (main, spec) where

import Test.Hspec

import qualified OptionWinner

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Failing" $ do
    it "A has no winner" $ do
      OptionWinner.winner OptionWinner.personA `shouldBe` Nothing
