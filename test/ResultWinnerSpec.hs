module ResultWinnerSpec (main, spec) where

import Test.Hspec

import qualified ResultWinner

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Failing" $ do
    it "A has no winner because C no youngest child" $ do
      ResultWinner.winner ResultWinner.personA `shouldBe`
        Left (ResultWinner.MyError "C has no youngest child")
