module TDDSpec
    ( spec ) where

import Test.Hspec
import TDD

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "TDD bowling" $ do
        it "exists" $ do
            bowl [] `shouldBe` 0
