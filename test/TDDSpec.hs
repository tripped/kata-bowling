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
        it "scores 0 on a gutter game" $ do
            bowl $ take 20 $ repeat 0
            `shouldBe` 0
        it "scores 20 on all ones" $ do
            bowl $ take 20 $ repeat 1
            `shouldBe` 20
