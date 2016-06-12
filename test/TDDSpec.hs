module TDDSpec
    ( spec ) where

import Test.Hspec
import TDD

main :: IO ()
main = hspec spec

rollMany :: Int -> Int -> [Int]
rollMany count pins = take count $ repeat pins

spec :: Spec
spec = do
    describe "TDD bowling" $ do
        it "exists" $ do
            bowl [] `shouldBe` 0
        it "scores 0 on a gutter game" $ do
            bowl $ rollMany 20 0
            `shouldBe` 0
        it "scores 20 on all ones" $ do
            bowl $ rollMany 20 1
            `shouldBe` 20
