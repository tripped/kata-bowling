module TDDSpec
    ( spec ) where

import Test.Hspec
import TDD

main :: IO ()
main = hspec spec

rollMany :: Int -> Int -> [Int]
rollMany = (. repeat) . take

spec :: Spec
spec = do
    describe "TDD bowling" $ do
        it "scores 0 on a gutter game" $ do
            bowl $ rollMany 20 0
            `shouldBe` 0
        it "scores 20 on all ones" $ do
            bowl $ rollMany 20 1
            `shouldBe` 20
        it "scores a spare" $ do
            bowl $ [5, 5, 3] ++ rollMany 17 0
            `shouldBe` 16
