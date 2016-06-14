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
        it "doesn't erroneously score non-spare" $ do
            bowl $ [0, 5, 5, 3] ++ rollMany 16 0
            `shouldBe` 13
        it "scores a strike" $ do
            bowl $ [10, 5, 1] ++ rollMany 16 0
            `shouldBe` 22
        it "scores several strikes" $ do
            bowl $ [10, 10, 10, 2, 1] ++ rollMany 12 0
            `shouldBe` 68
        it "scores a perfect game" $ do
            bowl $ rollMany 12 10
            `shouldBe` 300
        it "tallies 100 for a gutter-spare game" $ do
            bowl [ 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0, 10
                 , 0
                 ] `shouldBe` 100
