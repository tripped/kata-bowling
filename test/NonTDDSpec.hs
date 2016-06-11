module NonTDDSpec
    ( spec ) where

import Test.Hspec
import NonTDD

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "non-TDD bowlingScore" $ do
        it "tallies 0 for a crap game" $ do
            bowlingScore $ take 10 $ repeat 0
            `shouldBe` 0
        it "tallies 100 for a gutter-spare game" $ do
            bowlingScore [ 0, 10
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
        it "tallies 300 for 12 strikes" $ do
            bowlingScore [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
            `shouldBe` 300
        it "tallies 299 for 11 strikes and one moment of regret" $ do
            bowlingScore [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 9]
            `shouldBe` 299
        it "correctly scores multiplier after 10th frame spare" $ do
            bowlingScore [0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 9, 1, 10] `shouldBe` 20
        it "correctly scores multiplier after 10th frame strike" $ do
            bowlingScore [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 0, 10]
            --          ∑ 30  30  30  30  30  30  30  30  20  10  0  10 = 280
            `shouldBe` 280
        it "tallies 190 for repeated 9/1 spare frames" $ do
            bowlingScore [ 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9, 1
                         , 9
                         ] `shouldBe` 190
