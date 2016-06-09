import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "bowlingScore" $ do
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
        it "correctly scores multiplier for second 10th frame roll" $ do
            -- XXX: uh oh, spaghetti-os! This gets 290 somehow.
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
