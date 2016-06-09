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
