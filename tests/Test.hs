import Test.Hspec
import Wolfram

main :: IO ()
main = hspec $ do
    describe "Parssing" $ do
        it "test string is number" $ do
            isStringDigit "123" `shouldBe` True
        it "other dummy test" $ do
            isStringDigit "123a" `shouldBe` False
        it "other dummy test" $ do
            isStringDigit "-123" `shouldBe` True
        it "check negativ rules" $ do
            parsing ["--rule", "-10"] (0, 0, -1, 80, 0, 0) `shouldBe` (0 :: Int, 0 :: Int, -1 :: Int, 80 :: Int, 0 :: Int, 84 :: Int)
        it "check rule 256" $ do
            parsing ["--rule", "-10"] (0, 0, -1, 80, 0, 0) `shouldBe` (0 :: Int, 0 :: Int, -1 :: Int, 80 :: Int, 0 :: Int, 84 :: Int)
        it "check negativ lines" $ do
            parsing ["--lines", "-10"] (0, 0, -1, 80, 0, 0) `shouldBe` (0 :: Int, 0 :: Int, -1 :: Int, 80 :: Int, 0 :: Int, 84 :: Int)
        it "check with good argument" $ do
            parsing ["--rule", "30", "--start", "10", "--lines", "30"] (0, 0, -1, 80, 0, 0) `shouldBe` (30 :: Int, 10 :: Int, 30 :: Int, 80 :: Int, 0 :: Int, 0 :: Int)
