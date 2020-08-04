import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec spec

t1 = Term (Coefficient 1) (Exponent 2)
p1 = Polynomial []
p2 = Polynomial [t1]
p3 = Polynomial [t1, t1]

spec :: Spec
spec =
    describe "derivative" $ do
        it "should return an empty list for a null result" $
            derivative p1 `shouldBe` Polynomial []
        it "should differentiate a list with one term" $
            derivative p2 `shouldBe` Polynomial [(Coefficient 2, Exponent 1)]
        it "should differentiate a list with two terms" $
            derivative p3 `shouldBe` Polynomial [(Coefficient 2,  Exponent 1), (Coefficient 2,  Exponent 1)]
