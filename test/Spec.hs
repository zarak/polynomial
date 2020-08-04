import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec spec

t1 = Term (Exponent 2) (Coefficient 1)
t2 = Term (Exponent 1) (Coefficient 2)
p1 = Polynomial []
p2 = Polynomial [t1]
p3 = Polynomial [t1, t1]
p4 = Polynomial [t2]
p5 = Polynomial [t2, t2]

spec :: Spec
spec =
    context "Lib" $ do
        describe "derivative" $ do
            it "should return an empty list for a null result" $
                derivative p1 `shouldBe` Polynomial []
            it "should differentiate a list with one term" $
                derivative p2 `shouldBe` p4 
            it "should differentiate a list with two terms" $
                derivative p3 `shouldBe` p5 
        describe "listDeriv" $ do
            it "should return an empty list for a constant" $
                listDeriv [Coefficient 10] `shouldBe` []
            it "should differentiate a list with one term with degree greater than 0" $
                listDeriv (Coefficient <$> [0, 2]) `shouldBe` Coefficient <$> [2]
            it "should differentiate a degree 3 polynomial" $
                listDeriv (Coefficient <$> [1, 2, 3, 4]) `shouldBe` Coefficient <$> [2, 6, 12]
