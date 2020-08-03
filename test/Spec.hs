module Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec spec

p1 = Polynomial []

spec :: Spec
spec =
    describe "Polynomial" $ do
        it "should return an empty list for a null result" $
            derivative p1 `shouldBe` Polynomial []
