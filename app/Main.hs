module Main where

import Lib

main :: IO ()
main = do
    let p1 = Polynomial [ Term (Exponent 1) (Coefficient 10)
                        , Term (Exponent 2) (Coefficient 12)
                        , Term (Exponent 3) (Coefficient 0)]
    print $ derivative p1
