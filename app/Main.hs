module Main where

import Lib

main :: IO ()
main = do
    let p1 = Polynomial [ Term (Coefficient 10) (Exponent 1)
                        , Term (Coefficient 12) (Exponent 2)
                        , Term (Coefficient 0) (Exponent 3)]
    print $ derivative p1
