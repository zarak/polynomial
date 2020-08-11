module Lib where

import Data.Map
import Data.List

-- Types

newtype Exponent e =
    Exponent e
    deriving (Eq, Ord, Show)

newtype Coefficient c =
    Coefficient c
    deriving (Eq, Ord)

instance (Show c, Num c) => Show (Coefficient c) where
    show (Coefficient c) = (show c) ++ "x"

instance Functor Coefficient where
    fmap f (Coefficient a) = Coefficient (f a)

instance Applicative Coefficient where
    pure = Coefficient
    Coefficient f <*> Coefficient a = Coefficient (f a)

data Term e c =
    Term (Exponent e) (Coefficient c)
    deriving (Eq, Ord, Show)

data Polynomial = Polynomial [IntegerTerm]
    deriving (Eq, Show)

type IntegerTerm = Term Integer Integer



-- Functions
--
-- Version 1 using the Polynomial data type
derivative :: Polynomial -> Polynomial
derivative (Polynomial terms) = Polynomial $ fmap derivative' terms

derivative' :: IntegerTerm -> IntegerTerm
derivative' (Term (Exponent e) (Coefficient c)) =
    Term (Exponent (e - 1)) (Coefficient (e * c))

-- Version 2 using a list of coefficients
-- Assume the index represents the exponent n
listDeriv :: Num a => [Coefficient a] -> [Coefficient a]
listDeriv [] = []
listDeriv (x:xs) = listDeriv' xs 1

listDeriv' :: Num a => [Coefficient a] -> a -> [Coefficient a]
listDeriv' [] _ = []
listDeriv' (c:cs) n =
    ((*) <$> c <*> Coefficient n) : listDeriv' cs (n+1)


printp :: (Num a, Show a) => [Coefficient a] -> String
printp = intercalate " + " . reverse . zipWith (\exp coeff -> show coeff ++ "^" ++ show exp) [0..]


--view :: (Num a, Show a) => Int -> Coefficient a -> String
--view exp (Coefficient coeff) = 
    --case (exp, coeff) of
      --(0, c) -> [head . show $ coeff]
      --()
--
--printp :: [Coefficient a]
