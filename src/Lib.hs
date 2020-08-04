module Lib where

import Data.Map


newtype Exponent e = Exponent e
    deriving (Eq, Ord, Show)

newtype Coefficient c = Coefficient c
    deriving (Eq, Ord, Show)

data Term e c = Term (Exponent e) (Coefficient c)
          deriving (Eq, Ord, Show)

type IntegerTerm = Term Integer Integer

data Polynomial = Polynomial [IntegerTerm]
    deriving (Eq, Show)


derivative :: Polynomial -> Polynomial
derivative (Polynomial terms) = Polynomial $ fmap derivative' terms


derivative' :: IntegerTerm -> IntegerTerm
derivative' (Term (Exponent e) (Coefficient c)) =
    Term (Exponent (e - 1)) (Coefficient (e * c))
