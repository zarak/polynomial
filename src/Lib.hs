module Lib where

import Data.Map


newtype Exponent e = Exponent e
    deriving (Eq, Ord, Show)

newtype Coefficient c = Coefficient c
    deriving (Eq, Ord, Show)

data Term c e = Term (Coefficient c) (Exponent e)
          deriving (Eq, Ord, Show)

type IntegerTerm = Term Integer Integer

data Polynomial = Polynomial [IntegerTerm]
    deriving (Eq, Show)


derivative :: Polynomial -> Polynomial
derivative (Polynomial terms) = Polynomial $ fmap derivative' terms


derivative' :: IntegerTerm -> IntegerTerm
derivative' (Term (Coefficient c) (Exponent e)) =
    Term (Coefficient (e * c)) (Exponent (e - 1))
