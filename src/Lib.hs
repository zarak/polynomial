module Lib where

import Data.Map


newtype Exponent e = Exponent e
    deriving (Eq, Ord, Show)

newtype Coefficient c = Coefficient c
    deriving (Eq, Ord, Show)

instance Functor Coefficient where
    fmap f (Coefficient a) = Coefficient (f a)

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


-- Assume the index represents the exponent n
listDeriv :: Num a => [Coefficient a] -> [Coefficient a]
listDeriv [] = []
listDeriv (x:xs) = auxDeriv xs 1


auxDeriv :: Num a => [Coefficient a] -> a -> [Coefficient a]
auxDeriv [] _ = []
auxDeriv (c:cs) n =
    case c of
      Coefficient x -> Coefficient (x * n) : auxDeriv cs (n+1)
