module LList where

data LList a = Head a | Tail (LList a) deriving Show

instance Functor LList where
    fmap f (Head a) = Head (f a)
    fmap f (Tail a) = Tail (fmap f a)