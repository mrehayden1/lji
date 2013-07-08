module Data.Pointed (
  Pointed(..)
, fromList
) where

import Data.Monoid

class Pointed p where
  pure :: a -> p a

instance Pointed [] where
  pure = (:[])

fromList :: (Pointed l, Monoid (l a)) => [a] -> l a
fromList = mconcat . fmap pure
