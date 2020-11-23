{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Algebra where

import Data.Foldable

class Monoid a => Group a where
  inv :: a -> a
  -- Laws:
  -- a <> inv a == mempty
  -- inv a <> a == mempty

pow :: Monoid a => Int -> a -> a
pow i = fold . take i . repeat

powInv :: Group a => Int -> a -> a
powInv i = fold . fmap inv . take i . repeat

newtype Sum = Sum { unSum :: Int }
  deriving stock Show
  deriving newtype Num

instance Semigroup Sum where
  (<>) (Sum a) (Sum b) = Sum $ a + b

instance Monoid Sum where
  mempty = Sum 0

instance Group Sum where
  inv = negate

-- $> inv (Sum 3) <> Sum 3
--
-- $> Sum 3 <> inv (Sum 3)
--
-- $> mempty :: Sum

newtype Product = Product { unProduct :: Float }
  deriving stock Show
  deriving newtype Num

instance Semigroup Product where
  (<>) (Product a) (Product b) = Product $ a * b

instance Monoid Product where
  mempty = Product 1

instance Group Product where
  inv (Product a) = Product $ a ^^ (-1 :: Integer)

-- $> inv (Product 3)
--
-- $> inv (Product 3) <> Product 3
--
-- $> Product 3 <> inv (Product 3)
--
-- $> mempty :: Product
