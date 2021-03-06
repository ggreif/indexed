{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module Indexed.Monoid
  ( IMonoid(..)
  , Cat(..)
  , Morphism(..)
  ) where

import Indexed.Types
import Data.Monoid

infixr 9 %
-- | Poly-kinded categories (should be in GHC 7.6.1 as Category)
class Cat k where
  idd :: k a a
  (%) :: k b c -> k a b -> k a c

instance Cat (==) where
  idd = Refl
  Refl % Refl = Refl

instance Cat (->) where
  idd x = x
  (%) f g x = f (g x)

instance Cat (:~>) where
  idd = Nat id
  Nat f % Nat g = Nat (f . g)

infixr 6 ><
-- | A Category
class IMonoid m where
  (><) :: m '(i,j) -> m '(j,k) -> m '(i,k)
  imempty :: m '(i,i)

-- | A category is just an indexed monoid.
newtype Morphism k i = Morphism (k (Fst i) (Snd i))

instance Cat k => IMonoid (Morphism k) where
  imempty = Morphism idd
  Morphism f >< Morphism g = Morphism (g % f)

instance Monoid m => Cat (At m) where
  idd = At mempty
  At m % At n = At (m <> n)

