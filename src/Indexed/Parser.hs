{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Indexed.Parser where


--import Indexed.Types
import Indexed.Functor

-- | A binatural transformation from @f@ and @g@ to @h@
type (~~>) f g h = forall x. f x -> g x -> h x

class IApplicative f => IAlternative f where
  (<|>) :: (f a ~~> f a) (f a)

-- class IAlternative f => IParser f where
--   (<|>) :: (f a ~~> f a) (f a)
