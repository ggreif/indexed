{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Indexed.Parser where


--import Indexed.Types
import Indexed.Functor
import Indexed.Thrist

-- | A binatural transformation from @f@ and @g@ to @h@
type (~~>) f g h = forall x. f x -> g x -> h x

class IApplicative f => IAlternative f where
  (<|>) :: (f a ~~> f a) (f a)

-- class IAlternative f => IParser f where
--   (<|>) :: (f a ~~> f a) (f a)


-- this comes from attoparsec
newtype Parser f a = Parser {
  runParser :: forall r. Input f -> Added f -> More
                                 -> Failure f   r
                                 -> Success f a r
                                 -> IResult f   r
}

type Failure f   r = Input f -> Added f -> More -> [String] -> String -> IResult f r
type Success f a r = Input f -> Added f -> More -> a -> IResult f r
data IResult f r = forall x. Fail (f x) [String] String
                 -- | Partial (f ~> IResult f r)
                 | forall x. Done (f x) r

newtype Input f = I {unI :: forall x. f x}
newtype Added f = A {unA :: forall x. f x}
data More = Complete | Incomplete deriving (Eq, Show)

--instance IFunctor
