{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
--data IResult f r = forall x. Fail (f x) [String] String
--                 -- | Partial (f ~> IResult f r)
--                 | forall x. Done (f x) r

data R f r x = Fail' (f x) [String] String
             | Partial (f x -> R f r x)
             | Done' (f x) r
type IResult f r = forall x. R f r x


newtype Input f = I {unI :: forall x. f x}
newtype Added f = A {unA :: forall x. f x}
data More = Complete | Incomplete deriving (Eq, Show)

--instance IFunctor IResult
--instance IFunctor Parser

-- Build a thrist indexed by a pair of naturals

data N = Z' | S' N

data N' (n :: N) where
  Z :: N' Z'
  S :: N' n -> N' (S' n)

data Interv :: (i, i) -> * where
  P :: N' a -> N' b -> Interv '(a,b)

{- I get:

src/Indexed/Parser.hs:61:3:
    Data constructor `P' cannot be GADT-like in its *kind* arguments
      P :: forall (a :: N) (b :: N).
           N' a -> N' b -> Interv N ('(,) N N a a)
    In the definition of data constructor `P'
    In the data declaration for `Interv'

-}

t1 :: Thrist Interv '(Z', Z')
t1 = P Z Z :- P Z Z :- Nil
