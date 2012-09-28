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


-- this basically comes from attoparsec
{-
newtype Parser f a = Parser {
  runParser :: forall r. Input f -> Added f -> More
                                 -> Failure f   r
                                 -> Success f a r
                                 -> IResult f   r
}

type Failure f   r = Input f -> Added f -> More -> [String] -> String -> IResult f r
type Success f a r = Input f -> Added f -> More -> a -> IResult f r

data R f r x = Fail (f x) [String] String
             | Partial (f x -> R f r x)
             | Done (f x) r
type IResult f r = forall x. R f r x


newtype Input f = I {unI :: forall x. f x}
newtype Added f = A {unA :: forall x. f x}
data More = Complete | Incomplete deriving (Eq, Show)

instance IFunctor IResult where
  imap f (Fail x es e) = Fail (f x) es e

--instance IFunctor Parser
-}

-- understand the concept first
--
data Parser :: ((N,N) -> *) -> (N,N) -> * where
  OneChar :: Char -> Parser p '(a,S' a)
  PlusPlus :: Parser p '(a,S' (S' a))

-- Build a thrist indexed by a pair of naturals

data N = Z' | S' N

data N' (n :: N) where
  Z :: N' Z'
  S :: N' n -> N' (S' n)

infixr 6 :&

data Interv :: (N, N) -> * where
  (:&) :: N' a -> N' b -> Interv '(a,b)

data AtInc :: * -> (N, N) -> * where
  AtInc :: a -> AtInc a '(k, S' k)

-- Tests

t0 :: Thrist Interv '(Z', Z')
t0 = Z:&Z :- Z:&Z :- Nil

t1 :: Thrist Interv '(Z', S' Z')
t1 = Z:&Z :- Z:&S Z :- Nil

-- counting Char Thrist

t2 :: Thrist (AtInc Char) '(Z', S' (S' (S' (S' (S' (S' Z'))))))
t2 = AtInc 'H' :- AtInc 'e' :- AtInc 'l' :- AtInc 'l' :- AtInc 'o' :- AtInc '!' :- Nil
