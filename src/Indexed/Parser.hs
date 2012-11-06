{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Indexed.Parser where


--import Indexed.Types
import Indexed.Functor
import Indexed.Thrist

-- | A binatural transformation from @f@ and @g@ to @h@
type (~~>) f g h = forall x. f x -> g x -> h x

type family Alt (t :: k) (u :: k) :: k

class IApplicative f => IAlternative f where
  --type Alt :: (i -> *) -> (i -> *) -> (i -> *)
  --(<|>) :: (f a ~~> f b) (f (Alt a b))
  --type Alt :: i -> i -> i
  (<|>) :: f a x -> f a y -> f a (Alt x y)



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
  Choice :: Parser p '(a, a') -> Parser p '(a, a'') -> Parser p '(a, Alt a' a'')

{- Okay, here is what I believe is at the heart of things:

   @[Char]@ is to @Parsec a@
as
   @Thrist (At Char)@ is to @IParser (At a)@

And we can compare the signatures now

runParsec :: Parsec a -> [Char] -> (Maybe a, [Char])

vs. (simplified)

runIParser :: IParser (At a) -> Thrist (At Char) -> (Maybe (At a), Thrist (At Char))

plus, we need to add the positional information

runIParser :: IParser (At a) (k, l) -> Thrist (At Char) (k, m) -> Maybe (At a (k, l), Thrist (At Char) (l, m))

-}


instance IFunctor Parser where
  imap _ PlusPlus = PlusPlus

instance IApplicative Parser where
  mf /*/ ma = undefined -- mf !>= \f -> ma !>= \a -> ireturnAt (f a)

instance IAlternative Parser where
  --type Alt a a = a
  -- l <|> r = Choice l r -- this does not typecheck


data family PAlt (t :: (N,N)) :: *

--alt :: Parser '(n, Alt (S' (S' n)) (S' n))
--alt :: Parser PAlt '(n, S' n)
--alt :: Parser PAlt '(n, Alt (S' (S' n)) (S' n))
--alt = PlusPlus <|> OneChar 'x'

-- Build a thrist indexed by a pair of naturals

data N = Z' | S' N | OR N N

data N' (n :: N) where
  Z :: N' Z'
  S :: N' n -> N' (S' n)

infixr 6 :&

data Interv :: (N, N) -> * where
  (:&) :: N' a -> N' b -> Interv '(a,b)

data AtInc :: * -> (N, N) -> * where
  AtInc :: a -> AtInc a '(k, S' k)
  AtPar :: AtInc a '(k, k') -> AtInc a '(k, k') -> AtInc a '(k, k')
  AtComb :: AtInc a '(k, k') -> AtInc a '(k, k'') -> AtInc a '(k, OR k' k'')

-- Tests

t0 :: Thrist Interv '(Z', Z')
t0 = Z:&Z :- Z:&Z :- Nil

t1 :: Thrist Interv '(Z', S' Z')
t1 = Z:&Z :- Z:&S Z :- Nil

-- counting Char Thrist

type One = S' Z'
type Two = S' One
type Three = S' Two
type Four = S' Three
type Five = S' Four
type Six = S' Five
type Seven = S' Six

t2 :: Thrist (AtInc Char) '(Z', Six)
t2 = AtInc 'H' :- AtInc 'e' :- AtInc 'l' :- AtInc 'l' :- AtInc 'o' :- AtInc '!' :- Nil
