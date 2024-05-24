{-|
Module      : Control.Applicative.Logic
Description : Generalized logic operations for Applicative and Alternative functors
Copyright   : (c) Håkon Robbestad Gylterud, Year
License     : BSD-3-Clause
Maintainer  : lyesveasp@openbastille.org
Stability   : experimental
Portability : POSIX

This library contains a generalisation of local functions such as "any" and "all" to Applicative and Alternative functors.
-}
module Control.Applicative.Logic (any,or,all,and,(&&),convert,Searchable,search) where

import Control.Applicative
import Prelude hiding (any,all,and,or,(&&))

true :: (Applicative f, Monoid a) => f a
true = pure mempty

false :: (Alternative f) => f a
false = empty

-- | Generalized version of 'Prelude.any'. It takes a predicate that returns 
-- generalised truth values in an 'Alternative' functor and applies it disjunctively to
-- a foldable structure. I.e. it applies the predicate and folds with <|>.

any :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
any = ($ false) . foldr . ((<|>) .)

-- | Generalized version of 'Prelude.all'. It takes a predicate that gives generalized
-- truth values in an Applicative functor on a Monoid and applies it conjunctively to
-- a foldable structure. I.e. it applies the predicate and folds with an applicative
-- lifting of monoidal concatenation ('<>').

all :: (Applicative f, Monoid b, Foldable t)
    => (a -> f b) -> t a -> f b
all = ($ true) . foldr . (liftA2 (<>) <$>)


-- | Generalized version of the boolean 'or' to foldable structures of 'Alternative' functorial values.
-- It combines the elements using the alternative choice operator ('<|>').

or :: (Alternative f, Foldable t)
   => t (f a) -> f a
or = any id

-- | Generalized version of the boolean 'and' to foldable structures of
-- 'Applicative' functor applied to monoids. It combines the elements using the
-- monoidal concatenation ('<>').

and :: (Applicative f, Monoid a, Foldable t)
    => t (f a) -> f a
and = all id

-- | Generalized version of the boolean '&&' operator for 'Applicative' functors applied to monoids.
--   It sequences and combines two applicative values using the monoidal operation ('<>').
(&&) :: (Applicative f, Monoid a)
     => f a -> f a -> f a
(&&) = liftA2 (<>)

-- | Converts a foldable structure into an 'Alternative' functor, where each element is lifted into the
-- functor using 'pure' and then combined using the alternative choice operator ('<|>').
--
convert :: (Alternative f, Foldable t)
     => t a -> f a
convert = any pure

-- | 'Searchable' class represents structures that can be searched using a predicate.
-- An instance may use 'all' or 'any' to iterate over parts of its structure depending
-- on wether the substrcutres are conjunctive or disjunctive.
class (Foldable t) => Searchable t where
    -- | Search a structure using a predicate
    search :: (Alternative f, Monoid b) => (a -> f b) -> t a -> f b

-- | 'Maybe' instance of 'Searchable'.
instance Searchable Maybe where
    search = any


