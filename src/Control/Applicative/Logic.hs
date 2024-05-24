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
-- any :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
-- @
any :: (Alternative f, Foldable t) => (a -> f b) -> t a -> f b
any = ($ false) . foldr . ((<|>) .)

-- | Generalized version of 'Prelude.all'. It takes a predicate that gives generalized
-- truth values in an Applicative functor on a Monoid and applies it conjunctively to
-- a foldable structure. I.e. it applies the predicate and folds with an applicative
-- lifting of monoidal concatenation ('<>').
-- 
-- @
-- all :: (Applicative f, Monoid b, Foldable t) => (a -> f b) -> t a -> f b
-- @
--
all :: (Applicative f, Monoid b, Foldable t)
    => (a -> f b) -> t a -> f b
all = ($ true) . foldr . (liftA2 (<>) <$>)


-- | Generalized version of the boolean 'or' to foldable structures of 'Alternative' functorial values.
-- It combines the elements using the alternative choice operator ('<|>').
--
-- @
-- or :: (Alternative f, Foldable t) => t (f a) -> f a
-- @
or :: (Alternative f, Foldable t)
   => t (f a) -> f a
or = any id

-- | Generalized version of the boolean 'and' to foldable structures of 'Applicative' functor applied to monoids. It combines the elements using the monoidal concatenation ('<>').
--
-- @
-- and :: (Applicative f, Monoid a, Foldable t) => t (f a) -> f a
-- @
and :: (Applicative f, Monoid a, Foldable t)
    => t (f a) -> f a
and = all id

(&&) :: (Applicative f, Monoid a)
     => f a -> f a -> f a
(&&) = liftA2 (<>)

-- | Converts a foldable structure into an 'Alternative' functor, where each element is lifted into the
-- functor using 'pure' and then combined using the alternative choice operator ('<|>').
--
-- @
-- convert :: (Alternative f, Foldable t) => t a -> f a
-- @
convert :: (Alternative f, Foldable t)
     => t a -> f a
convert = any pure

class (Foldable t) => Searchable t where
    search :: (Alternative f, Monoid b) => (a -> f b) -> t a -> f b

instance Searchable Maybe where
    search = any


