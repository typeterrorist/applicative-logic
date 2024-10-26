module Control.Applicative.Logic.Modal where

import Prelude hiding (all,any,or,and)
import Control.Applicative.Logic
import Control.Applicative

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

type Modal b a = (a -> b) -> (a -> b) 

modal :: (Functor t)
      => Algebra t b
      -> CoAlgebra t a
      -> Modal b a
modal mu nu predicate = mu  . (predicate <$>) . nu

necessarily :: (Applicative f, Foldable t, Monoid b)
    => CoAlgebra t a
    -> Modal (f b) a
necessarily nu predicate = all predicate . nu

□ = necessarily

possibly :: (Alternative f, Foldable t)
        => CoAlgebra t a
        -> Modal (f b) a
possibly nu predicate = any predicate . nu

◇ = possibly

