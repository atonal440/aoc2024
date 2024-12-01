module Lib where

import Data.Bifunctor ( Bifunctor(bimap) )

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f
