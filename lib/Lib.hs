module Lib where

import Data.Bifunctor

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f
