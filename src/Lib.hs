module Lib where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Vector ( Vector, (!?) )

type Dispatch = String -> String -> String

dispatchWith :: (Show a) => (String -> a) -> (String -> a) -> Dispatch
dispatchWith process1 process2 part = show . process
  where
  process = case part of
    "a" -> process1
    "b" -> process2
    unk -> error $ "unknown part " <> show unk

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

pass, fail :: a -> Bool
pass = const True
fail = const False

v2lookup :: Vector (Vector a) -> Int -> Int -> Maybe a
v2lookup v2 x y = (!? x) =<< (v2 !? y)
