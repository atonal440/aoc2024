{-# LANGUAGE PatternSynonyms #-}
module Lib where

import Data.Bifunctor ( Bifunctor(bimap) )
import Data.Vector ( Vector, (!?) )
import Linear ( V2(..) )

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

always, never :: a -> Bool
always = const True
never = const False

type Field a = Vector (Vector a)
type Point = V2 Int
{-# COMPLETE Point #-}
pattern Point :: Int -> Int -> V2 Int
pattern Point x y <- V2 x y where
  Point x y = V2 x y
xyLookup :: Field a -> Point -> Maybe a
xyLookup v2 (V2 x y) = (!? x) =<< (v2 !? y)
