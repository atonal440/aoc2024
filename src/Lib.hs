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

always, never :: a -> Bool
always = const True
never = const False

type Field a = Vector (Vector a)
type Point = (Int, Int)
xyLookup :: Field a -> Point -> Maybe a
xyLookup v2 (x,y) = (!? x) =<< (v2 !? y)
