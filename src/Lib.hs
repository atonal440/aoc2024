module Lib where

import Data.Bifunctor ( Bifunctor(bimap) )

type Dispatch = String -> String -> String

dispatchWith :: (Show a) => (String -> i) -> (i -> a) -> (i -> a) -> Dispatch
dispatchWith parse process1 process2 part = show . process . parse
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