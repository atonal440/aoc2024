module Lib.Field
( module Lib.Field
, Point, pattern Point, px, py
) where

import Lib ( both, vec )
import Lib.Point ( Point, pattern Point, px, py, bounded )
import Control.Arrow ( (&&&) )
import qualified Data.Map as Map
import Data.Vector ( Vector, (!), (!?), (//) )
import Data.Vector qualified as Vec

type Field a = Vector (Vector a)

lookup :: Point -> Field a -> Maybe a
lookup Point{..} field = (!? px) =<< field !? py

update :: Point -> a -> Field a -> Field a
update Point{..} x v = v // [(py, v ! py // [(px, x)])]

updates :: [(Point, a)] -> Field a -> Field a
updates xs field = Vec.imap updateRow field
  where
  updateRow ix row = maybe row (row //) $ Map.lookup ix rowUpdates
  rowUpdates = Map.fromListWith (<>) $ extractRow <$> xs
  extractRow (Point{..}, x) = (py, [(px, x)])

elemIndices :: (Show a, Eq a) => a -> Field a -> Vector Point
elemIndices x field = do
  (colIx, row) <- Vec.imap (,) field
  rowIx <- Vec.elemIndices x row
  pure $ Point rowIx colIx

within :: Point -> Field a -> Bool
within point field = bounded point (extent field)

extent :: Field a -> Point
extent = uncurry Point . both pred . (minimum . fmap Vec.length &&& Vec.length)

parse :: (Char -> a) -> String -> Field a
parse parseChar = vec . fmap (vec . fmap parseChar) . lines

indices :: Field a -> [Point]
indices field = let Point{..} = extent field in
  [Point x y | x <- [0..px], y <- [0..py]]

replicate :: Point -> a -> Field a
replicate Point{..} = Vec.replicate py . Vec.replicate px
