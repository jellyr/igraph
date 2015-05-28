module Data.IGraph.Parser where

import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import Data.IGraph

readMatrix :: E d B.ByteString => FilePath -> Bool -> IO (Graph d B.ByteString)
readMatrix fl header = B.readFile fl >>= return . flip fromMatrix header

fromMatrix :: E d B.ByteString => B.ByteString -> Bool -> Graph d B.ByteString
fromMatrix s header = 
    let xs = map B.words . B.lines $ s
        (h:m) = if header
                   then xs
                   else map (B.pack . show) [1..length xs] : xs
        nm = V.fromList h
        adjMatrix = (map.map) (read . B.unpack) m :: [[Double]]
    in fromList $ getEdges nm adjMatrix
  where
    getEdges name m = map f.filter ((/=0).fst).zip (concat m) $ [(i,j) | i <- [0..l-1], j <- [0..l-1]]
      where
        f (_, (i,j)) = (name V.! i, name V.! j)
        l = V.length name
{-# INLINE fromMatrix #-}

readMatrixWeighted :: (E d B.ByteString, IsUnweighted d) => FilePath -> Bool -> IO (Graph (Weighted d) B.ByteString)
readMatrixWeighted fl header = B.readFile fl >>= return . flip fromMatrixWeighted header

fromMatrixWeighted :: (E d B.ByteString, IsUnweighted d) => B.ByteString -> Bool -> Graph (Weighted d) B.ByteString
fromMatrixWeighted s header = 
    let xs = map B.words . B.lines $ s
        (h:m) = if header
                   then xs
                   else map (B.pack . show) [1..length xs] : xs
        nm = V.fromList h
        adjMatrix = (map.map) (read . B.unpack) m :: [[Double]]
    in fromListWeighted $ getEdges nm adjMatrix
  where
    getEdges name m = map f.filter ((/=0).fst).zip (concat m) $ [(i,j) | i <- [0..l-1], j <- [0..l-1]]
      where
        f (w, (i,j)) = (name V.! i, name V.! j, truncate $ w * 10000)
        l = V.length name
{-# INLINE fromMatrixWeighted #-}
