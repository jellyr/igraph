module Data.IGraph.Parser where

import qualified Data.Vector as V
import Data.IGraph

readMatrix :: E d String => FilePath -> Bool -> IO (Graph d String)
readMatrix fl header = do 
    content <- readFile fl
    let (h:m) | header = map words.lines $ content
              | otherwise = map show [1..(length.lines) content] : (map words.lines) content
        nm = V.fromList h
        adjMatrix = (map.map) read m :: [[Double]]
    return.fromList $ getEdges nm adjMatrix
  where
    getEdges name m = map f.filter ((/=0).fst).zip (concat m) $ [(i,j) | i <- [0..l-1], j <- [0..l-1]]
      where
        f (_, (i,j)) = (name V.! i, name V.! j)
        l = V.length name
