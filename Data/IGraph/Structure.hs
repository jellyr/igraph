-- | Haskell bindings to the igraph C library.
-- Chapter 13. Structural Properties of Graphs
--

module Data.IGraph.Structure
    ( eigenvectorCentrality
    , kCore
    ) where

import Data.IGraph hiding (eigenvectorCentrality)
import Data.IGraph.Internal
import Data.IGraph.Types
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (unless)

-- | 6. Centrality Measures
-- | 6.13. igraph_eigenvector_centrality — Eigenvector centrality of the vertices
eigenvectorCentrality :: Graph d a -> Bool -> (Double, [(a, Double)])
eigenvectorCentrality g s = unsafePerformIO $ alloca $ \dp -> do
    v <- newVector 0
    e <- withGraph g $ \gp ->
         withVector v $ \vp ->
         withOptionalWeights g $ \wp ->
         withArpack g $ \ap ->
             c_igraph_eigenvector_centrality gp vp dp True s wp ap
    unless (e == 0) $ error "error"
    res <- peek dp
    lst <- vectorToList v
    return (realToFrac res, zip (nodes g) lst)


foreign import ccall "igraph_eigenvector_centrality"
    c_igraph_eigenvector_centrality :: GraphPtr
                                    -> VectorPtr
                                    -> Ptr CDouble
                                    -> Bool
                                    -> Bool
                                    -> VectorPtr
                                    -> ArpackPtr
                                    -> IO CInt

-- | 16. K-Cores
-- | 16.1. igraph_coreness — Finding the coreness of the vertices in a network.
kCore :: (Ord a, Show a) => Graph d a -> Int -> [[a]]
kCore g k = unsafePerformIO $ withGraph g $ \gp -> do
    v <- newVector 0
    withVector v $ \vp -> do
        e <- c_igraph_coreness gp vp (getNeiMode g)
        unless (e == 0) $ error "error"
    lst <- vectorToList v
    let vs = fst.unzip.filter ((>= fromIntegral k).snd) $ zip (nodes g) lst
        g' = inducedSubgraph g (VsList vs) CreateFromScratch
        cs = decompose g' Weak (-1) 1
    return $ map nodes cs

foreign import ccall "igraph_coreness"
    c_igraph_coreness :: GraphPtr -> VectorPtr -> CInt -> IO CInt
