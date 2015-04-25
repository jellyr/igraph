-- | Haskell bindings to the igraph C library.
-- Chapter 13. Structural Properties of Graphs
--

module Data.IGraph.Structure
    ( closeness
    , betweenness
    , eigenvectorCentrality
    , kCore
    ) where

import Data.IGraph hiding (eigenvectorCentrality, closeness, betweenness)
import Data.IGraph.Internal
import Data.IGraph.Types
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (unless)

-- | 6. Centrality Measures
-- | 6.1. igraph_closeness — Closeness centrality calculations for some vertices.
--
-- The closeness centrality of a vertex measures how easily other vertices can
-- be reached from it (or the other way: how easily it can be reached from the
-- other vertices). It is defined as the number of the number of vertices minus
-- one divided by the sum of the lengths of all geodesics from/to the given
-- vertex.
--
-- If the graph is not connected, and there is no path between two vertices, the
-- number of vertices is used instead the length of the geodesic. This is always
-- longer than the longest possible geodesic.
closeness :: Ord a => Graph d a -> VertexSelector a -> [(a,Double)]
closeness g vs = unsafePerformIO $ do
  v  <- newVector 0
  _e <- withGraph g $ \gp ->
        withOptionalWeights g $ \wp ->
        withVs vs g $ \vsp ->
        withVector v $ \vp ->
          c_igraph_closeness
            gp
            vp
            vsp
            (getNeiMode g)
            wp
  scores <- vectorToList v
  return $ zip (selectedVertices g vs) scores

foreign import ccall "closeness"
  c_igraph_closeness :: GraphPtr -> VectorPtr -> VsPtr -> CInt -> VectorPtr -> IO CInt

-- | 6.2. igraph_betweenness — Betweenness centrality of some vertices.
--
-- The betweenness centrality of a vertex is the number of geodesics going
-- through it. If there are more than one geodesic between two vertices, the
-- value of these geodesics are weighted by one over the number of geodesics.
betweenness :: Graph d a -> VertexSelector a -> [(a, Double)]
betweenness g vs = unsafePerformIO $ do
  v  <- newVector 0
  _e <- withGraph g $ \gp ->
        withOptionalWeights g $ \wp ->
        withVs vs g $ \vsp ->
        withVector v $ \vp ->
          c_igraph_betweenness
            gp
            vp
            vsp
            True -- should be OK for all graphs
            wp
            True -- should be OK for all graphs
  scores <- vectorToList v
  return $ zip (selectedVertices g vs) scores

foreign import ccall "betweenness"
  c_igraph_betweenness :: GraphPtr -> VectorPtr -> VsPtr -> Bool -> VectorPtr -> Bool -> IO CInt

-- | 6.13. igraph_eigenvector_centrality — Eigenvector centrality of the vertices
eigenvectorCentrality :: Graph d a
                      -> Bool  -- ^ if True, the values will be scaled such that
                               -- the largest value is 1
                      -> (Double, [(a, Double)])
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
