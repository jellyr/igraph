-- | Haskell bindings to the igraph C library.
-- Chapter 13. Structural Properties of Graphs
--

module Data.IGraph.Structure
    ( eigenvectorCentrality
    ) where

import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (unless)

-- | 1.2. igraph_community_optimal_modularity — Calculate the community structure with the highest modularity value
--
eigenvectorCentrality :: Graph d a -> Bool -> (Double, [(a, Double)])
eigenvectorCentrality g s = unsafePerformIO $ alloca $ \dp -> do
    v <- newVector 0
    e <- withGraph g $ \gp ->
         withVector v $ \vp ->
         withOptionalWeights g $ \wp ->
         withArpack g $ \ap ->
             c_igraph_eigenvector_centrality gp vp dp True s wp ap
    unless (e /= 1) $ error "error"
    res <- peek dp
    lis <- vectorToList v
    return (realToFrac res, zip (nodes g) lis)


foreign import ccall "igraph_eigenvector_centrality"
    c_igraph_eigenvector_centrality :: GraphPtr
                                    -> VectorPtr
                                    -> Ptr CDouble
                                    -> Bool
                                    -> Bool
                                    -> VectorPtr
                                    -> ArpackPtr
                                    -> IO CInt
