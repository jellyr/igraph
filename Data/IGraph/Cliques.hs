-- Chapter 15. Cliques and Independent Vertex Sets
module Data.IGraph.Cliques
    ( cliques
    , maximalCliques
    ) where

import Data.IGraph.Internal
import Data.IGraph.Types
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (unless)

-- | 1.1. igraph_cliques — Find all or some cliques in a graph
cliques :: Graph d a -> (Int, Int) -> [[a]]
cliques g (min', max') = unsafePerformIO $ do
    vp <- newVectorPtr 0
    e <- withGraph g $ \gp -> 
         withVectorPtr vp $ \vpp -> 
         c_igraph_cliques gp vpp (fromIntegral min') (fromIntegral max')
    unless (e == 0) $ error "Cliques: error occurs, aborted!"
    vectorPtrToVertices g vp

foreign import ccall "igraph_cliques"
    c_igraph_cliques :: GraphPtr
                     -> VectorPtrPtr
                     -> CInt
                     -> CInt
                     -> IO CInt

-- | 1.3. igraph_maximal_cliques — Find all maximal cliques of a graph
maximalCliques :: Graph d a -> (Int, Int) -> [[a]]
maximalCliques g (min', max') = unsafePerformIO $ do
    vp <- newVectorPtr 0
    e <- withGraph g $ \gp -> 
         withVectorPtr vp $ \vpp -> 
         c_igraph_maximal_cliques gp vpp (fromIntegral min') (fromIntegral max')
    unless (e == 0) $ error "maximalCliques: error occurs, aborted!"
    vectorPtrToVertices g vp

foreign import ccall "igraph_maximal_cliques"
    c_igraph_maximal_cliques :: GraphPtr
                             -> VectorPtrPtr
                             -> CInt
                             -> CInt
                             -> IO CInt
