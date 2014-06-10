module Data.IGraph.Basic
    ( vCount
    , eCount
    ) where

import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (unless)

-- | 2. The basic interface
-- | 2.2. Basic Query Operations
-- | 2.2.1. igraph_vcount — The number of vertices in a graph.
vCount :: Graph d a -> Int
vCount g = fromIntegral.unsafePerformIO $ withGraph g $ \gp ->
    c_igraph_vcount gp

foreign import ccall "igraph_vcount"
    c_igraph_vcount :: GraphPtr -> IO CInt

-- | 2.2.2. igraph_ecount — The number of edges in a graph.
eCount :: Graph d a -> Int
eCount g = fromIntegral.unsafePerformIO $ withGraph g $ \gp ->
    c_igraph_ecount gp

foreign import ccall "igraph_ecount"
    c_igraph_ecount :: GraphPtr -> IO CInt
