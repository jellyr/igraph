-- | Haskell bindings to the igraph C library.
-- Chapter 18. Generating Layouts for Graph
--

module Data.IGraph.Layout
    ( layoutFruchtermanReingold
    , layoutLgl
    ) where

import Data.IGraph.Internal
import Data.IGraph.Types
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad

helper :: Monad m => Graph d a -> [[Double]] -> m [(a, (Double, Double))]
{-# INLINE helper #-}
helper g = return.map ( \(i,[x,y]) -> (idToNode'' g i, (x,y)) ).zip [0..] 

-- | 1. 2D layout generators
--
-- | 1.8. igraph_layout_fruchterman_reingold — Places the vertices on a plane according to the Fruchterman-Reingold algorithm.
layoutFruchtermanReingold :: Graph d a
                          -> Int
                          -> Double
                          -> Double
                          -> Double
                          -> Double
                          -> [(a, (Double, Double))]
layoutFruchtermanReingold g niter maxdelta area coolexp repulserad =
    unsafePerformIO $ withGraph g $ \gp -> do
        res <- newMatrix 0 0
        withMatrix res $ \mp -> do
            e <- c_igraph_layout_fruchterman_reingold gp
                                                      mp
                                                      (fromIntegral niter)
                                                      (realToFrac maxdelta)
                                                      (realToFrac area)
                                                      (realToFrac coolexp)
                                                      (realToFrac repulserad)
                                                      0
                                                      nullPtr
                                                      nullPtr
                                                      nullPtr
                                                      nullPtr
                                                      nullPtr
            unless (e == 0) $ error "error!"
        matrixToList res >>= helper g


foreign import ccall "igraph_layout_fruchterman_reingold"
    c_igraph_layout_fruchterman_reingold :: GraphPtr
                                         -> MatrixPtr
                                         -> CInt
                                         -> CDouble
                                         -> CDouble
                                         -> CDouble
                                         -> CDouble
                                         -> CInt
                                         -> VectorPtr
                                         -> VectorPtr
                                         -> VectorPtr
                                         -> VectorPtr
                                         -> VectorPtr
                                         -> IO CInt

-- | 1.12. igraph_layout_lgl — Force based layout algorithm for large graphs.
layoutLgl :: Graph d a 
          -> Int
          -> Double
          -> Double
          -> Double
          -> Double
          -> Double
          -> Int
          -> [(a, (Double, Double))]
layoutLgl g niter maxdelta area coolexp repulserad cellsize proot =
    unsafePerformIO $ withGraph g $ \gp -> do
        res <- newMatrix 0 0
        withMatrix res $ \mp -> do 
            e <- c_igraph_layout_lgl gp
                                     mp
                                     (fromIntegral niter)
                                     (realToFrac maxdelta)
                                     (realToFrac area)
                                     (realToFrac coolexp)
                                     (realToFrac repulserad)
                                     (realToFrac cellsize)
                                     (fromIntegral proot)
            unless (e == 0) $ error "error!"
        matrixToList res >>= helper g

foreign import ccall "igraph_layout_lgl"
    c_igraph_layout_lgl :: GraphPtr
                        -> MatrixPtr
                        -> CInt
                        -> CDouble
                        -> CDouble
                        -> CDouble
                        -> CDouble
                        -> CDouble
                        -> CInt
                        -> IO CInt
