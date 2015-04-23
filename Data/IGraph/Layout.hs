-- | Haskell bindings to the igraph C library.
-- Chapter 18. Generating Layouts for Graph
--

module Data.IGraph.Layout
    ( layoutFruchtermanReingold
    , layoutGridFruchtermanReingold
    , layoutLgl
    , layoutKamadaKawai
    ) where

import Data.IGraph.Internal
import Data.IGraph.Types
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad

helper :: Monad m => Graph d a -> [[Double]] -> m [(a, (Double, Double))]
helper g = return.map ( \(i,[x,y]) -> (idToNode'' g i, (x,y)) ).zip [0..] 
{-# INLINE helper #-}

-- | 1. 2D layout generators
--
-- | 1.8. igraph_layout_fruchterman_reingold — Places the vertices on a plane according to the Fruchterman-Reingold algorithm.
layoutFruchtermanReingold :: Graph d a
                          -> Int     -- ^ The number of iterations to do. A
                                     -- reasonable default value is 500
                          -> Double  -- ^ The maximum distance to move a vertex
                                     -- in an iteratoin. A reasonable default is
                                     -- the number of vertices
                          -> Double  -- ^ The area parameter of the algorithm. A
                                     -- reasonable default is the square of the
                                     -- number of vertices
                          -> Double  -- ^ the cooling exponent of the simulated
                                     -- annealing. A reasonable default is 1.5
                          -> Double  -- ^ Determins the radius at which vertex-
                                     -- vertex repulsion cancels out attraction
                                     -- of adjacent vertices. A reasonable default
                                     -- is area times the number of vertices
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

layoutGridFruchtermanReingold :: Graph d a
                              -> Int     -- ^ The number of iterations to do. A
                                         -- reasonable default value is 500
                              -> Double  -- ^ The maximum distance to move a vertex
                                         -- in an iteratoin. A reasonable default is
                                         -- the number of vertices
                              -> Double  -- ^ The area parameter of the algorithm. A
                                         -- reasonable default is the square of the
                                         -- number of vertices
                              -> Double  -- ^ the cooling exponent of the simulated
                                         -- annealing. A reasonable default is 1.5
                              -> Double  -- ^ Determins the radius at which vertex-
                                         -- vertex repulsion cancels out attraction
                                         -- of adjacent vertices. A reasonable default
                                         -- is area times the number of vertices
                              -> Double  -- The size of the grid cells. A reasonable
                                         -- default is the fourth root of area
                                         -- (or the square root of the number
                                         -- of vertices if area is also left at its default value)
                              -> [(a, (Double, Double))]
layoutGridFruchtermanReingold g niter maxdelta area coolexp repulserad cellsize =
    unsafePerformIO $ withGraph g $ \gp -> do
        res <- newMatrix 0 0
        withMatrix res $ \mp -> do
            e <- c_igraph_layout_grid_fruchterman_reingold gp
                                                           mp
                                                           (fromIntegral niter)
                                                           (realToFrac maxdelta)
                                                           (realToFrac area)
                                                           (realToFrac coolexp)
                                                           (realToFrac repulserad)
                                                           (realToFrac cellsize)
                                                           0
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

foreign import ccall "igraph_layout_grid_fruchterman_reingold"
    c_igraph_layout_grid_fruchterman_reingold :: GraphPtr
                                              -> MatrixPtr
                                              -> CInt
                                              -> CDouble
                                              -> CDouble
                                              -> CDouble
                                              -> CDouble
                                              -> CDouble
                                              -> CInt
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

layoutKamadaKawai :: Graph d a
                  -> Int
                  -> Double
                  -> Double
                  -> Double
                  -> Double
                  -> [(a, (Double, Double))]
layoutKamadaKawai g niter sigma initemp coolexp kkconst = 
    unsafePerformIO $ withGraph g $ \gp -> do
        res <- newMatrix 0 0
        withMatrix res $ \mp -> do 
            e <- c_igraph_layout_kamada_kawai gp
                                              mp
                                              (fromIntegral niter)
                                              (realToFrac sigma)
                                              (realToFrac initemp)
                                              (realToFrac coolexp)
                                              (realToFrac kkconst)
                                              0
                                              nullPtr
                                              nullPtr
                                              nullPtr
                                              nullPtr
            unless (e == 0) $ error "error!"
        matrixToList res >>= helper g

foreign import ccall "igraph_layout_kamada_kawai"
    c_igraph_layout_kamada_kawai :: GraphPtr
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
                                 -> IO CInt
