{-# LANGUAGE BangPatterns #-}
-- | Haskell bindings to the igraph C library.
-- Chapter 22. Detecting Community Structure
--

module Data.IGraph.Community
    ( modularity
    , communityOptimalModularity
    , communitySpinglass
    , communityLeadingEigenvector
    ) where

import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Data.List
import Data.Function
import Control.Monad (unless)

groupByMembership :: Ord b => [a] -> [b] -> [[a]]
groupByMembership xs = map (fst . unzip) . groupBy ((==) `on` snd)
                                         . sortBy (compare `on` snd) . zip xs
{-# INLINE groupByMembership #-}

-- | group vertices in a graph by membership
fromMembership :: Ord b => Graph d a -> [b] -> [[a]]
fromMembership g = (map.map) (idToNode'' g).groupByMembership [0..]
{-# INLINE fromMembership #-}

toMembership :: Graph d a -> [[a]] -> IO Vector
{-# INLINE toMembership #-}
toMembership g communities = listToVector.snd.unzip.sort $ loop (0::Int) communities
  where
    loop !n (x:xs) = zip (map (nodeToId'' g) x) (repeat n) ++ loop (n+1) xs
    loop _ _ = []
    
-- | 1. Common functions related to community structure
--
-- | 1.1. igraph_modularity — Calculate the modularity of a graph with respect to some vertex types
modularity :: Graph d a -> [[a]] -> Double
modularity g communities = unsafePerformIO $ withGraph g $ \gp -> do
    membership <- toMembership g communities
    withVector membership $ \vp ->
        withOptionalWeights g $ \wp ->
        alloca $ \mp -> do
            e <- c_igraph_modularity gp vp mp wp
            unless (e == 0) $ error "error"
            peek mp

foreign import ccall "igraph_modularity"
    c_igraph_modularity :: GraphPtr
                        -> VectorPtr
                        -> Ptr Double
                        -> VectorPtr
                        -> IO CInt

-- | 1.2. igraph_community_optimal_modularity — Calculate the community structure with the highest modularity value
communityOptimalModularity :: Graph d a -> [[a]]
communityOptimalModularity g = unsafePerformIO $ withGraph g $ \gp -> do
    membership <- newVector 0
    withVector membership $ \memberp -> do
        e <- c_igraph_community_optimal_modularity gp nullPtr memberp nullPtr
        unless (e == 0) $ error "error"
        memberList <- vectorToList membership
        return.fromMembership g $ memberList

foreign import ccall "igraph_community_optimal_modularity"
    c_igraph_community_optimal_modularity :: GraphPtr
                                          -> Ptr Double
                                          -> VectorPtr
                                          -> VectorPtr
                                          -> IO CInt
        
-- | 2. Community structure based on statistical mechanics
--
-- | 2.1. igraph_community_spinglass — Community detection based on statistical mechanics
communitySpinglass :: Graph d a
                   -> Int     -- ^ number of spins
                   -> Double  -- ^ The temperature at the start. Default: 1.0
                   -> Double  -- ^ Stop temperature. Default: 0.01
                   -> Double  -- ^ The cooling factor for the simulated annealing.
                              -- Default: 0.99
                   -> Double  -- ^ Gamma parameter, which defines the weight of
                              -- the missing and existing links in the quality
                              -- function for the clustering. Default: 1.0
                   -> [[a]]
communitySpinglass g spins starttemp stoptemp coolfact gamma = unsafePerformIO $
    withGraph g $ \gp ->
    withOptionalWeights g $ \wp -> do
        membership <- newVector 0
        withVector membership $ \membership' -> do
            e <- c_igraph_community_spinglass gp wp nullPtr nullPtr membership'
                                              nullPtr (fromIntegral spins) parupdate
                                              (realToFrac starttemp)
                                              (realToFrac stoptemp)
                                              (realToFrac coolfact) updateRule
                                              (realToFrac gamma) implementation
                                              gamma_minus
            unless (e == 0) $ error "error"
        memberList <- vectorToList membership
        return.fromMembership g $ memberList
  where
    parupdate = False
    gamma_minus = 0
    implementation = fromIntegral.fromEnum $ SpincommImpOrig
    updateRule = fromIntegral.fromEnum $ SpincommUpdateConfig

foreign import ccall "igraph_community_spinglass"
    c_igraph_community_spinglass :: GraphPtr     -- graph
                                 -> VectorPtr    -- weights
                                 -> Ptr CDouble  -- modularity
                                 -> Ptr CDouble  -- temperature
                                 -> VectorPtr    -- membership
                                 -> VectorPtr    -- csize
                                 -> CInt         -- spins
                                 -> Bool         -- parallel update
                                 -> CDouble      -- start temperature
                                 -> CDouble      -- stop temperature
                                 -> CDouble      -- cool factor
                                 -> CInt         -- update_rule
                                 -> CDouble      -- gamma
                                 -> CInt         -- implementation
                                 -> CDouble      -- gamma minus
                                 -> IO CInt

-- | 3. Community structure based on eigenvectors of matrices
--
-- | 3.1. igraph_community_leading_eigenvector — Leading eigenvector community finding (proper version).

communityLeadingEigenvector :: Graph d a
                            -> Int
                            -> [[a]]
communityLeadingEigenvector g step = unsafePerformIO $ withGraph g $ \ gp -> do
    membership <- newVector 0
    withVector membership $ \membership' -> do
        withArpack g $ \ap -> do
            e <- c_igraph_community_leading_eigenvector gp nullPtr nullPtr
                                                        membership'
                                                        (fromIntegral step) ap
                                                        nullPtr 0 nullPtr
                                                        nullPtr nullPtr
                                                        nullFunPtr nullFunPtr
            unless (e == 0) $ error "error"
        memberList <- vectorToList membership
        return.fromMembership g $ memberList


foreign import ccall "igraph_community_leading_eigenvector"
    c_igraph_community_leading_eigenvector :: GraphPtr
                                           -> VectorPtr
                                           -> MatrixPtr
                                           -> VectorPtr
                                           -> CInt
                                           -> ArpackPtr
                                           -> Ptr CDouble
                                           -> CInt
                                           -> VectorPtr
                                           -> VectorPtrPtr
                                           -> VectorPtr
                                           -> FunPtr CInt
                                           -> FunPtr ()
                                           -> IO CInt
