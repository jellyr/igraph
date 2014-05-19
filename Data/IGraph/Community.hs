-- | Haskell bindings to the igraph C library.
-- Chapter 22. Detecting Community Structure
--

module Data.IGraph.Community
    ( communityOptimalModularity
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

groupByMembership :: Ord b => [a] -> [b] -> [[a]]
groupByMembership xs = map (fst . unzip) . groupBy ((==) `on` snd)
                                         . sortBy (compare `on` snd) . zip xs
{-# INLINE groupByMembership #-}

-- | group vertices in a graph by membership
fromMembership :: Ord b => Graph d a -> [b] -> [[a]]
fromMembership g = (map.map) (idToNode'' g).groupByMembership [0..]
{-# INLINE fromMembership #-}

-- | 1.2. igraph_community_optimal_modularity — Calculate the community structure with the highest modularity value
--
communityOptimalModularity :: Graph d a -> [[a]]
communityOptimalModularity g = unsafePerformIO $ withGraph g $ \gp -> do
    membership <- newVector 0
    withVector membership $ \memberp -> do
        _ <- c_igraph_community_optimal_modularity gp nullPtr memberp nullPtr
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
                   -> Double
                   -> Double
                   -> Double
                   -> Double
                   -> [[a]]
communitySpinglass g starttemp stoptemp coolfact gamma = unsafePerformIO $
    alloca $ \modularity -> 
    alloca $ \temperature ->
    withGraph g $ \gp -> do
        membership <- newVector 0
        _ <- withVector membership $ \membership' -> c_igraph_community_spinglass
            gp nullPtr modularity temperature membership' nullPtr spins
            parupdate (realToFrac starttemp) (realToFrac stoptemp) 
            (realToFrac coolfact) updateRule (realToFrac gamma) implementation
            gamma_minus
        memberList <- vectorToList membership
        return.fromMembership g $ memberList
  where
    spins = 25
    parupdate = 0
    gamma_minus = 0
    implementation = fromIntegral.fromEnum $ SpincommImpOrig
    updateRule = fromIntegral.fromEnum $ SpincommUpdateConfig

foreign import ccall "igraph_community_spinglass"
    c_igraph_community_spinglass :: GraphPtr
                                 -> VectorPtr
                                 -> Ptr CDouble
                                 -> Ptr CDouble
                                 -> VectorPtr
                                 -> VectorPtr
                                 -> CInt
                                 -> CInt
                                 -> CDouble
                                 -> CDouble
                                 -> CDouble
                                 -> CInt
                                 -> CDouble
                                 -> CInt
                                 -> CDouble
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
        _ <- withArpack g $ \ap ->
            c_igraph_community_leading_eigenvector gp nullPtr nullPtr membership' (fromIntegral step) ap nullPtr 0 nullPtr nullPtr nullPtr nullFunPtr nullFunPtr
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
