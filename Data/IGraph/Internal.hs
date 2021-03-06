{-# OPTIONS -fno-warn-orphans #-}

module Data.IGraph.Internal where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as V

import Control.Monad
import Data.List as L
import Data.IORef
import Data.Maybe
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Data.IGraph.Types
import Data.IGraph.Internal.Constants

nodeToId'' :: Graph d a -> a -> Int
nodeToId'' (G g) n = fromMaybe (error "nodeToId': Graph node/ID mismatch.")
                   $ Map.lookup n $ graphNodeToId g

idToNode'' :: Graph d a -> Int -> a
idToNode'' (G g) i = fromMaybe (error $ "idToNode': Graph ID/node mismatch, ID = " ++ show i)
                   $ Map.lookup i $ graphIdToNode g

edgeToEdgeId :: Graph d a -> Edge d a -> Int
edgeToEdgeId g@(G _) e = case elemIndex e (edges g) of
  Just i -> i
  _      -> error "edgeToEdgeId: Edge not in graph."

edgeIdToEdge :: Graph d a -> Int -> Edge d a
edgeIdToEdge g i
  | i < 0 || i >= length es = error ("edgeIdToEdge: Index " ++ show i ++ " out of bound.")
  | otherwise               = es !! i
 where
  es = edges g

getNeiMode :: Integral i => Graph d a -> i
getNeiMode (G g) = fromIntegral $ fromEnum (graphNeiMode g)

--------------------------------------------------------------------------------
-- Graphs


--
-- Graph IDs
--

foreign import ccall "igraphhaskell_graph_set_vertex_ids"
  c_igraphhaskell_graph_set_vertex_ids :: GraphPtr -> IO Int

setVertexIds :: GraphPtr -> IO ()
setVertexIds gp = do
  e <- c_igraphhaskell_graph_set_vertex_ids gp
  case e of
       -1 -> error "setVertexIds: igraph C attributes not initialized. Try compiling your program with GHC instead of using GHCi. See GHC ticket #781."
       _  -> return ()

foreign import ccall "igraphhaskell_graph_get_vertex_ids"
  c_igraphhaskell_graph_get_vertex_ids :: GraphPtr -> VectorPtr -> IO CInt

getVertexIds :: GraphPtr -> IO (Maybe [Double])
getVertexIds gp = do
  v <- newVector 0
  s <- withVector v $ \vp ->
         c_igraphhaskell_graph_get_vertex_ids
           gp
           vp
  case s of
       1  -> return Nothing
       -1 -> error "getVertexIds: igraph C attributes not initialized. Try compiling your program with GHC instead of using GHCi. See GHC ticket #781."
       _  -> Just `fmap` vectorToList v

subgraphFromPtr :: Graph d a -> GraphPtr -> IO (Graph d a)
subgraphFromPtr g@(G _) gp' = do
    Just oriIds <- getVertexIds gp'
    from <- newVector 0
    to <- newVector 0
    _ <- withVector from $ \vp1 -> withVector to $ \vp2 ->
        c_igraph_edges gp' vp1 vp2
    l1 <- vectorToList from
    l2 <- vectorToList to
    let idMap = V.fromList . map truncate $ oriIds
        getNodes = map (idToNode'' g . (idMap V.!) . truncate)
        -- final graph without weights
        subg@(G sg) = fromListWithCtxt g $ zip (getNodes l1) (getNodes l2)
        -- set correct weights
        wes' = [ setWeight (toEdge a b) w
             | e <- intersectBy (eqOnEdgeNodes g)
                                (edges g)
                                (edges subg)
             , let a = edgeFrom e
                   b = edgeTo e
                   w = fromMaybe 0 (edgeWeight e)
             ]
        subgWithWeights = G sg{ graphEdges = Set.fromList wes' }
    return $ if isWeighted g
                then subgWithWeights
                else subg
  where
    fromListWithCtxt :: Graph d a -> [(a,a)] -> Graph d a
    fromListWithCtxt (G _) = fromList

    eqOnEdgeNodes :: Eq a => Graph d a -> Edge d a -> Edge d a -> Bool
    eqOnEdgeNodes (G _) e1 e2 =
      (==) (edgeFrom e1, edgeTo e1)
      (edgeFrom e2, edgeTo e2)

foreign import ccall "igraphhaskell_initialize"
  c_igraphhaskell_initialize :: IO CInt

-- | Initialize C vertex attributes (only once! -> C code handles this)
initialize :: IO ()
initialize = do
  _ <- c_igraphhaskell_initialize
  return ()

--
-- Graph construction
--

foreign import ccall "c_igraph_create"
  c_igraph_create :: VectorPtr -> CInt -> IO GraphPtr

foreign import ccall "c_arpack_create"
  c_arpack_create :: IO ArpackPtr

foreign import ccall "&c_arpack_destroy"
  c_arpack_destroy :: FunPtr (ArpackPtr -> IO ())

buildForeignGraph :: Graph d a -> Graph d a
buildForeignGraph g@(G gr) = G
  (gr { graphForeignPtr    = unsafePerformIO io
      , graphArpackOptions = unsafePerformIO arpack
      })
  where
  io :: IO (ForeignPtr Grph)
  io = do -- initialize vertex IDs/C attributes
          initialize
          -- initialization done
          v <- edgesToVector g
          withVector v $ \vp -> do
            gp  <- c_igraph_create vp (if isDirected g then 1 else 0)
            newForeignPtr c_igraph_destroy gp
  arpack :: IO (ForeignPtr Arpack)
  arpack = do
    p <- c_arpack_create
    newForeignPtr c_arpack_destroy p

withGraph :: Graph d a -> (GraphPtr -> IO res) -> IO res
withGraph (G gr) = withForeignPtr (graphForeignPtr gr)

setGraphPointer :: Graph d a -> GraphPtr -> IO (Graph d a)
setGraphPointer (G g) gp = do
  fp <- newForeignPtr c_igraph_destroy gp
  return $ G g{ graphForeignPtr = fp }

withWeights :: Graph (Weighted d) a -> (VectorPtr -> IO res) -> IO res
withWeights g io = do
  v <- listToVector $ map getWeight (edges g)
  withVector v io

withOptionalWeights :: Graph d a -> (VectorPtr -> IO res) -> IO res
withOptionalWeights g@(G _) io = do
  let mws = getWeights g
  case mws of
       Nothing -> io nullPtr
       Just ws -> listToVector ws >>= flip withVector io

foreign import ccall "edges"
  c_igraph_edges :: GraphPtr -> VectorPtr -> VectorPtr -> IO CInt

--------------------------------------------------------------------------------
-- (orphan) graph instances

instance Show a => Show (Graph U a) where
  show (G g) = show (graphEdges g)

instance Show a => Show (Graph D a) where
  show (G g) = show (graphEdges g)

instance Eq (Graph U a) where
  (G g1) == (G g2) = graphEdges g1 == graphEdges g2

instance Eq (Graph D a) where
  (G g1) == (G g2) = graphEdges g1 == graphEdges g2

instance Show (Edge d a) => Show (Graph (Weighted d) a) where
  show (G g) = show (graphEdges g)

instance Eq (Edge d a) => Eq (Graph (Weighted d) a) where
  (G g1) == (G g2) = graphEdges g1 == graphEdges g2

--------------------------------------------------------------------------------
-- Vertex selectors

foreign import ccall "c_igraph_vs_create"
  c_igraph_vs_create :: IO VsPtr

foreign import ccall "&c_igraph_vs_destroy"
  c_igraph_vs_destroy :: FunPtr (VsPtr -> IO ())

newVs :: IO VsForeignPtr
newVs = do
  vsp <- c_igraph_vs_create
  fvp <- newForeignPtr c_igraph_vs_destroy vsp
  return $ VsF fvp


foreign import ccall "igraph_vs_all"
  c_igraph_vs_all :: VsPtr -> IO CInt

foreign import ccall "igraph_vs_adj"
  c_igraph_vs_adj :: VsPtr -> CInt -> CInt -> IO CInt

foreign import ccall "igraph_vs_nonadj"
  c_igraph_vs_nonadj :: VsPtr -> CInt -> CInt -> IO CInt

foreign import ccall "igraph_vs_none"
  c_igraph_vs_none :: VsPtr -> IO CInt

foreign import ccall "igraph_vs_1"
  c_igraph_vs_1 :: VsPtr -> CInt -> IO CInt

foreign import ccall "igraph_vs_vector"
  c_igraph_vs_vector :: VsPtr -> VectorPtr -> IO CInt

{-
foreign import ccall "igraph_vs_seq"
  c_igraph_vs_seq :: VsPtr -> CInt -> CInt -> IO CInt
-}

withVs :: VertexSelector a -> Graph d a -> (VsPtr -> IO res) -> IO res
withVs vs g f = do
  fvs <- newVs
  -- bind to C vertex selector pointer
  withVs' fvs $ \vsp -> do
    e <- case vs of
         VsAll      -> c_igraph_vs_all    vsp
         VsNone     -> c_igraph_vs_none   vsp
         VsAdj    a -> c_igraph_vs_adj    vsp (ident a) (fromIntegral $ fromEnum Out)
         VsNonAdj a -> c_igraph_vs_nonadj vsp (ident a) (fromIntegral $ fromEnum Out)
         Vs1      a -> c_igraph_vs_1      vsp (ident a)
         VsList   l -> do
           v <- listToVector (map ident l)
           withVector v $ c_igraph_vs_vector vsp
    unless (e == 0) $ error "error"
    f vsp
 where
  ident a = fromIntegral (nodeToId'' g a) :: CInt

withVs' :: VsForeignPtr -> (VsPtr -> IO res) -> IO res
withVs' (VsF fp) = withForeignPtr fp

--------------------------------------------------------------------------------
-- Edge selectors

foreign import ccall "c_igraph_es_create"
  c_igraph_es_create :: IO EsPtr
foreign import ccall "&c_igraph_es_destroy"
  c_igraph_es_destroy :: FunPtr (EsPtr -> IO ())

newEs :: IO EsForeignPtr
newEs = do
  esp <- c_igraph_es_create
  fep <- newForeignPtr c_igraph_es_destroy esp
  return $ EsF fep

foreign import ccall "igraph_es_all"
  c_igraph_es_all       :: EsPtr -> CInt -> IO CInt
foreign import ccall "igraph_es_none"
  c_igraph_es_none      :: EsPtr -> IO CInt
foreign import ccall "igraph_es_incident"
  c_igraph_es_incident  :: EsPtr -> CInt -> CInt -> IO CInt
foreign import ccall "igraph_es_1"
  c_igraph_es_1         :: EsPtr -> CInt -> IO CInt
foreign import ccall "igraph_es_vector"
  c_igraph_es_vector    :: EsPtr -> VectorPtr -> IO CInt
foreign import ccall "es_fromto"
  c_igraph_es_fromto    :: EsPtr -> VsPtr -> VsPtr -> IO CInt
foreign import ccall "igraph_es_seq"
  c_igraph_es_seq       :: EsPtr -> CInt -> CInt -> IO CInt

withEs :: EdgeSelector d a -> Graph d a -> (EsPtr -> IO res) -> IO res
withEs es g f = do
  fes <- newEs
  _e  <- withEs' fes $ \esp ->
    case es of
         EsAll            -> c_igraph_es_all      esp (fromIntegral $ fromEnum EdgeOrderId)
         EsNone           -> c_igraph_es_none     esp
         EsIncident a     -> c_igraph_es_incident esp (ident a) (getNeiMode g)
         EsSeq a b        -> c_igraph_es_seq      esp (ident a) (ident b)
         Es1 e            -> c_igraph_es_1        esp (ident' e)
         EsFromTo vs1 vs2 -> withVs vs1 g $ \vsp1 -> withVs vs2 g $ \vsp2 ->
                             c_igraph_es_fromto   esp vsp1 vsp2
         EsList l         -> do
           v <- listToVector (map (edgeToEdgeId g) l)
           withVector v $ \vp ->
             c_igraph_es_vector esp vp
  withEs' fes f
 where
  ident  a = fromIntegral (nodeToId''   g a) :: CInt
  ident' a = fromIntegral (edgeToEdgeId g a) :: CInt

withEs' :: EsForeignPtr -> (EsPtr -> IO res) -> IO res
withEs' (EsF fp) = withForeignPtr fp

--------------------------------------------------------------------------------
-- ARPACK options

withArpack :: Graph d a -> (ArpackPtr -> IO res) -> IO res
withArpack (G Graph{ graphArpackOptions = fp }) = withForeignPtr fp


--------------------------------------------------------------------------------
-- Matrices

foreign import ccall "igraph_matrix_e"
  c_igraph_matrix_get     :: MatrixPtr -> CLong -> CLong -> IO CDouble
foreign import ccall "c_igraph_matrix_create"
  c_igraph_matrix_create  :: CLong -> CLong -> IO MatrixPtr
foreign import ccall "&c_igraph_matrix_destroy"
  c_igraph_matrix_destroy :: FunPtr (MatrixPtr -> IO ())
foreign import ccall "igraph_matrix_set"
  c_igraph_matrix_set     :: MatrixPtr -> CLong -> CLong -> CDouble -> IO ()
foreign import ccall "igraph_matrix_nrow"
  c_igraph_matrix_nrow    :: MatrixPtr -> IO CLong
foreign import ccall "igraph_matrix_ncol"
  c_igraph_matrix_ncol    :: MatrixPtr -> IO CLong
foreign import ccall "igraph_matrix_get_row"
  c_igraph_matrix_get_row :: MatrixPtr -> VectorPtr -> CLong -> IO CInt

newMatrix :: Int -> Int -> IO Matrix
newMatrix nrow ncol = do
  mp  <- c_igraph_matrix_create (fromIntegral nrow) (fromIntegral ncol)
  fmp <- newForeignPtr c_igraph_matrix_destroy mp
  return $ Matrix fmp

getMatrixValue :: Matrix -> Int -> Int -> IO Double
getMatrixValue (Matrix fmp) x y = withForeignPtr fmp $ \ mp -> do
  cd <- c_igraph_matrix_get mp (fromIntegral x) (fromIntegral y)
  return $ realToFrac cd

listToMatrix :: Integral a => [[a]] -> IO Matrix
listToMatrix l = do
  m <- newMatrix nrow ncol
  withMatrix m $ \mp ->
    -- fill the matrix
    forListM_ (zip [0..] l)     $ \(r,row) ->
      forListM_ (zip [0..] row) $ \(c,val) ->
        c_igraph_matrix_set mp r c (fromIntegral val)
  return m
 where
  nrow = maximum (map length l)
  ncol = length l

matrixToList :: Matrix -> IO [[Double]]
matrixToList m = withMatrix m $ \mp -> do
  nrow <- c_igraph_matrix_nrow mp
  ncol <- c_igraph_matrix_ncol mp
  forM [0..nrow-1] $ \r -> do
    v  <- newVector (fromIntegral ncol)
    _e <- withVector v $ \vp ->
      c_igraph_matrix_get_row mp vp r
    vectorToList v

--------------------------------------------------------------------------------
-- Sparse matrices

{-
foreign import ccall "c_igraph_sparsemat_create"
  c_igraph_sparsemat_create  :: CLong -> CLong -> IO SpMatrixPtr
foreign import ccall "&c_igraph_sparsemat_destroy"
  c_igraph_sparsemat_destroy :: FunPtr (SpMatrixPtr -> IO ())
foreign import ccall "igraph_sparsemat_set"
  c_igraph_sparsemat_set     :: SpMatrixPtr -> CLong -> CLong -> CDouble -> IO ()

--foreign import ccall "igraph_sparsemat_nrow"
--  c_igraph_sparsemat_nrow    :: SpMatrixPtr -> IO CLong
--foreign import ccall "igraph_sparsemat_ncol"
--  c_igraph_sparsemat_ncol    :: SpMatrixPtr -> IO CLong
--foreign import ccall "igraph_sparsemat_get_row" -- does not exist!
--  c_igraph_sparsemat_get_row :: SpMatrixPtr -> VectorPtr -> CLong -> IO CInt

newSparseMatrix :: Int -> Int -> IO SparseMatrix
newSparseMatrix nrow ncol = do
  mp  <- c_igraph_sparsemat_create (fromIntegral nrow) (fromIntegral ncol)
  fmp <- newForeignPtr c_igraph_sparsemat_destroy mp
  return $ SparseMatrix fmp

listToSparseMatrix :: Integral a => [[a]] -> IO SparseMatrix
listToSparseMatrix l = do
  sm <- newSparseMatrix nrow ncol
  withSparseMatrix sm $ \smp ->
    -- fill the matrix
    forListM_ (zip [0..] l)     $ \(r,row) ->
      forListM_ (zip [0..] row) $ \(c,val) ->
        c_igraph_sparsemat_set smp r c (fromIntegral val)
  return sm
 where
  nrow = maximum (map length l)
  ncol = length l

{-
sparseMatrixToList :: SparseMatrix -> IO [[Double]]
sparseMatrixToList sm = withSparseMatrix sm $ \smp -> do
  nrow <- c_igraph_sparsemat_nrow smp
  ncol <- c_igraph_sparsemat_ncol smp
  forM [0..nrow-1] $ \r -> do
    v  <- newVector (fromIntegral ncol)
    _e <- withVector v $ \vp ->
      c_igraph_sparsemat_get_row
        smp
        vp
        r
    vectorToList v
-}
-}

--------------------------------------------------------------------------------
-- Vectors

foreign import ccall "c_igraph_vector_create"             c_igraph_vector_create              :: CLong -> IO VectorPtr
foreign import ccall "&c_igraph_vector_destroy"           c_igraph_vector_destroy             :: FunPtr (VectorPtr -> IO ())

newVector :: Int -> IO Vector
newVector s = do
  vp <- c_igraph_vector_create (fromIntegral s)
  newVector' vp

newVector' :: VectorPtr -> IO Vector
newVector' vp = do
  fvp <- newForeignPtr c_igraph_vector_destroy vp
  return $ Vector fvp

foreign import ccall "igraph_vector_set"                  c_igraph_vector_set                 :: VectorPtr -> CLong -> CDouble -> IO ()
foreign import ccall "igraph_vector_e"                    c_igraph_vector_get                 :: VectorPtr -> CLong -> IO CDouble
foreign import ccall "igraph_vector_size"                 c_igraph_vector_length              :: VectorPtr -> IO CLong

vectorToList :: Vector -> IO [Double]
vectorToList (Vector fvp) = withForeignPtr fvp vectorToList'

vectorToList' :: VectorPtr -> IO [Double]
vectorToList' vp = do
  len <- c_igraph_vector_length vp
  let go :: [Double] -> CLong -> IO [Double]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_get vp (i - 1)
                    go (realToFrac e : acc) (i - 1)
  go [] len

listToVector :: (Integral a) => [a] -> IO Vector
listToVector as = do
  v <- newVector (length as)
  withVector v $ \vp -> do
    sizeRef <- newIORef (0 :: Int)
    forListM_ as $ \a -> do
      size <- readIORef sizeRef
      c_igraph_vector_set vp (fromIntegral size) (fromIntegral a)
      modifyIORef sizeRef (+1)
  return v


--------------------------------------------------------------------------------
-- VectorPtr

foreign import ccall "c_igraph_vector_ptr_create"         c_igraph_vector_ptr_create          :: CLong -> IO VectorPtrPtr
foreign import ccall "&c_igraph_vector_ptr_destroy"       c_igraph_vector_ptr_destroy         :: FunPtr (VectorPtrPtr -> IO ())

newVectorPtr :: Int -> IO VectorP
newVectorPtr s = do
  vp <- c_igraph_vector_ptr_create (fromIntegral s)
  newVectorPtr' vp

newVectorPtr' :: VectorPtrPtr -> IO VectorP
newVectorPtr' vp = do
  fvp <- newForeignPtr c_igraph_vector_ptr_destroy vp
  return $ VectorP fvp

foreign import ccall "&c_graph_vector_destroy"
  c_graph_vector_destroy :: FunPtr (Ptr GraphVec -> IO ())

newGraphVector :: Int -> IO GraphVectorP
newGraphVector s = do
  gvp  <- c_igraph_vector_ptr_create (fromIntegral s)
  fgvp <- newForeignPtr c_graph_vector_destroy (castPtr gvp)
  return $ GraphVectorP fgvp

foreign import ccall "igraph_vector_ptr_e"                c_igraph_vector_ptr_get             :: VectorPtrPtr -> CLong -> IO VectorPtr
foreign import ccall "igraph_vector_ptr_size"             c_igraph_vector_ptr_length          :: VectorPtrPtr -> IO CLong

vectorPtrToList :: VectorP -> IO [[Double]]
vectorPtrToList vptr = do
  vps <- vectorPtrToListOfVectorPtr vptr
  mapM vectorToList' vps

vectorPtrToListOfVectorPtr
  :: VectorP
  -> IO [VectorPtr]
vectorPtrToListOfVectorPtr (VectorP fvp) = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_ptr_length vp
  let go :: [VectorPtr] -> CLong -> IO [VectorPtr]
      go acc 0 = return acc
      go acc i = do vp' <- c_igraph_vector_ptr_get vp (i-1)
                    go (vp':acc) (i-1)
  go [] len


edgesToVector :: Graph d a -> IO Vector
edgesToVector g@(G g') =
  listToVector $ foldr (\e r -> toId (edgeFrom e) : toId (edgeTo e) : r) [] (edges g)
 where
  toId n = fromMaybe (error "edgesToVector: Graph node/ID mismatch.")
         $ Map.lookup n (graphNodeToId g')

vectorToEdges :: Graph d a -> Vector -> IO [Edge d a]
vectorToEdges g@(G _) v = do
  l <- vectorToList v
  return $ map (edgeIdToEdge g . round) l

vectorToEdges' :: Graph d a -> VectorPtr -> IO [Edge d a]
vectorToEdges' g@(G _) vp = do
  l <- vectorToList' vp
  return $ map (edgeIdToEdge g . round) l

vectorToVertices :: Graph d a -> Vector -> IO [a]
vectorToVertices g@(G _) v = fmap (map (idToNode'' g . round)) (vectorToList v)

vectorToVertices' :: Graph d a -> VectorPtr -> IO [a]
vectorToVertices' g@(G _) vp = fmap (map (idToNode'' g . round)) (vectorToList' vp)

vectorPtrToVertices :: Graph d a -> VectorP -> IO [[a]]
vectorPtrToVertices g@(G _) v = fmap (map (map (idToNode'' g . round))) (vectorPtrToList v)

vectorPtrToEdges :: Graph d a -> VectorP -> IO [[Edge d a]]
vectorPtrToEdges g@(G _) v = do
  l <- vectorPtrToList v
  return $ map (map (edgeIdToEdge g . round)) l

graphVectorToSubgraphs :: GraphVectorP -> Graph d a -> IO [Graph d a]
graphVectorToSubgraphs (GraphVectorP fvp) g = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_ptr_length (castPtr vp)
  let go :: Graph d a -> [Graph d a] -> CLong -> IO [Graph d a]
      go _    acc 0 = return acc
      go ctxt acc i = do gp <- c_igraph_vector_ptr_get (castPtr vp) (i-1)
                         --fp <- newForeignPtr c_igraph_destroy gp
                         g' <- subgraphFromPtr ctxt (castPtr gp)
                         go ctxt (g' : acc) (i-1)
  go g [] len

--------------------------------------------------------------------------------
-- Ptr stuff

withMatrix :: Matrix -> (MatrixPtr -> IO a) -> IO a
withMatrix (Matrix fmp) = withForeignPtr fmp

withVector :: Vector -> (VectorPtr -> IO a) -> IO a
withVector (Vector fvp) = withForeignPtr fvp

withVectorPtr :: VectorP -> (VectorPtrPtr -> IO a) -> IO a
withVectorPtr (VectorP fvp) = withForeignPtr fvp

withGraphVector :: GraphVectorP -> (GraphVecPtr -> IO a) -> IO a
withGraphVector (GraphVectorP fgvp) = withForeignPtr fgvp

{-
withSparseMatrix :: SparseMatrix -> (SpMatrixPtr -> IO a) -> IO a
withSparseMatrix (SparseMatrix fmp) = withForeignPtr fmp
-}

--------------------------------------------------------------------------------
-- Foreign imports

foreign import ccall "&c_igraph_destroy"                  c_igraph_destroy                    :: FunPtr (GraphPtr -> IO ())

--------------------------------------------------------------------------------
-- Helper Functions

forListM_ :: [a] -> (a -> IO b) -> IO ()
forListM_ []       _ = return ()
forListM_ (a : as) f = f a >> forListM_ as f

-- forListM :: [a] -> (a -> IO b) -> IO [b]
-- forListM = go []
--   where
--   go :: [b] -> [a] -> (a -> IO b) -> IO [b]
--   go acc [] _       = return (reverse acc)
--   go acc (a : as) f = f a >>= \b -> go (b : acc) as f
--
--
--
--
--------------------------------------------------------------------------------
-- Basics

getWeight :: Edge (Weighted d) a -> Int
getWeight (W _ w) = w

toEdgeWeighted :: E d a => a -> a -> Int -> Edge (Weighted d) a
toEdgeWeighted a b = W (toEdge a b)

emptyGraph :: E d a => Graph d a
emptyGraph = buildForeignGraph $ G (Graph 0 0 Map.empty Map.empty Set.empty undefined undefined Out)

-- Get old context
emptyWithCtxt :: Graph d a -> Graph d a
emptyWithCtxt (G _) = emptyGraph

fromList :: E d a => [(a,a)] -> Graph d a
fromList = foldl' (\g (a,b) -> insertEdge (toEdge a b) g) emptyGraph

fromListWeighted :: (E d a, IsUnweighted d) => [(a,a,Int)] -> Graph (Weighted d) a
fromListWeighted = foldl' (\g (a,b,w) -> insertEdge (W (toEdge a b) w) g) emptyGraph

numberOfNodes :: Graph d a -> Int
numberOfNodes (G g) = graphNodeNumber g

numberOfEdges :: Graph d a -> Int
numberOfEdges (G g) = graphEdgeNumber g

member :: a -> Graph d a -> Bool
member a (G g) = a `Map.member` graphNodeToId g

nodeToId :: Graph d a -> a -> Maybe Int
nodeToId (G g) n = Map.lookup n (graphNodeToId g)

idToNode :: Graph d a -> Int -> Maybe a
idToNode (G g) i = Map.lookup i (graphIdToNode g)

-- insertNode :: a -> Graph d a -> Graph d a
-- insertNode n (G g)
--   | n `member` (G g) = G g -- node already in g
--   | otherwise = G $
--     g { graphNodeNumber = i
--       , graphIdToNode   = Map.insert i n (graphIdToNode g)
--       , graphNodeToId   = Map.insert n i (graphNodeToId g)
--       , graphForeignPtr = Nothing
--       }
--  where
--   i = graphNodeNumber g + 1

deleteNode :: a -> Graph d a -> Graph d a
deleteNode n (G g) = buildForeignGraph $ G $
  case Map.lookup n (graphNodeToId g) of
       Just i  -> g { graphNodeNumber = graphNodeNumber g - 1
                    , graphIdToNode   = Map.delete i (graphIdToNode g)
                    , graphNodeToId   = Map.delete n (graphNodeToId g)
                    , graphEdges      = Set.filter (\e -> edgeFrom e /= n && edgeTo e /= n) (graphEdges g)
                    }
       Nothing -> g

insertEdge :: Edge d a -> Graph d a -> Graph d a
insertEdge e (G g)
  | e `elem` edges (G g) || f == t = G g
  | otherwise = buildForeignGraph $ G $
    case (Map.member f (graphNodeToId g), Map.member t (graphNodeToId g)) of
         (True,  True)  -> insertEdge'' (G g)
         (False, True)  -> insertEdge'' (insertNode f i (G g))
         (True,  False) -> insertEdge'' (insertNode t i (G g))
         (False, False) -> insertEdge'' (insertNode t (i+1) $ insertNode f i (G g))
 where
  (f,t) = (edgeFrom e, edgeTo e)
  i     = Map.size (graphIdToNode g)

  insertEdge'' (G g') =
    g' { graphEdgeNumber = graphEdgeNumber g' + 1
       , graphEdges      = Set.insert e (graphEdges g')
       }

  insertNode :: a -> Int -> Graph d a -> Graph d a
  insertNode n ni (G g') = G $
    g' { graphNodeNumber = graphNodeNumber g' + 1
       , graphIdToNode   = Map.insert ni n  (graphIdToNode g')
       , graphNodeToId   = Map.insert n  ni (graphNodeToId g') }

deleteEdge :: Edge d a -> Graph d a -> Graph d a
deleteEdge e (G g)
  | Set.member e (graphEdges g) = buildForeignGraph $ deleteNodes $ G $
    g { graphEdges      = Set.delete e (graphEdges g)
      , graphEdgeNumber = graphEdgeNumber g - 1
      }
  | otherwise = G g
 where
  (f,t) = (edgeFrom e, edgeTo e)
  deleteNodes g' =
    let delF = if null (neighbours f g') then deleteNode f else id
        delT = if null (neighbours t g') then deleteNode t else id
     in delT . delF $ g'

-- | return the nodes ordered by their id
nodes :: Graph d a -> [a]
nodes (G g) = Map.elems.graphIdToNode $ g

edges :: Graph d a -> [Edge d a]
edges (G g) = F.toList $ graphEdges g

neighbours :: a -> Graph d a -> [a]
neighbours n g@(G _) =
  foldr neighbours'' [] (edges g)
 where
  neighbours'' e r
    | edgeFrom e == n                       = edgeTo   e : r
    | edgeTo   e == n && not (isDirected g) = edgeFrom e : r
    | otherwise                             = r

-- | Reverse graph direction. This simply changes the associated
-- `igraph_neimode_t` of the graph (`IGRAPH_OUT` to `IGRAPH_IN`, `IGRAPH_IN` to
-- `IGRAPH_OUT`, other to `IGRAPH_OUT`). O(1)
reverseGraphDirection :: Graph d a -> Graph d a
reverseGraphDirection (G g) = G g { graphNeiMode = reverse' (graphNeiMode g) }
 where
  reverse' Out = In
  reverse' In  = Out
  reverse' _ = Out

toDirected :: (IsUndirected u, E (ToDirected u) a) => Graph u a -> Graph (ToDirected u) a
toDirected (G g) =
  G g { graphEdges = Set.map undirectedToDirected (graphEdges g) }

toUndirected :: (IsDirected d, E (ToUndirected d) a) => Graph d a -> Graph (ToUndirected d) a
toUndirected (G g) =
  G g { graphEdges = Set.map directedToUndirected (graphEdges g) }
