{-# LANGUAGE Strict #-}
module Data.Graph (
    Graph,
    Nodes,
    Vertex,
    Edge,
    buildG,
    fromEdges,
    transpose,
    bounds,
    deleteEdge,
    commonDescendant,
    neighs,
    node,
    updateNode,
    vertices,
    dfs,
    domTree,
    reachable,
    Tree(..)
    ) where

import Data.Array hiding (bounds)
import qualified Data.Array as A
import Data.List(intersect, delete, sortBy)
import Data.Maybe(mapMaybe)
import Data.Foldable(foldr', foldl', toList)

newtype Graph = Graph { unGraph :: Array Vertex [Vertex] } deriving (Eq, Ord)

instance Show Graph where
    show = show . assocs . unGraph

newtype Nodes a = Nodes { unNodes :: Array Vertex a } deriving (Eq, Ord)

instance Show a => Show (Nodes a) where
    show = show . assocs . unNodes

instance Functor Nodes where
    fmap f = Nodes . fmap f . unNodes

instance Foldable Nodes where
    foldMap f = foldMap f . unNodes

instance Traversable Nodes where
    sequenceA = fmap Nodes . sequenceA . unNodes

type Vertex = Int

type Edge = (Vertex, Vertex)

buildG :: (Int, Int) -> [Edge] -> Graph
buildG bounds = Graph . accumArray (flip (:)) [] bounds

fromEdges :: Ord key => [(a, key, [key])] -> (Graph, Nodes a, key -> Maybe Vertex)
fromEdges edges = (graph, nodes, keyMap)
  where bounds = (0,maxInd)
        maxInd = length edges-1
        numEdges = zip [0..] $ sortBy byKey edges
        byKey (_,k1,_) (_,k2,_) = compare k1 k2
        graph = Graph $ array bounds [(i, mapMaybe keyMap ks) | (i,(_,_,ks)) <- numEdges]
        nodes = Nodes $ array bounds [(i,n) | (i,(n,_,_)) <- numEdges]
        keyArr = array bounds [(i,k) | (i,(_,k,_)) <- numEdges]
        keyMap k = findVertex 0 maxInd
          where findVertex a b
                  | a > b = Nothing
                  | otherwise = case midKey `compare` k of
                      LT -> findVertex (mid+1) b
                      EQ -> Just mid
                      GT -> findVertex a (mid-1)
                  where midKey = keyArr ! mid
                        mid = a + (b-a) `div` 2

transpose :: Graph -> Graph
transpose (Graph gr) = buildG (A.bounds gr) [(w,v) | v <- indices gr, w <- gr!v]

bounds :: Graph -> (Vertex, Vertex)
bounds = A.bounds . unGraph

deleteEdge :: Graph -> Edge -> Graph
deleteEdge (Graph gr) (v1,v2) = Graph $ gr // [(v1, delete v2 (gr ! v1))]

commonDescendant :: Graph -> Vertex -> Vertex -> Maybe Vertex
commonDescendant gr v1 v2 = case tail (reachable gr v1) `intersect` tail (reachable gr v2) of
    (x:_) -> Just x
    _ -> Nothing

neighs :: Graph -> Vertex -> [Vertex]
neighs gr v = unGraph gr ! v

node :: Nodes a -> Vertex -> a
node ns v = unNodes ns ! v

updateNode :: Nodes a -> Vertex -> (a -> a) -> Nodes a
updateNode (Nodes ns) i f = Nodes $ ns // [(i, f $ ns ! i)]

vertices :: Graph -> [Vertex]
vertices = indices . unGraph

dfs :: Graph -> Vertex -> Tree Vertex
dfs (Graph gr) root = fst $ dfs' (array (A.bounds gr) initVisited) root
  where initVisited = map (\i -> if i == root then (i,True) else (i,False)) [0..maxInd]
        maxInd = snd $ A.bounds gr
        dfs' visited v = (Branch v rest, visited'')
          where (rest, visited'') = deep visited' neighs
                deep visited (n:ns) =
                    let (tr, visited') = dfs' visited n
                        (rest, visited'') = deep visited' ns
                    in (tr:rest, visited'')
                deep visited _ = ([], visited)
                visited' = visited // map (\i -> (i,True)) neighs
                neighs = filter (\i -> not $ visited ! i) (gr ! v)

domTree :: Graph -> Vertex -> Graph
domTree gr@(Graph arr) root = buildG (A.bounds arr) $ fst $ domTree' (dfs gr root) [] initEdged
  where domTree' (Branch v rest) edges edged = (edges'', edged' // deltaEdged)
          where (edges'', deltaEdged) = foldl' insEdge (edges', []) doms
                (edges',edged') = foldl' (\(es,ed) t -> domTree' t es ed) (edges,edged) rest
                doms = foldr' (\(i,r) d -> if r then d else i:d) [] (assocs $ reachableAvoid gr root v)
                insEdge (es, delta) w
                  | edged' ! w = (es, delta)
                  | otherwise = ((v,w):es, (w,True):delta)
        initEdged = array (A.bounds arr) (zip [0..] $ replicate arrLen False)
        arrLen = snd (A.bounds arr) + 1

reachableAvoid :: Graph -> Vertex -> Vertex -> Array Vertex Bool
reachableAvoid (Graph gr) root avoid
  | root == avoid = initVisited
  | otherwise = reachableAvoid' initVisited root
  where reachableAvoid' visited root = foldl' reachableAvoid' visited' neighs
          where neighs = filter (\v -> v /= avoid && not (visited ! v)) (gr ! root)
                visited' = visited // map (\n -> (n,True)) neighs
        initVisited = array (A.bounds gr) $ map (\v -> (v, v == root || v == avoid)) [0..maxInd]
        maxInd = snd $ A.bounds gr

reachable :: Graph -> Vertex -> [Vertex]
reachable gr = toList . dfs gr

data Tree a = Branch a [Tree a] deriving (Eq, Ord, Show)

instance Functor Tree where
    fmap f (Branch x xs) = Branch (f x) (map (fmap f) xs)

instance Foldable Tree where
    foldMap f (Branch x xs) = f x <> foldMap (foldMap f) xs

instance Traversable Tree where
    sequenceA (Branch x xs) = Branch <$> x <*> traverse sequenceA xs
