module Reachable where

import GLuanalysis.AG.ControlFlow
import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import GLua.AG.PrettyPrint
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Graphviz

import Data.Char
import Data.List (nub,(\\))

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Control.Monad

data NewNode = N NodeThing Sign Bool --currently using boolean, should probs use Bottom
        deriving Show

type Sign = Either SignVar Bottom
data SignVar = B Bool | I [EvenOdd] [IntSign]
        deriving Show
data EvenOdd = E | O
        deriving Show
data IntSign = Min | Nul | Pos
        deriving Show
data Bottom = Bottom
        deriving Show

-- TODO: Build in chaos algorithm or some sort of fixedpoint, do something properly with signs and assignments, refine the statements into expressions to be usedde .

-- | Transform the graph to a datatype we can use in our reachability analysis.
transformGraph :: AnalysisGraph -> Gr NewNode EdgeLabel
transformGraph g = let nodes = labNodes . fst $ g
                       edges = labEdges . fst $  g
                       newnodes = map (\(l,x) -> (l, N x (Right Bottom) False)) nodes
		   in  mkGraph newnodes edges

-- Assume the start node is 1 and end node the highest node, for now.
deptfirst :: Gr NewNode EdgeLabel -> Node -> Gr NewNode EdgeLabel
deptfirst g n = let nexts = concatMap ((\(l,x@(N y f b)) -> if l `elem` suc g n && not b then [l] else [] )) nodes
                    newG = mkGraph newnodes (labEdges g)
                    nodes = labNodes g
                    newnodes = map (\(l,N x f b) -> if l `elem` suc g n then (l,N x f True) else (l,N x f b)) nodes
                in  if null nexts then g else unify $ map (deptfirst newG) nexts

unify :: [Gr NewNode EdgeLabel] -> Gr NewNode EdgeLabel
unify (x:y:xs) = let nodes = labNodes x
                     edges = labEdges x
                     nodesY = labNodes y
                     edgesY = labEdges y
                 in  unify ((mkGraph (unifyNodes nodes nodesY) edges) : xs)
unify [x] = x
unify [] = empty
unify (x:y:[]) = let nodes = labNodes x
                     edges = labEdges x
                     nodesY = labNodes y
                     edgesY = labEdges y
                 in  (mkGraph (unifyNodes nodes nodesY) edges)
-- | Assumes sortedness
unifyNodes :: [LNode NewNode] -> [LNode NewNode] -> [LNode NewNode]
unifyNodes ((l,N y f b):xs) ((k,N x g c):ys) = (if l == k then (l,N y (unifySign f g) (b || c)) else error "unsorted nodes in unifyNodes") : unifyNodes xs ys
unifyNodes [] [] = []
unifyNodes _ _ = error "unifyNodes"

unifySign :: Sign -> Sign -> Sign
unifySign (Left s) (Left y) = case s of
                                (B b) -> case y of
                                         (B c) -> Left (B $ b || c)
                                         _ -> error "types"
                                (I e f) -> case y of
                                           (I h i) -> Left (I (e++h) (f++i))
unifySign _ _ = Right Bottom
