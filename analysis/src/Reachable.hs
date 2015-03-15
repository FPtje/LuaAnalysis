module Reachable where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST

import Data.List (union,(\\))
import MonotoneFramework

-- | Monotone framework instance to find all reachable nodes on a graph from the starting vertex.
mFramework :: MF Bool
mFramework = MF {joinOp=(||),joinOpReturn=(||),iota=True,bottom=False,consistent= \a b _ -> a && b,transfer= \_ y -> y, transferReturn= \_ _ b c -> c , outfun= \l' _ (gr,_) -> out gr l'}
