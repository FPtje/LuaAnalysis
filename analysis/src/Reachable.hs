module Reachable where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST

import Data.List (union,(\\))
import qualified Data.Map as M
import MonotoneFramework

-- | Monotone framework instance to find all reachable nodes on a graph from the starting vertex.
mFramework :: MF {- Map [Node] -} Bool
mFramework = MF {joinOp=(||),joinOpReturn=(||),iota=True,bottom=False,consistent= \a b _ -> a && b,transfer= \_ y -> y, transferReturn= \_ _ b c -> c , outfun= \l' _ (gr,_) -> out gr l'}

type EmbellishedReach = M.Map [Node] Bool

rJoin :: EmbellishedReach -> EmbellishedReach -> EmbellishedReach
rJoin = M.unionWith (||)

rJoinR :: EmbellishedReach -> EmbellishedReach -> EmbellishedReach
rJoinR = M.unionWith (||)

rIota :: EmbellishedReach
rIota = M.fromList [([], False)]

rBottom :: EmbellishedReach
rBottom = M.fromList [([], True)]

rConsistent :: EmbellishedReach -> EmbellishedReach -> EdgeLabel -> Bool
rConsistent x y _ = all snd . M.toList $ M.unionWith (&&) x y

rTransfer :: NodeThing -> EmbellishedReach -> EmbellishedReach
rTransfer _ r = r -- Add'em, just copy the value over (M.map id)

rTransferReturn :: NodeThing -> NodeThing ->  EmbellishedReach -> EmbellishedReach ->  EmbellishedReach
rTransferReturn _ _ _ r = r -- ignore from before call(?)

rOutFun ::  Node -> EmbellishedReach -> AnalysisGraph -> [AEdge]
rOutFun = undefined -- Check 'em
