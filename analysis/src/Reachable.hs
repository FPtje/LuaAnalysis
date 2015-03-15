module Reachable where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST

import Data.List (union,(\\))
import Data.Map
import MonotoneFramework

-- | Monotone framework instance to find all reachable nodes on a graph from the starting vertex.
mFramework :: MF {- Map [Node] -} Bool
mFramework = MF {joinOp=(||),joinOpReturn=(||),iota=True,bottom=False,consistent= \a b _ -> a && b,transfer= \_ y -> y, transferReturn= \_ _ b c -> c , outfun= \l' _ (gr,_) -> out gr l'}

type EmbellishedReach = Map [Node] Bool

rJoin :: EmbellishedReach -> EmbellishedReach -> EmbellishedReach
rJoin = unionWith (||)

rJoinR :: EmbellishedReach -> EmbellishedReach -> EmbellishedReach
rJoinR = unionWith (||)

rIota :: EmbellishedReach
rIota = fromList [([],False)]

rBottom :: EmbellishedReach
rBottom = fromList [([],True)]

rConsistent :: EmbellishedReach -> EmbellishedReach -> EdgeLabel -> Bool
rConsistent = \x y z -> and . Prelude.map snd . toList $ unionWith (\a b -> a && b) x y

rTransfer :: NodeThing -> EmbellishedReach -> EmbellishedReach
rTransfer = undefined -- Add'em

rTransferReturn :: NodeThing -> NodeThing ->  EmbellishedReach -> EmbellishedReach ->  EmbellishedReach
rTransferReturn = undefined -- Subtract'em

rOutFun ::  Node -> EmbellishedReach -> AnalysisGraph -> [AEdge]
rOutFun = undefined -- Check 'em