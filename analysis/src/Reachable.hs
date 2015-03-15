module Reachable where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST
import Debug.Trace

import Data.List (union,(\\))
import qualified Data.Map as M
import MonotoneFramework

-- | Monotone framework instance to find all reachable nodes on a graph from the starting vertex.
mFramework :: MF {- Map [Node] -} Bool
mFramework = MF {joinOp=(||),joinOpReturn=(||),iota=True,bottom=False,consistent= \a b _ -> a && b,transfer= \_ y -> y, transferReturn= \_ _ b c -> c , outfun= \l' _ (gr,_) -> out gr l'}

mEmbellishedFramework :: MF EmbellishedReach
mEmbellishedFramework = MF {joinOp=rJoin,joinOpReturn=rJoinR,iota=rIota,bottom=rBottom,consistent=rConsistent,transfer=rTransfer,transferReturn=rTransferReturn,outfun=rOutFun}

type EmbellishedReach = M.Map [Node] Bool

rJoin :: EmbellishedReach -> EmbellishedReach -> EmbellishedReach
rJoin = M.unionWith (||)

rJoinR :: EmbellishedReach -> EmbellishedReach -> EmbellishedReach
rJoinR = M.unionWith (||)

rIota :: EmbellishedReach
rIota = M.fromList [([], True)]

rBottom :: EmbellishedReach
rBottom = M.fromList [([], False)]

rConsistent :: EmbellishedReach -> EmbellishedReach -> EdgeLabel -> Bool
rConsistent x y _ = all snd . M.toList $ M.unionWith (&&) x y

rTransfer :: NodeThing -> EmbellishedReach -> EmbellishedReach
rTransfer nod r = case nod of 
                  ExprCallEntry _ n -> M.mapKeys (\x -> n:x ) r -- Add'em, just copy the value over (M.map id) ; no: pattern match on function entry, add 'em
                  _ -> r

rTransferReturn :: NodeThing -> NodeThing ->  EmbellishedReach -> EmbellishedReach ->  EmbellishedReach
rTransferReturn _ _ _ r =  M.mapKeys (\x -> case x of 
                                                           { (_:xs) -> xs ;
                                                             _ -> x }) r -- ignore from before call(?) ; no, pop 'em

rOutFun ::  Node -> EmbellishedReach -> AnalysisGraph -> [AEdge]
rOutFun l' reach (gr,_) = out gr l' -- Check 'em