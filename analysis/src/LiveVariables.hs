module LiveVariables where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST
import Data.Maybe
import Data.List (union,(\\))
import MonotoneFramework
import GLuanalysis.AG.LiveVariables
import Debug.Trace

import qualified Data.Map as M

type KillSet = [Token]
type GenSet = [Token]

data LVNode = LV KillSet GenSet
        deriving Show

-- | Monotone Framework for Live Variable Analysis
mFramework :: MF [Token]
mFramework = MF {joinOp=union,joinOpReturn= \x y -> x ,iota=[],bottom=[],consistent=subset,transfer=lvEntry,transferReturn = \a b c d -> d,outfun=outF}

--lvEntry :: [[Token]] -> [Token]
--lvEntry (x:xs) = union x (lvEntry xs)
--lvEntry [] = []

-- | Embellished Monotone Framework for Live Variable Analysis
mEmbellishedFramework :: MF EmbellishedLive
mEmbellishedFramework = MF {joinOp=lJoin,joinOpReturn=lJoinR,iota=lIota,bottom=lBottom,consistent=lConsistent,transfer=lTransfer,transferReturn=lTransferReturn,outfun=lOutFun}

type EmbellishedLive = M.Map [Node] [Token]

lJoin :: EmbellishedLive -> EmbellishedLive -> EmbellishedLive
lJoin a b = trace ("Normal join op: " ++ show a ++ " , " ++ show b ++ " Result: " ++ show (M.unionWith union a b) ) $
                M.unionWith union a b

lJoinR :: EmbellishedLive -> EmbellishedLive -> EmbellishedLive
lJoinR a b= trace ("JoinOpReturn: " ++ show a ++ " " ++ show b ++ " Result " ++ show (M.unionWith union a b) ) $
        M.unionWith union a b -- M.unionWith union -- M.unionWith union

lIota :: EmbellishedLive
lIota = M.fromList [([],[])]

lBottom :: EmbellishedLive
lBottom = M.fromList [([],[])]

lConsistent :: EmbellishedLive -> EmbellishedLive -> EdgeLabel -> Bool
lConsistent x y l = let a = all null $ map snd $ trace ("Consistence: " ++ show x ++ "  " ++ show y ) $ M.toList $ M.unionWith (\a b -> if subset a b l then [] else a++b) x y -- check key difference
                        b = M.keys x == M.keys y
                    in a && b

lTransfer :: NodeThing -> EmbellishedLive -> EmbellishedLive
lTransfer nod r = case nod of
                  ExprCallEntry _ n -> trace ("Entering call " ++ show r ++ " node: " ++ show nod ) $
                                        (M.map (lvEntry nod) $M.mapKeys (\x -> case x of
                                                           { (_:xs) -> xs ;
                                                             _ -> x }) r)
                  _ -> M.map (lvEntry nod) r

lTransferReturn :: NodeThing -> NodeThing ->  EmbellishedLive -> EmbellishedLive ->  EmbellishedLive
lTransferReturn a b f r  = trace ("Returning, " ++ show a ++ show b ++ " Sets: " ++ show f ++ show r ++ " RR " ++ show ( M.mapKeys (\x -> 1:x ) f )++ "\n") $
                                M.mapKeys (\x -> 1:x ) f

lOutFun ::  Node -> EmbellishedLive -> AnalysisGraph -> [AEdge]
lOutFun l' reach (gr,_) ={-let isReturn = case M.keys reach of
                                         ((x:xs):ys) -> filter (\x -> doReturn x (head . head $ M.keys reach)) $ out gr l'
                                         _ -> []
                          in if null isReturn then out gr l' else isReturn -}  out gr l'

doReturn (_,_,Inter (a,_,_,_)) r = a == r
doReturn a b = False
-- | Transfer function of Live Variables.
lvEntry :: NodeThing -> [Token] -> [Token]
lvEntry (NStat n) ts = let (killset,genset) = getSets n
                       in (ts \\ killset) `union` genset
lvEntry (NReturn (AReturn _ s)) ts = let (killset,genset) = ([],concatMap varsMExpr s)
                                     in (ts \\ killset) `union` genset
lvEntry (ExprCallExit _) ts = ts
lvEntry (ExprCallEntry _ _) ts = ts
lvEntry (UnknownFunction s) ts = let (killset,genset) = getSets s
                                 in (ts \\ killset) `union` genset
lvEntry (UnknownFunctionExpr s) ts = ts `union` varsMExpr s
lvEntry x ts = error $ show x

-- | Subset, the consistence function of the monotone framework instance
subset :: Eq a => [a] -> [a] -> EdgeLabel -> Bool
subset x y _ = all (`elem` y) x

-- | Function to create the Kill/Gen set of a graph
createKG :: AnalysisGraph -> [(Node,LVNode)]
createKG g = newnodes where
    nodes = labNodes . fst $ g
    kgsets = map (\(i, s) -> sets s) nodes
    newnodes = zipWith (\(k,g) (l,_) -> (l , LV k g)) kgsets nodes

getSets :: MStat -> (KillSet,GenSet)
getSets = kgMStat

sets :: NodeThing -> (KillSet, GenSet)
sets (NStat stat)                 = kgMStat stat
sets (UnknownFunction stat)       = kgMStat stat
sets (UnknownFunctionExpr mexpr)  = kgMExpr mexpr
sets (CallEntry stat node)        = ([], [])
sets (CallExit stat)              = ([], [])
sets (ExprCallEntry mexpr node)   = ([], [])
sets (ExprCallExit mexpr)         = ([], [])
sets (NReturn areturn)            = kgAReturn areturn
sets (NExpr mexpr)                = kgMExpr mexpr
sets (NElseIf elseif)             = kgElseIf elseif


outF l' a (gr,_) = out gr l'

