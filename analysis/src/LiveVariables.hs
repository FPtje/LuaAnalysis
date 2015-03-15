module LiveVariables where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST
import Data.Maybe
import Data.List (union,(\\))
import MonotoneFramework
import Debug.Trace
type KillSet = [Token]
type GenSet = [Token]

data LVNode = LV KillSet GenSet
        deriving Show

mFramework :: MF [Token]
mFramework = MF {joinOp=union,joinOpReturn=union,iota=[],bottom=[],consistent=subset,transfer=lvEntry,transferReturn = \a b c d -> d,outfun=outF}

--lvEntry :: [[Token]] -> [Token]
--lvEntry (x:xs) = union x (lvEntry xs)
--lvEntry [] = []

lvEntry :: NodeThing -> [Token] -> [Token]
lvEntry (NStat n) ts = let (killset,genset) = getSets n
                       in (ts \\ killset) `union` genset
lvEntry (NReturn (AReturn _ s)) ts = let (killset,genset) = ([],concatMap (\(MExpr _ e) -> findUsedVars e) s)
                                     in (ts \\ killset) `union` genset
lvEntry (ExprCallExit _) ts = ts
lvEntry (ExprCallEntry _ _) ts = ts
lvEntry x ts = error $ show x


subset :: Eq a => [a] -> [a] -> EdgeLabel -> Bool
subset x y _ = all (`elem` y) x

createKG :: AnalysisGraph -> [(Node,LVNode)]
createKG g =    let nodes = labNodes . fst $ g
                    edges = labEdges . fst $ g
                    newnodes = let n' = map (\x -> case x of
                                                   Just y -> getSets y
                                                   Nothing -> ([],[]))nodes'
                                   nodes' =          map (\(l,s') -> case s' of
                                                                        (NStat s) -> Just s
                                                                        (NReturn _) -> Nothing
                                                                        (ExprCallExit _) -> Nothing
                                                                        (ExprCallEntry _ _) -> Nothing
                                                                        x -> error (show x) ) nodes --wrong, but works for now
                                   nodes2 =          map (\(l,s') -> case s' of
                                                                        (NStat _) -> Nothing
                                                                        (NReturn (AReturn _ s)) -> Just s
                                                                        (ExprCallExit _) -> Nothing
                                                                        (ExprCallEntry _ _) -> Nothing
                                                                        x -> error (show x) ) nodes --wrong, but works for now
                                   n2 = map (\x -> case x of
                                                   Nothing -> ([],[])
                                                   Just y -> ([], (concatMap (\(MExpr _ e) -> findUsedVars e)) y))  nodes2
                               in zipWith3 (\(k,g) (k1,g1) (l,_) -> (l , LV (union k k1) (union g g1) ) ) n' n2 nodes
                in newnodes

getSets :: Stat -> (KillSet,GenSet)
getSets s = case s of
            (Def v) -> let declvars = map fst v
                           usedvars = map snd v
                           usedvars' = map (\(MExpr _ e) -> e) usedvars
                           declvars' = map (\(PFVar (MToken _ g) _) -> g) declvars --lets assume no function calls for now
                       in (declvars', concatMap findUsedVars usedvars') --deal with local vars
            (AIf (MExpr _ e) _ _ _) -> ([],findUsedVars e)
            (ABreak) -> ([],[])
            (AWhile (MExpr _ e) _ ) -> ([],findUsedVars e)
            (ARepeat _ (MExpr _ e)  ) -> ([],findUsedVars e)
            (AFunc _ _ _) -> ([],[])
            _ -> error (show s)

outF l' a (gr,_) = out gr l'

findUsedVars :: Expr -> [Token]
findUsedVars e =
                 case e of
                 (APrefixExpr pre) -> findUsedVars'' pre
                 (BinOpExpr _ a b) -> findUsedVars' a ++ findUsedVars' b
                 (UnOpExpr _ a) -> findUsedVars' a
                 _ -> []
        where  findUsedVars' (MExpr _ e1) = findUsedVars e1
               findUsedVars'' (PFVar (MToken _ g) [Call _]) = []
               findUsedVars'' (PFVar (MToken _ g) _) = [g]
               findUsedVars'' (ExprVar e _) = findUsedVars' e
