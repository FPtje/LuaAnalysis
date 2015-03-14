module LiveVariables where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST

import Data.List (union,(\\))
import MonotoneFramework

type KillSet = [Token]
type GenSet = [Token]

data LVNode = LV KillSet GenSet
        deriving Show

mFramework :: MF [Token]
mFramework = MF {joinOp=union,iota=[],bottom=[],consistent=subset,transfer=lvEntry}

--lvEntry :: [[Token]] -> [Token]
--lvEntry (x:xs) = union x (lvEntry xs)
--lvEntry [] = []

lvEntry :: NodeThing -> [Token] -> [Token]
lvEntry (NStat n) ts = let (killset,genset) = getSets n
                       in (ts \\ killset) `union` genset
lvEntry (NReturn _) ts = ts


subset :: Eq a => [a] -> [a] -> EdgeLabel -> Bool
subset x y _ = all (`elem` y) x

createKG :: AnalysisGraph -> Gr LVNode EdgeLabel
createKG g =    let nodes = labNodes . fst $ g
                    edges = labEdges . fst $ g
                    newnodes = let n' = map getSets nodes'
                                   nodes' = map (\(l,(NStat s)) -> s) nodes --wrong, but works for now
                               in zipWith (\(k,g) (l,_) -> (l , LV k g ) ) n' nodes
                in mkGraph newnodes edges

getSets :: Stat -> (KillSet,GenSet)
getSets s = case s of
            (Def v) -> let declvars = map fst v
                           usedvars = map snd v
                           usedvars' = map (\(MExpr _ e) -> e) usedvars
                           declvars' = map (\(PFVar (MToken _ g) _) -> g) declvars --lets assume no function calls for now
                       in (declvars', concatMap findUsedVars usedvars') --deal with local vars
            (AIf (MExpr _ e) _ _ _) -> ([],findUsedVars e)
            _ -> error (show s)

findUsedVars :: Expr -> [Token]
findUsedVars e = case e of
                 (APrefixExpr pre) -> findUsedVars'' pre
                 (BinOpExpr _ a b) -> findUsedVars' a ++ findUsedVars' b
                 (UnOpExpr _ a) -> findUsedVars' a
                 _ -> []
        where  findUsedVars' (MExpr _ e1) = findUsedVars e1
               findUsedVars'' (PFVar (MToken _ g) _) = [g]
               findUsedVars'' (ExprVar e _) = findUsedVars' e
