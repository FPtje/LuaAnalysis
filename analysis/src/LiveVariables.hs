module LiveVariables where

import GLuanalysis.AG.ControlFlow
import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import GLua.AG.PrettyPrint
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Graphviz
import GLua.AG.AST

import Data.Char
import Data.List (union)

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import GLua.TokenTypes
import MonotoneFramework

type KillSet = [Token]
type GenSet = [Token]

data LVNode = LV KillSet GenSet
        deriving Show

mFramework :: MF [Token]
mFramework = MF {joinOp=union,iota=[],bottom=[],consistent=subset,transfer=undefined}

subset :: Eq a => [a] -> [a] -> EdgeLabel -> Bool
subset = \ x y a -> and $ map (\z -> elem z y) x

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

findUsedVars :: Expr -> [Token]
findUsedVars e = case e of
                 (APrefixExpr pre) -> findUsedVars'' pre
                 (BinOpExpr _ a b) -> findUsedVars' a ++ findUsedVars' b
                 (UnOpExpr _ a) -> findUsedVars' a
                 _ -> []
        where  findUsedVars' (MExpr _ e1) = findUsedVars e1
               findUsedVars'' (PFVar (MToken _ g) _) = [g]
               findUsedVars'' (ExprVar e _) = findUsedVars' e

