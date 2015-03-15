module SignAnalysis where

import GLuanalysis.AG.ControlFlow
import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import GLua.AG.PrettyPrint
import Data.Graph.Inductive.Graph hiding (empty)
import Data.Graph.Inductive.PatriciaTree
import Graphviz
import GLua.AG.AST

import qualified Data.Map as M
import Data.Maybe
import Data.Char
import qualified Data.List as L

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Debug.Trace
import Control.Monad
import GLua.TokenTypes
import MonotoneFramework

type SignAn = M.Map Token SignType
data SignType = I [IntType] | B [Bool] | Bottom
        deriving (Show,Eq)
data IntType = N | Z | P
        deriving (Show,Eq,Ord)
signFramework :: MF SignAn
signFramework = MF {joinOp=M.unionWith signJoin,iota=M.empty,bottom=M.empty,consistent=signConsist,transfer=signAss,outfun=outF}

signJoin :: SignType -> SignType -> SignType
signJoin (I d) (I e) = I (L.union e d )
signJoin (B d) (B e) = B (L.union e d)
signJoin _ _ = error "mixing ints and bools"

signConsist :: SignAn -> SignAn -> EdgeLabel -> Bool
signConsist a b c = keyDiff a b

keyDiff :: (Ord k) => M.Map k a -> M.Map k a -> Bool
keyDiff a b = let l = M.toList a
                  k = map (\(x,_) -> case M.lookup x b of 
                                     (Just _) -> Nothing
                                     Nothing -> Just x) l
                  m = catMaybes k
              in null m

signAss :: NodeThing -> SignAn -> SignAn
signAss (NStat a) b = let ass = catMaybes $ getAss a b
                      in  inserts ass b
             where inserts ((x,y):xs) c = inserts xs (M.insert x y c)
                   inserts [] c = c
signAss (NReturn _) b = b
   
getAss :: Stat -> SignAn -> [Maybe (Token,SignType)]
getAss s a= case s of
                   (Def v) -> let defs = map fst v 
                                  vals = map snd v
                                  vals' = map (\(MExpr _ e) -> calcAss e a) vals
                                  defs' = map (\(PFVar (MToken _ g) _) -> g) defs
                              in map Just $ zipWith (,) defs' vals'
                   _ -> [Nothing]
                   
outF :: Node -> SignAn -> AnalysisGraph -> [AEdge]
outF l' a (gr,_) = 
               let nodething = fromJust $ lab gr l' :: NodeThing
                   outs = out gr l'
                   isConditional = case nodething of 
                                 (NStat d) -> case d of
                                              (AIf (MExpr _ c) _ _ _) -> Just c
                                              _ -> Nothing
                                 _ -> Nothing
               in case isConditional of 
                  Nothing -> outs
                  Just c -> case calcAss c a of
                            (B [True]) -> filter (\(x,y,z) -> filterEdges z True ) outs
                            (B [False]) -> filter (\(x,y,z) -> filterEdges z True ) outs
                            (B [True,False]) ->  outs
                            _ -> [] -- outs

filterEdges (Intra g ) f = g == f
filterEdges (Inter _) f = False
filterEdges (ExprInter g ) f = False
                            
calcAss :: Expr -> SignAn -> SignType
calcAss e s = 
              case e of 
               ANil -> Bottom
               AFalse -> B [False]
               ATrue -> B [True]
               ANumber n -> if (read n :: Int) > 0 then I [P] else if (read n :: Int) == 0 then I [Z] else I [N]
               AString (MToken _ st) -> Bottom -- fromJust $ M.lookup st s
               AVarArg -> Bottom
               AnonymousFunc p b -> Bottom
               APrefixExpr (PFVar (MToken _ g) _) -> case M.lookup g s of
                                                          (Just a) -> a
                                                          Nothing -> Bottom -- error ("Lookup of " ++ show g ++ " failed, env: " ++ show s)
               ATableConstructor fs -> Bottom
               BinOpExpr op (MExpr _ l) (MExpr _ r) -> 
                                   case op of 
                                    APlus -> let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if elem N first || elem N second
                                                then if elem P first || elem P second
                                                     then I [N,Z,P]
                                                     else I [N]
                                                else if elem Z first || elem Z second
                                                     then if elem P first || elem P second 
                                                          then I [P]
                                                          else I [Z]
                                                     else I [P]
                                    BinMinus ->     let (I first) = (calcAss l s)
                                                        (I second) = (calcAss r s)
                                                     in if elem N first || elem N second
                                                        then if elem P first || elem P second
                                                             then I [N,Z,P]
                                                             else I [N,Z]
                                                        else if elem Z first || elem Z second
                                                             then if elem P first || elem P second 
                                                                  then I [P,N,Z]
                                                                  else I [Z]
                                                             else I [P,N,Z]
                                    AMultiply ->
                                             let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if elem N first || elem N second
                                                then if elem P first || elem P second
                                                     then if first == [Z] || second == [Z]
                                                          then I [Z]
                                                          else if elem Z first || elem Z second
                                                               then I [N,Z]
                                                               else I [N]
                                                     else I [N]
                                                else if elem Z first || elem Z second
                                                     then if elem P first || elem P second 
                                                          then if first == [Z] || second == [Z]
                                                          then I [Z]
                                                          else if elem Z first || elem Z second
                                                               then I [P,Z]
                                                               else I [P]
                                                          else I [Z]
                                                     else I [Z]
                                    ADivide ->
                                             let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if first == [Z] then I [Z] else
                                                if elem N first || elem N second
                                                then if elem Z first || elem Z second
                                                     then if elem P first || elem P second 
                                                          then I [P,Z,N]
                                                          else I [N]
                                                     else I [P]
                                                else if elem Z first || elem Z second
                                                     then if elem P first || elem P second 
                                                          then I [Z,P]
                                                          else I [Z]
                                                     else I [P]
                                    AModulus ->
                                             let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if first == [Z] then I [Z] else I second                                             
                                    APower ->
                                             let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if elem N first || elem N second
                                                then if elem Z first || elem Z second
                                                     then I [N,P,Z]
                                                     else I [P,N]
                                                else if elem Z first || elem Z second
                                                     then I [P,Z]
                                                     else I [P]
                                    AConcatenate -> Bottom
                                    ALT ->   let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if (first == [N] && not (elem Z second) )||( first == [Z] && second == [P] )then B [True] else B [True,False]
                                    ALEQ -> let (I first) = (calcAss l s)
                                                (I second) = (calcAss r s)
                                            in if (first == [N]) || (elem Z first && (not $ elem N second) && not (elem P first)) || ( elem P first && elem P second) then B [True] else B [True,False]
                                    AGT -> let (I first) = (calcAss l s)
                                               (I second) = (calcAss r s)
                                           in if ( first == [P] && not (elem P second) ) || (elem Z first && second == [N]) then B [True] else B [True,False]
                                    AGEQ ->  let (I first) = (calcAss l s)
                                                 (I second) = (calcAss r s)
                                             in if (first == [P]) || (elem Z first && not (elem P second) && not( elem N first)) || ( elem N first && elem N second) then B [True] else B [True,False]
                                    AEq ->   let first = (calcAss l s)
                                                 second = (calcAss r s)
                                             in case first of
                                                (I _) -> signJoin first second
                                                (B f) -> case second of
                                                         (B g) -> if f == [True] && g == [True] || f ==[False] && g == [False] then B [True] else B[True,False]
                                                         _ -> Bottom
                                    ANEq ->  let first = (calcAss l s)
                                                 second = (calcAss r s)
                                             in case first of
                                                (I _) -> signJoin first second
                                                (B f) -> case second of
                                                         (B g) ->  if f == [True] && g == [True] || f ==[False] && g == [False] then B [False] else B[True,False] -- not totally accurate
                                                         _ -> Bottom
                                    AAnd ->  let first = (calcAss l s)
                                                 second = (calcAss r s)
                                             in case first of
                                                (I _) -> Bottom
                                                (B f) -> case second of
                                                         (B g) -> if not (elem False f ) && not (elem False g) then B [True] else if f ==[False] && g ==[False] then B[False] else B[True,False]
                                                         _ -> Bottom
                                    AOr ->   let first = (calcAss l s)
                                                 second = (calcAss r s)
                                             in case first of
                                                (I _) -> Bottom
                                                (B f) -> case second of
                                                         (B g) -> if f == [True] || g == [True] then B [True] else if not (elem True f) || not (elem True g) then B[False] else B[True,False]
                                                         _ -> Bottom
               UnOpExpr op (MExpr _ r) -> 
                               case op of 
                                UnMinus -> let (I f) = calcAss r s
                                           in I (map unSignI f)
                                ANot ->  let (B f) = calcAss r s
                                         in B $ map not f
                                AHash -> Bottom
               _ -> error "ayy"
unSignI Z = Z
unSignI N = P
unSignI P = N