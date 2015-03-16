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
data SignType = I [IntType] | B [Bool] | Bottom | Top
        deriving (Show,Eq)
data IntType = N | Z | P
        deriving (Show,Eq,Ord)
signFramework :: MF SignAn
signFramework = MF {joinOp=M.unionWith signJoin,joinOpReturn=M.unionWith signJoinOver,iota=M.empty,bottom=M.empty,consistent=signConsist,transfer=signAss,transferReturn= \ a (NReturn b) c d -> synchReturn b c d ,outfun=outF}

mEmbellishedFramework :: MF EmbellishedSign
mEmbellishedFramework = MF {joinOp=sJoin,joinOpReturn=sJoinR,iota=sIota,bottom=sBottom,consistent=sConsistent,transfer=sTransfer,transferReturn=sTransferReturn,outfun=sOutFun}

type EmbellishedSign = M.Map [Node] SignAn

sJoin :: EmbellishedSign -> EmbellishedSign -> EmbellishedSign
sJoin = M.unionWith (M.unionWith signJoin)

sJoinR :: EmbellishedSign -> EmbellishedSign -> EmbellishedSign
sJoinR = M.unionWith (M.unionWith signJoinOver)

sIota :: EmbellishedSign
sIota = M.fromList [([],M.empty)]

sBottom :: EmbellishedSign
sBottom = M.fromList [([],M.empty)]

sConsistent :: EmbellishedSign -> EmbellishedSign -> EdgeLabel -> Bool
sConsistent x y l = all M.null $ map snd $ M.toList $ M.unionWith (\a b -> if signConsist a b l then M.empty else M.union a b) x y

sTransfer :: NodeThing -> EmbellishedSign -> EmbellishedSign
sTransfer nod r = M.map (signAss nod) r

sTransferReturn :: NodeThing -> NodeThing ->  EmbellishedSign -> EmbellishedSign ->  EmbellishedSign
sTransferReturn a (NReturn b) c d = M.unionWith (\e f -> synchReturn b e f) c d

sOutFun ::  Node -> EmbellishedSign -> AnalysisGraph -> [AEdge]
sOutFun l' reach (gr,_) = out gr l'

signJoin :: SignType -> SignType -> SignType
signJoin (I d) (I e) = I (L.union e d )
signJoin (B d) (B e) = B (L.union e d)
signJoin Top _ = Top
signJoin _ Top = Top
signJoin Bottom Bottom = Bottom
signJoin a b = a -- error ("mixing ints and bools" ++ show a ++ show b)

signJoinOver :: SignType -> SignType -> SignType
signJoinOver (I d) (I e) = I (d)
signJoinOver (B d) (B e) = B (d)
signJoinOver Top _ = Top
signJoinOver _ Top = Top
signJoinOver Bottom Bottom = Bottom
signJoinOver a b = a -- error ("mixing ints and bools" ++ show a ++ show b)

signConsist :: SignAn -> SignAn -> EdgeLabel -> Bool
signConsist a b c = keyDiff a b

keyDiff :: (Ord k,Show k) => M.Map k SignType -> M.Map k SignType -> Bool
keyDiff a b = let l = M.toList a
                  k = map (\(x,y) -> case M.lookup x b of
                                     (Just z) -> if containedIn z y then Nothing else Just x
                                     Nothing -> Just x) l
                  m = catMaybes k
              in null m

containedIn Top kek = True
containedIn kek Top = True
containedIn Bottom Bottom = False
containedIn (B a) (B b) = and $ map (\x -> elem x a) b
containedIn (I a) (I b) = and $ map (\x -> elem x a) b
containedIn a b = False

signAss :: NodeThing -> SignAn -> SignAn
signAss (NStat a) b = let ass = filter (\(x,y) -> y /= Bottom) $ catMaybes $ getAss a b
                      in  inserts ass b
             where inserts ((x,y):xs) c = inserts xs (M.insert x y c)
                   inserts [] c = c
signAss (NReturn _) b = b
signAss (ExprCallExit _) ts = ts
signAss (ExprCallEntry _ _) ts = ts
signAss (UnknownFunction s) ts =ts
signAss (UnknownFunctionExpr s) ts =ts
signAss x _ = error $ show x

getAss :: MStat -> SignAn -> [Maybe (Token,SignType)]
getAss s a= case s of
                   (MStat p (Def v)) -> let defs = map fst v
                                            vals = map snd v
                                            vals' = map (\(MExpr _ e) -> calcAss e a) vals
                                            defs' = map (\(PFVar (MToken _ g) _) -> g) defs
                                      in map Just $ zipWith (,) defs' vals'
                   (MStat p (LocDef v)) -> let defs = map fst v
                                               vals = map snd v
                                               vals' = map (\(MExpr _ e) -> calcAss e a) vals
                                               defs' = map (\(PFVar (MToken _ g) _) -> g) defs
                                           in map Just $ zipWith (,) defs' vals'
                   _ -> [Nothing]

synchReturn (AReturn _ [MExpr _ e]) a b = let val = calcAss e b
                                          in M.fromList $ map (\(x,c) -> (x,val)) $(M.toList a)
outF :: Node -> SignAn -> AnalysisGraph -> [AEdge]
outF l' a (gr,_) =
               let nodething = fromJust $ lab gr l' :: NodeThing
                   outs = out gr l'
                   isConditional = case nodething of
                                 (NStat (MStat _ d)) -> case d of
                                              (AIf (MExpr _ c) _ _ _) -> Just c
                                              (AWhile (MExpr _ c) _) -> Just c
                                              (ARepeat _ (MExpr _ c) ) -> Just c
                                              _ -> Nothing
                                 _ -> Nothing
               in case isConditional of
                  Nothing ->outs
                  Just c -> case calcAss c a of
                            (B [True]) -> filter (\(x,y,z) -> filterEdges z True ) outs
                            (B [False]) -> filter (\(x,y,z) -> filterEdges z False ) outs
                            Top -> outs
                            (B [True,False]) -> outs
                            _ -> [] -- outs

filterEdges (Intra g ) f = g == f
filterEdges (Inter _) f = f
filterEdges (ExprInter g ) f = f

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
                                                          Nothing -> Top -- error ("Lookup of " ++ show g ++ " failed, env: " ++ show s)
               ATableConstructor fs -> Bottom
               BinOpExpr op (MExpr _ l) (MExpr _ r) ->
                                   if (calcAss l s == Top || calcAss r s == Top) then Top else
                                   case op of
                                    APlus -> let first = case  (calcAss l s) of
                                                        (I f) -> f
                                                        _ -> []
                                                 second = case  (calcAss r s) of
                                                            (I f) -> f
                                                            _ -> []
                                           in
                                           if elem N first || elem N second
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
                                    AGT -> let first = case  (calcAss l s) of
                                                        (I f) -> f
                                                        _ -> []
                                               second = case  (calcAss r s) of
                                                            (I f) -> f
                                                            _ -> []
                                           in
                                              if first == [] || second == [] then Bottom else
                                              if null [x | x <- first, y <- second, x >= y]
                                              then B [False]
                                              else if null [x | x <- first, y <- second, x <= y]
                                                   then B [True]
                                                   else B [True,False]
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
               UnOpExpr op (MExpr _ r) -> if (calcAss r s == Top) then Top else
                               case op of
                                UnMinus -> let (I f) = calcAss r s
                                           in I (map unSignI f)
                                ANot ->  let (B f) = calcAss r s
                                         in B $ map not f
                                AHash -> Bottom
               _ -> error "ayy"

unSignI :: IntType -> IntType
unSignI Z = Z
unSignI N = P
unSignI P = N
