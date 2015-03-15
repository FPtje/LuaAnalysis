module MonotoneFramework where

import GLuanalysis.AG.ControlFlow
import Data.Graph.Inductive.Graph
import Data.Maybe
import Debug.Trace
import qualified Data.Map as M

-- Mononotone framework data type
data MF a = MF {
    joinOp :: a -> a -> a,
    iota :: a,
    bottom :: a,
    consistent :: a -> a -> EdgeLabel -> Bool,
    transfer :: NodeThing -> a -> a,
    transferReturn :: NodeThing -> NodeThing -> a -> a -> a, -- binary function, give info from call entry + exit
    outfun :: Node -> a -> AnalysisGraph -> [AEdge]
}

-- Working list
type WorkingList = [AEdge]
-- NodeLabels
type NodeLabels a = M.Map Int a

-- Does initialisation of monotone framework.
-- outsources the iteration to the "iteration" function
mfp :: (Show a, Eq a) => MF a -> AnalysisGraph -> (NodeLabels a, NodeLabels a) -- (open, closed)
mfp mf g@(gr, extremals) = let iter = iteration mf g workingList lblData
                           in  (iter, M.mapWithKey mkClosed iter)
        where
    -- Set all initial value to bottom/iota
    nonExtremals = map (\n -> (n, bottom mf)) . filter (`notElem` extremals) . map fst $ labNodes gr
    extremalVals = map (\i -> (i, iota mf))  extremals
    lblData = M.fromList (extremalVals ++ nonExtremals) -- initial values

    workingList = concatMap (out gr) extremals --all edges leaving from extremal value

    -- Create closed set
    mkClosed k = transfer mf (fromJust (lab gr k))

-- Performs monotone framework iterations
iteration :: (Show a) => MF a -> AnalysisGraph -> WorkingList -> NodeLabels a -> NodeLabels a
iteration _  _         []                  nl = nl
iteration mf g@(gr, _) ((l, l', lbl) : xs) nl =
   if consistent mf transferred toNodeVal lbl then
        iteration mf g xs nl -- Next iteration
    else iteration mf g newW newNl where

    getLbl :: Node -> NodeThing
    getLbl      = fromJust . lab gr -- get the label of a node
    fromNodeVal = nl M.! l -- A[l]
    toNodeVal   = nl M.! l' -- A[l']
    lLabel      = getLbl l
    transferred = case lbl of
        Inter (c, _, e, r) -> if l' == r then -- Return part, or in reverse flow graph, the call part
                -- Call the transfer function with arity 2. Give it info from the caller and from the exit
                transferReturn mf (getLbl c) (getLbl e) (nl M.! c) (nl M.! e)
            else
                -- Other side of the function call
                transfer mf lLabel fromNodeVal
        _ -> transfer mf lLabel fromNodeVal  -- f_l(A[l])

    -- A[l'] := A[l'] ⨆ f_l(A[l]);
    newNl = M.insert l' (joinOp mf toNodeVal transferred) nl
    -- forall l'' with (l', l'') ∈ F do W := (l', l'') : W;
    newW =  xs ++ outfun mf l' transferred g -- out gr l'
