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
    transfer :: NodeThing -> a -> a
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

    workingList = labEdges gr

    -- Create closed set
    mkClosed k = transfer mf (fromJust (lab gr k))

-- Performs monotone framework iterations
iteration :: (Show a) => MF a -> AnalysisGraph -> WorkingList -> NodeLabels a -> NodeLabels a
iteration _  _         []                  nl = nl
iteration mf g@(gr, _) ((l, l', lbl) : xs) nl = if consistent mf transferred toNodeVal lbl then
        iteration mf g xs nl -- Next iteration
    else iteration mf g newW newNl where

    fromNodeVal = nl M.! l -- A[l]
    toNodeVal   = nl M.! l' -- A[l']
    lLabel     = fromJust $ lab gr l
    transferred = transfer mf lLabel fromNodeVal  -- f_l(A[l])

    -- A[l'] := A[l'] ⨆ f_l(A[l]);
    newNl = M.insert l' (joinOp mf toNodeVal transferred) nl
    -- forall l'' with (l', l'') ∈ F do W := (l', l'') : W;
    newW = xs ++ out gr l'
