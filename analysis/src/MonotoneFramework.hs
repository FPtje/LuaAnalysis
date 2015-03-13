module MonotoneFramework where

import GLuanalysis.AG.ControlFlow
import Data.Graph.Inductive.Graph
import qualified Data.Map as M

-- Mononotone framework data type
data MF a = MF {
    joinOp :: a -> a -> a,
    iota :: a,
    bottom :: a,
    consistent :: a -> a -> EdgeLabel -> Bool,
    tf :: a -> a
}

-- Working list
type WorkingList = [AEdge]
-- NodeLabels
type NodeLabels a = M.Map Int a

-- Does initialisation of monotone framework.
-- outsources the iteration to the "iteration" function
mfp :: MF a -> AnalysisGraph -> NodeLabels a
mfp mf g@(gr, extremals) = iteration mf g workingList lblData where

    -- Set all initial value to bottom/iota
    nonExtremals = map (\n -> (n, bottom mf)) . filter (`elem` extremals) . map fst $ labNodes gr
    extremalVals = map (\i -> (i, iota mf))  extremals
    lblData = M.fromList (nonExtremals ++ extremalVals) -- initial values

    workingList = labEdges gr

-- Performs monotone framework iterations
iteration :: MF a -> AnalysisGraph -> WorkingList -> NodeLabels a -> NodeLabels a
iteration _  _         []                  nl = nl
iteration mf g@(gr, _) ((l, l', lbl) : xs) nl = if consistent mf transferred toNodeVal lbl then
        iteration mf g xs nl -- Next iteration
    else
        iteration mf g newW newNl where

    fromNodeVal = nl M.! l -- A[l]
    toNodeVal   = nl M.! l' -- A[l']
    transferred = tf mf fromNodeVal -- f_l(A[l])

    -- A[l'] := A[l'] ⨆ f_l(A[l]);
    newNl = M.insert l' (joinOp mf toNodeVal transferred) nl
    -- forall l'' with (l', l'') ∈ F do W := (l', l'') : W;
    newW = out gr l'
