module Reachable where

import GLuanalysis.AG.ControlFlow
import GLua.TokenTypes
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import GLua.AG.AST

import Data.List (union,(\\))
import MonotoneFramework

mFramework :: MF [Token]
mFramework = MF {joinOp=union,iota=[],bottom=[],consistent=undefined,transfer=undefined}
