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

import Data.Map
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

type SignAn = Map Token SignType
data SignType = I [IntType] | B [Bool]
        deriving Show
data IntType = N | Z | P
        deriving (Show,Eq)
signFramework :: MF SignAn
signFramework = MF {joinOp=unionWith signJoin,iota=empty,bottom=empty,consistent=undefined,transfer=undefined}

signJoin :: SignType -> SignType -> SignType
signJoin (I d) (I e) = I (L.union e d )
signJoin (B d) (B e) = B (L.union e d)
signJoin _ _ = error "mixing ints and bools"