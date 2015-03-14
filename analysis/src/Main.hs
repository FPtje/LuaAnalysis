module Main where

import GLuanalysis.AG.ControlFlow
import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import GLua.AG.PrettyPrint
import Data.Graph.Inductive.Graph
import Graphviz

import Data.Char
import Data.List (nub,(\\))

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Control.Monad
import MonotoneFramework

import qualified Reachable as R
import qualified LiveVariables as LV
import SignAnalysis

main = putStrLn "Hello World"

run file =do
		contents <- readFile file

		-- Lex the file
		let lex = execParseTokens contents
		let tokens = fst lex
		let errors = snd lex

		unless (null errors) $ do
			mapM_ print errors
			-- Attempt to fix errors when asked
			when (True) $ do
				writeFile file . concatMap show $ tokens
				putStrLn "Success"
				exitSuccess

			exitWith (ExitFailure 1)

		let ast = parseGLua tokens

		putStrLn "\nGraph:\n"
		putStrLn . prettify . fst . getGraph . fst $ ast

		putStrLn "\nExtremal nodes:\n"
		putStrLn . show . snd . getGraph . fst $ ast

		putStrLn "\nNodes\n"
		putStrLn . show . map fst . getNodes. fst $ ast

		putStrLn "\nDuplicate Nodes\n"
		putStrLn . show $ (map fst $ getNodes. fst $ ast) \\ (nub . map fst . getNodes. fst $ ast)

		putStrLn "\nEdges\n"
		putStrLn . show . getEdges . fst $ ast

		putStrLn "\nErrors:\n"
		putStrLn . show . snd $ ast

		putStrLn "Pretty printed code:"
		putStrLn . prettyprint . fst $ ast

liveVar file = do
		contents <- readFile file

		-- Lex the file
		let lex = execParseTokens contents
		let tokens = fst lex
		let errors = snd lex

		unless (null errors) $ do
			mapM_ print errors
			-- Attempt to fix errors when asked
			when (True) $ do
				writeFile file . concatMap show $ tokens
				putStrLn "Success"
				exitSuccess

			exitWith (ExitFailure 1)

		let ast = parseGLua tokens
		putStrLn $ show $ mfp LV.mFramework  (getGraphR . fst $ ast)

reachA file = do
		contents <- readFile file

		-- Lex the file
		let lex = execParseTokens contents
		let tokens = fst lex
		let errors = snd lex

		unless (null errors) $ do
			mapM_ print errors
			-- Attempt to fix errors when asked
			when (True) $ do
				writeFile file . concatMap show $ tokens
				putStrLn "Success"
				exitSuccess

			exitWith (ExitFailure 1)

		let ast = parseGLua tokens
		putStrLn $ show $ mfp R.mFramework  (getGraph . fst $ ast)
                
signA file = do
		contents <- readFile file

		-- Lex the file
		let lex = execParseTokens contents
		let tokens = fst lex
		let errors = snd lex

		unless (null errors) $ do
			mapM_ print errors
			-- Attempt to fix errors when asked
			when (True) $ do
				writeFile file . concatMap show $ tokens
				putStrLn "Success"
				exitSuccess

			exitWith (ExitFailure 1)

		let ast = parseGLua tokens
                
		putStrLn $ show $ mfp signFramework  (getGraph . fst $ ast)

deadcodeAnalysis file = 
                do
                        contents <- readFile file

                        -- Lex the file
                        let lex = execParseTokens contents
                        let tokens = fst lex
                        let errors = snd lex

                        unless (null errors) $ do
                                mapM_ print errors
                                -- Attempt to fix errors when asked
                                when (True) $ do
                                        writeFile file . concatMap show $ tokens
                                        putStrLn "Success"
                                        exitSuccess

                                exitWith (ExitFailure 1)

                        let ast = parseGLua tokens
                        
                        let sign = snd $ mfp signFramework  (getGraph . fst $ ast)
                        let reach = snd $ mfp R.mFramework  (getGraph . fst $ ast)
                        let lv = snd $ mfp LV.mFramework  (getGraphR . fst $ ast)
                        let deadcode = undefined -- zipEm sign reach lv
                        putStrLn ""
zipEm :: (Show d) => a -> b -> c -> [d]
zipEm = undefined

viewGr file = do
		contents <- readFile file

		-- Lex the file
		let lex = execParseTokens contents
		let tokens = fst lex
		let errors = snd lex

		unless (null errors) $ do
			mapM_ print errors
			-- Attempt to fix errors when asked
			when (True) $ do
				writeFile file . concatMap show $ tokens
				putStrLn "Success"
				exitSuccess

			exitWith (ExitFailure 1)

		let ast = parseGLua tokens

		putStrLn . graphviz' . fst . getGraph . fst $ ast
