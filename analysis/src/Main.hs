module Main where

import GLuanalysis.AG.ControlFlow
import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser

import Data.Char

import System.FilePath
import System.Environment
import System.IO
import System.Exit
import Control.Monad


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
		
		putStrLn . show . getGraph . fst $ ast
		
		putStrLn "\nErrors:\n"
		putStrLn . show . snd $ ast