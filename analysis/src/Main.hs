-- Student numbers: 3583600, 3705269, 3800296, 3749002

module Main where
import GLuanalysis.AG.ControlFlow
import GLua.Lexer
import GLua.TokenTypes
import GLua.Parser
import GLua.AG.PrettyPrint
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Graphviz

import Data.Map (toList)
import Data.Maybe
import Data.List (nub,(\\))

import System.Exit
import Control.Monad
import MonotoneFramework

import qualified Reachable as R
import qualified LiveVariables as LV
import SignAnalysis
import GLua.AG.Token

main :: IO ()
main = do
    contents <- getContents

    let (ast, errors) = parseGLuaFromString contents

    unless (null errors) $ do
        mapM_ print errors
        exitWith (ExitFailure 1)

    let forwards@(grF, _) = getGraph ast
    let backwards@(_, _) = getGraphR ast

    let sign = toList $ fst $ mfp signFramework forwards
    let reach = toList $ snd $ mfp R.mFramework forwards
    let lv = toList $ snd $ mfp LV.mFramework backwards
    let lv' = map (\(x, LV.LV k _) -> (x,k)) $ LV.createKG backwards
    let lv2 = checkLV lv lv' grF
    let deadcode = catMaybes $ zipEm sign reach lv2
    let deadcode1 = mapMaybe (lab grF) deadcode

    mapM_ print deadcode1

run :: FilePath -> IO ()
run file = do
        contents <- readFile file

        -- Lex the file
        let lex = execParseTokens contents
        let tokens = fst lex
        let errors = snd lex

        unless (null errors) $ do
            mapM_ print errors
            -- Attempt to fix errors when asked
            when True $ do
                writeFile file . concatMap show $ tokens
                putStrLn "Success"
                exitSuccess

            exitWith (ExitFailure 1)

        let ast = parseGLua tokens

        putStrLn "\nGraph:\n"
        putStrLn . prettify . fst . getGraph . fst $ ast

        putStrLn "\nExtremal nodes:\n"
        print . snd . getGraph . fst $ ast

        putStrLn "\nNodes\n"
        print .  getNodes. fst $ ast

        putStrLn "\nDuplicate Nodes\n"
        print $ (map fst . getNodes. fst $ ast) \\ (nub . map fst . getNodes. fst $ ast)

        putStrLn "\nEdges\n"
        print . getEdges . fst $ ast

        putStrLn "\nErrors:\n"
        print . snd $ ast

        putStrLn "Pretty printed code:"
        putStrLn . prettyprint . fst $ ast

liveVar :: FilePath -> IO ()
liveVar file = do
        contents <- readFile file

        -- Lex the file
        let lex = execParseTokens contents
        let tokens = fst lex
        let errors = snd lex

        unless (null errors) $ do
            mapM_ print errors
            -- Attempt to fix errors when asked
            when True $ do
                writeFile file . concatMap show $ tokens
                putStrLn "Success"
                exitSuccess

            exitWith (ExitFailure 1)

        let ast = parseGLua tokens
        return ()


reachA :: FilePath -> IO ()
reachA file = do
        contents <- readFile file

        -- Lex the file
        let lex = execParseTokens contents
        let tokens = fst lex
        let errors = snd lex

        unless (null errors) $ do
            mapM_ print errors
            -- Attempt to fix errors when asked
            when True $ do
                writeFile file . concatMap show $ tokens
                putStrLn "Success"
                exitSuccess

            exitWith (ExitFailure 1)

        let ast = parseGLua tokens
        print $ mfp R.mFramework  (getGraph . fst $ ast)

signA :: FilePath -> IO ()
signA file = do
        contents <- readFile file

        -- Lex the file
        let lex = execParseTokens contents
        let tokens = fst lex
        let errors = snd lex

        unless (null errors) $ do
            mapM_ print errors
            -- Attempt to fix errors when asked
            when True $ do
                writeFile file . concatMap show $ tokens
                putStrLn "Success"
                exitSuccess

            exitWith (ExitFailure 1)

        let ast = parseGLua tokens

        print $ mfp signFramework  (getGraph . fst $ ast)
        -- print $ mfp mEmbellishedFramework  (getGraph . fst $ ast)


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
                                when True $ do
                                        writeFile file . concatMap show $ tokens
                                        putStrLn "Success"
                                        exitSuccess

                                exitWith (ExitFailure 1)

                        let ast = parseGLua tokens

                        let sign = toList $ fst $ mfp signFramework  (getGraph . fst $ ast)
                        let reach = toList $ snd $ mfp R.mFramework  (getGraph . fst $ ast)
                        let lv = toList $ snd $ mfp LV.mFramework  (getGraphR . fst $ ast)
                        let lv' = map (\(x, LV.LV k _) -> (x,k)) $ LV.createKG (getGraphR . fst $ ast)
                        let lv2 = checkLV lv lv' (fst $ getGraph . fst $ ast)
                        let deadcode = catMaybes $ zipEm sign reach lv2
                        let deadcode1 = map (lab (fst $ getGraph . fst $ ast)) deadcode

                        print lv2 -- reach --(fst $ getGraph . fst $ ast)

checkLV :: [(Node,[Token])] -> [(Node,LV.KillSet)] -> Gr NodeThing EdgeLabel -> [(Node,Bool)]
checkLV nodeset ((y,[]):xs) gr = (y,True) : checkLV nodeset xs gr
checkLV nodeset ((y,g):xs) gr =  let adjacent = suc gr y
                                     nodeset' = concat $ catMaybes $ map (\x -> lookup x nodeset) adjacent
                                 in (y,  any (`elem` nodeset') g) : checkLV nodeset xs gr
checkLV nodeset [] gr = []

zipEm :: [(Node,SignAn)] -> [(Node,Bool)] -> [(Node,Bool)] -> [Maybe Node]
zipEm = zipWith3 (\(a,b) (c,d) (e,f) -> if a == c && a == e
                                        then if (((toList b) == [] && a /= 1)|| d == False || f == False )
                                             then Just a
                                             else Nothing
                                        else error ( "Unsorted in zipEm " ++ show a ++ " " ++ show c ++ " " ++ show e ))

viewGr file = do
        contents <- readFile file

        -- Lex the file
        let lex = execParseTokens contents
        let tokens = fst lex
        let errors = snd lex

        unless (null errors) $ do
            mapM_ print errors
            -- Attempt to fix errors when asked
            when True $ do
                writeFile file . concatMap show $ tokens
                putStrLn "Success"
                exitSuccess

            exitWith (ExitFailure 1)

        let ast = parseGLua tokens

        putStrLn . graphviz' . fst . getGraph . fst $ ast
