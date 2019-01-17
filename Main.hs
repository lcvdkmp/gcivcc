module Main (main) where

import System.IO
import System.Exit

import Compiler.Ast.Ast
import Compiler.Ast.Print
import Compiler.Ast.Display

import Compiler.Opt
import Compiler.Scanner
import Compiler.Preprocessor
import Compiler.Parser
import Compiler.Error (displayMarker, throwError)

import Compiler.Analyse.SemanticAnalyser
import Compiler.Analyse.CheckUndefined
import Compiler.Analyse.TypeCheck
import Compiler.Analyse.CheckReturn
import Compiler.Analyse.RangeCheck

import Compiler.Pass.Pass

import qualified Compiler.CodeGen as Codegen

import Text.Parsec (errorPos)
import Text.Parsec.Pos
import Data.List.Split (splitOn)
import Control.Monad (foldM, when)

import Data.List (intercalate)
import Data.Set hiding (filter, foldl, map)


-- Returns the passes to run given a certain break. These passes are
-- returned in order
passesToRun :: Maybe CompilerPass -> [CompilerPass]
passesToRun (Just cBreak) = [(minBound :: CompilerPass) .. cBreak]
passesToRun Nothing = [(minBound :: CompilerPass) ..]

main :: IO()
main = do
        (opts, non) <- getOptions

        let Options { 
              verbose = oVerbose
            , input   = oInput
            , output  = oOutput   
            , breakAt = oBreakAt
            } = opts

        case non of 
            (_:_:_) -> do 
                        hPutStrLn stderr "Too many arguments!"
                        printUsage 
                        exitFailure
            _ -> do
                return ()

        -- Handle input file specified by argument
        (inp, fn) <- case oInput of 
                    Nothing ->
                        case non of 
                            (i:_) -> do return (readFile i, i)
                            _ -> 
                                do printUsage
                                   exitFailure
                    Just f -> 
                        case non of
                            [] -> do return (readFile f, f)
                            (_:_) -> 
                                do hPutStrLn stderr "Warning: multiple input files specified."
                                   hPutStrLn stderr "Using the one specified by '-i'"
                                   return (readFile f, f)

        putStrLn $ "Using verbosity level of " ++ show oVerbose

        -- Read infile
        is <- inp

        -- Scan the file and preprocess. This also inserts the getLines
        -- into the tokens
        t' <- scanAndPreprocess fn is

        -- Parse
        ast <- case parse t' of
                        Right x -> do return x
                        -- TODO: move to displayError
                        Left err -> do hPutStrLn stderr $ "Error while parsing: " ++ (show err)
                                       hPutStrLn stderr $ getLine $ sourceLine $ errorPos err
                                       hPutStrLn stderr $ displayMarker $ sourceColumn $ errorPos err
                                       exitFailure
                                       where
                                           getLine :: Int -> String
                                           getLine i = (splitOn "\n" is) !! (i - 1)

        -- TODO: Maybe it would better to merge typeCheckA and
        -- typeCheckMain?
        fast <- case anotate ast >>= checkUndefined >>= typeCheckA >>= checkReturns >>= typeCheckMain of
            Right a -> do return a
            Left err -> do hPutStrLn stderr $ "Error: " ++ show err
                           exitFailure

        -- Collect warnings
        let warns = map (("Warning: " ++) . show) $ checkRange ast
        -- Print warnings
        when (warns /= []) $ do mapM_ putStrLn warns


        when (oVerbose >= 1) $ do
                putStrLn ""
                putStrLn "Before passes:"
                putStrLn ""
                putStrLn $ replicate 80 '='
                putStrLn " Ast: "
                putStrLn $ show fast
                putStrLn $ replicate 80 '='
                putStrLn "Reconstructed program: "
                putStrLn $ programPrint fast
                putStrLn $ replicate 80 '='

        fpast <- if (oVerbose >= 2) 
                    then do foldM passAndPrint fast $ passesToRun oBreakAt
                    else do let x = foldl pass fast $ passesToRun oBreakAt
                            return x

        when (oVerbose >= 1) $ do
            putStrLn ""
            putStrLn $ "After passes:"
            putStrLn ""
            putStrLn $ show fpast
            putStrLn $ replicate 80 '='
            putStrLn $ programPrint fpast 
            putStrLn $ replicate 80 '='

        -- Generate and output code
        let i = Codegen.generateCode fpast
        let noi = intercalate "\n" $ fst $ foldl (\(s, o) (s', o') -> (s ++ [s' ++ " ; " ++ show o], o + o')) ([], 0) $ fmap (\x -> (show x, Codegen.length x)) i
        let oi = noi ++ "\n"
        when (oVerbose >= 1) $ do
            putStrLn "Output program:"
            putStrLn ""
            putStrLn oi
            putStrLn $ replicate 80 '='

        putStrLn "Outputting program..."
        oOutput oi

        -- | Normally the compiler shouldn't do this.
        -- However, the framework expects the compiler to fail on
        -- integerOutOfRange errors
        when (warns /= []) $ do exitWith $ ExitFailure 2
