module Compiler.Opt (
    Options(..),
    listCompilerPasses,
    getOptions,
    printUsage
) where
import Compiler.Pass.Pass

import System.IO
import System.Console.GetOpt
import System.Environment
import System.Exit

    
readEither :: (Read a) => String -> String -> Either String a
readEither s err = case reads s of
                  [(x, _)] -> Right x
                  _ -> Left err

exitError err = do
        putStrLn $ "Error: " ++ err
        exitFailure
                                
-- Command line argument options
data Options = Options 
             { verbose :: Int
             , input :: Maybe (String)
             , output :: String -> IO()
             , breakAt :: Maybe CompilerPass
             }

-- Default command line argument options
defaultOptions :: Options
defaultOptions = Options 
                { verbose = 0
                , input = Nothing
                , output  = putStr
                , breakAt = Nothing
                }

options :: [ OptDescr (Options -> IO Options) ]
options = 
        [ Option "i" ["input"]
            (ReqArg
                (\arg opt -> return opt { input = Just arg })
                "FILE")
            "The input file"

        , Option "h" ["help"] 
            (NoArg (\_ -> do
                printUsage
                exitWith ExitSuccess))
            "Show (this) help message"

        , Option "v" ["verbose"]
            (ReqArg
                (\arg opt -> return opt { verbose = read arg})
                "LEVEL")
            "Enable verbose output"

        , Option "o" ["output"]
            (ReqArg
                (\arg opt -> return opt { output = writeFile arg })
                "FILE")
            "Output to FILE"

        , Option "b" ["break"]
            (ReqArg
                (\arg opt -> do
                -- Read the compiler pass, erroring when the compiler pass
                -- in invalid
                    case (readEither arg arg) :: (Either String (CompilerPass)) of
                        Right break -> do
                            return opt { breakAt = Just break}
                        Left err -> 
                            exitError $ ("Unknown CompilerPass \"" ++ err ++ "\"\n" ++
                                "Available passes: " ++ showPasses)
                )
                "COMPILER_PASS")
            ("Break at a certain compiler pass.")
        ]

getOptions :: IO (Options, [String])
getOptions = do
        args <- getArgs
        let (actions, nonOptions, errors) = getOpt Permute options args
        opts <- foldl (>>=) (return defaultOptions) actions
        return (opts, nonOptions)

printUsage :: IO ()
printUsage = do 
                pname <- getProgName
                hPutStrLn stderr $ "Usage: " ++ (usageInfo (pname ++ " [FILE]") options)
                hPutStrLn stderr $ "Verbosity levels:\n\
                                   \    0: No extra information printed, just errors.\n\
                                   \    1: Ast is printed before and after passes. \n\
                                   \    2: Ast is printed after each pass. \n"
                hPutStrLn stderr showPasses

showPasses :: String
showPasses = "Available passes:" ++ (concat $ map ("\n    - " ++) $ map show listCompilerPasses)
