-- |
-- Define all compiler passes here.
--
--
module Compiler.Pass.Pass (
    Pass,
    CompilerPass(..),
    pass,
    listCompilerPasses,
   passAndPrint
) where

import Compiler.Ast.Ast
import Compiler.Pass.LoopRewriter
import Compiler.Pass.Initialization
import Compiler.Pass.ArrayRewriter
import Compiler.Pass.ConstantFolder
import Compiler.Ast.Display
import Compiler.Ast.Print

listCompilerPasses :: [CompilerPass]
listCompilerPasses = [(minBound :: CompilerPass) ..]

class Pass f where   
    pass :: Ast -> f -> Ast


data CompilerPass = ArrayRewrite
                  | Initializers
                  | LoopRewrite
                  -- | FoldConstants
                  deriving (Read, Show, Enum, Bounded, Eq, Ord)

instance Pass CompilerPass where
    pass ast ArrayRewrite = rewriteArrays ast
    pass ast LoopRewrite = rewriteLoops ast
    pass ast Initializers = rewriteInitializers ast
    -- pass ast FoldConstants = foldConstants ast

passAndPrint :: (Pass a, Show a) => Ast -> a -> IO Ast
passAndPrint as p = do
        let np = pass as p
        putStrLn $ replicate 80 '='
        putStrLn $ "After pass " ++ show p ++ ":"
        putStrLn $ display np
        putStrLn $ replicate 80 '='
        putStrLn $ programPrint np
        putStrLn $ replicate 80 '='
        return np
