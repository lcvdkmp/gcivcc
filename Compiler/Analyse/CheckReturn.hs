{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analyse.CheckReturn (
    checkReturns,
) where
import Control.Lens
import Data.Generics hiding (typeOf)
import qualified Compiler.Error as Error
import Compiler.Analyse.TypeCheck (traverseM'')
import Compiler.Ast.Ast



-- Check if all non-void functions end in a return statement.
-- Note that the type of the return statement is not checked here. This is
-- automatically handled by the typechecker.
--
-- TODO: this is actually a bit too strict.
-- Every possible control flow in a non-void function should end with a return and
-- checking if the last statement of every non-void function is a return
-- statement does enforce this. However is the last statement is an
-- if-then-else block for example, each control flow could still reach
-- a return without the last statement of the function being a return
-- itself.
checkReturns :: Ast -> Error.ErrorMonad Ast
checkReturns ast = checkGlobalReturns ast >> checkLocalReturns ast

checkGlobalReturns :: Ast -> Error.ErrorMonad Ast
checkGlobalReturns = traverseM f
    where
        -- Void functions don't require returns.
        f c@(FunctionDefinition { header = FunctionHeader { returnType = Void {}}})
            = do return c
        f c@(FunctionDefinition {body = b, header = FunctionHeader {funcName = n}})
            | hasReturn b = do return c
            | otherwise   = do Error.throwError 
                              $ Error.MissingReturn
                                  { Error.location = l (statements b) c
                                  , Error.functionLocation = c ^. location
                                  , Error.functionName = n
                                  }
            where
                l :: [Statement] -> Declaration -> Location
                l [] c = c ^. location
                l ll _ = (last ll) ^. location
        f c@_ = do return c

checkLocalReturns:: Ast -> Error.ErrorMonad Ast
checkLocalReturns = traverseM f
    where
        -- Void functions don't require returns.
        f c@(LocalFunctionDeclaration { localHeader = FunctionHeader { returnType = Void {}}})
            = do return c
        f c@(LocalFunctionDeclaration {localBody = b, localHeader = FunctionHeader {funcName = n}})
            | hasReturn b = do return c
            | otherwise   = do Error.throwError 
                              $ Error.MissingReturn
                                  { Error.location = l (statements b) c
                                  , Error.functionLocation = c ^. location
                                  , Error.functionName = n
                                  }
            where
                l :: [Statement] -> LocalFunctionDeclaration -> Location
                l [] c = c ^. location
                l ll _ = (last ll) ^. location

-- | Returns whether or not a function body ends with a return
hasReturn :: FunctionBody -> Bool
hasReturn FunctionBody { statements = [] } 
          = False
hasReturn FunctionBody { statements = s } 
          = isReturn $ last s
          where 
                isReturn :: Statement -> Bool
                isReturn ReturnStatement {} = True
                isReturn _ = False
