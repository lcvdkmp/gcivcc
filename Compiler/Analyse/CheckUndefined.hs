{-# LANGUAGE FlexibleContexts #-}
module Compiler.Analyse.CheckUndefined (
    checkUndefined
    , checkUndefinedE
) where


import Compiler.Ast.Ast
import Control.Lens ((^.))
import Compiler.Type (t', typeOf, uOpTypeOf)
import qualified Compiler.Error as Error

import Compiler.Utils (foldrMr)
import Compiler.Type.HasType (symbolFunctionType, symbolVariableType)

import Data.Generics

traverseM'' :: (Monad m, Data a, Typeable b) => (b -> m b) -> a -> m a
-- | A special traverse that ensures we don't traverse symbol tables.
traverseM'' = traverseWhereNotM' b
    where
        b (SymbolTable {}) = True 

-- | Check if a variable is defined. If it isn't throw an error.
isDefinedV :: HasLocation a Location => a -> String -> Maybe SymbolTable -> Error.ErrorMonad a
isDefinedV l i Nothing = do Error.throwError $ Error.UndefinedVariable
                                                        { Error.location = (l ^. location)
                                                        , Error.identifier = i
                                                        }
isDefinedV l i (Just t) = do 
                          case variableInfoOf t i of
                              Just _ -> do return l
                              Nothing -> do Error.throwError $ Error.UndefinedVariable 
                                                            { Error.location = (l ^. location)
                                                            , Error.identifier = i
                                                            }

-- | Check is a function is defined. If it isn't throw an error.
isDefinedF :: HasLocation a Location => a -> String -> Maybe SymbolTable -> Error.ErrorMonad a
isDefinedF l i Nothing = do Error.throwError $ Error.UndefinedFunction
                                                        { Error.location = (l ^. location)
                                                        , Error.identifier = i
                                                        }
isDefinedF l i (Just t) = do 
                          case functionInfoOf t i of
                              Just _ -> do return l
                              Nothing -> do Error.throwError $ Error.UndefinedFunction
                                                            { Error.location = (l ^. location)
                                                            , Error.identifier = i
                                                            }
checkUndefinedS :: Ast -> Error.ErrorMonad Ast
checkUndefinedS ast = traverseM'' f ast
    where
        f e@(AssignmentStatement {targetName = n, statementTable = t}) = isDefinedV e n t
        f e@(ElemAssignmentStatement {elemTargetName = n, statementTable = t}) = isDefinedV e n t
        f e = do return e

        b (SymbolTable {}) = False

checkUndefinedF :: Ast -> Error.ErrorMonad Ast
checkUndefinedF ast = traverseM'' f ast
    where
        f e@(FunctionCall {functionName = n, functionSymbolTable = t}) = isDefinedF e n t
        b (SymbolTable {}) = False


checkUndefinedE :: Ast -> Error.ErrorMonad Ast
checkUndefinedE ast = traverseM'' f ast
    where
        f e@(IdentifierExpression {identifierName = n, identifierSymbolTable = t}) = isDefinedV e n t
        f e@(ArrayExpression {arrayId = n, arraySymbolTable = t}) = isDefinedV e n t
        f e = do return e

        b (SymbolTable {}) = False

-- | Check an Ast on undefined variables and functions.
checkUndefined :: Ast -> Error.ErrorMonad Ast
checkUndefined ast = foldrMr ($) l ast
        where
            l = [ checkUndefinedS
                , checkUndefinedF
                , checkUndefinedE
                ]
