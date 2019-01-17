{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analyse.RangeCheck (
    checkRange,
    simplifyNegates

) where

import Control.Monad.State
import Compiler.Error as Error
import Compiler.Ast.Ast as Ast
import Control.Lens
import Compiler.Analyse.TypeCheck (traverseM'')

import Compiler.Config
import Data.Generics hiding (typeOf)


outOfRangeComment = "Note: the literal will still be outputted. However, some assemblers or virtual machines might not be able to handle the literal."

skipNegate :: Ast.UnaryExpression -> Ast.UnaryExpression
skipNegate (UnaryExpression {operator = Negate {}, unaryValue = 
                UnaryExpression {operator = Negate {}, unaryValue = v}}) = skipNegate v
skipNegate a = a

simplifyNegates :: Ast.Ast -> Ast.Ast
simplifyNegates = Ast.traverse' f
    where 
          f :: Ast.UnaryExpression-> Ast.UnaryExpression 
          f u@(UnaryExpression {operator = Negate {}})
                = skipNegate u
          f u = u

-- | Checks if any integer literals are most likely to be out of range for
-- some assemblers and virtual machines
checkRange :: Ast.Ast -> [Error.CompilerError]
checkRange a = execState (checkNegativeRanges $ simplifyNegates a) []
               ++ execState (checkPositiveRanges $ simplifyNegates a) []

-- The range check is split into a negative and a positive check. This
-- together with the definition of traverseM''' ensures that a positive
-- check will not also be triggered by a negative integer.
checkNegativeRanges :: Ast.Ast -> State [Error.CompilerError] Ast.Ast
checkNegativeRanges  = traverseM'' f -- It is not really required for this to ignore SymbolTables. However we do need a traverseM that traverses top-down.
        where
            f :: UnaryExpression -> State [Error.CompilerError] UnaryExpression 
            f u@(UnaryExpression {operator = Negate {}, unaryValue = UnaryPassThrough {passBasicExpression = v@(Literal {Ast.value = (IntegerValue i)}) }})
                | (i > (-intMin)) 
                   = do
                        x <- get
                        put $ x ++ [Error.Warning 
                                       { Error.error = Error.IntegerOutOfRange 
                                                           (u ^. Ast.location)
                                                           (-i)
                                                           intMin
                                        , Error.comment = outOfRangeComment
                                        }]
                        return u
                | otherwise = do return u
            f u@_ = do return u

traverseM''' :: (Monad m, Data a, Typeable b) => (b -> m b) -> a -> m a
traverseM''' = traverseWhereNotM' b
    where
        b (UnaryExpression {operator = Negate {}, unaryValue = UnaryPassThrough {passBasicExpression = Literal {}}}) = True 
        b _ = False

checkPositiveRanges :: Ast.Ast -> State [Error.CompilerError] Ast.Ast
checkPositiveRanges  = traverseM''' f -- Here we ensure we don't continue after encountering a -(literal) as to not report on negative literals.
        where
            f u@(UnaryPassThrough {passBasicExpression = v@(Literal {Ast.value = (IntegerValue i)}) })
                | (i > intMax) 
                   = do
                        x <- get
                        put $ x ++ [Error.Warning 
                                       { Error.error = Error.IntegerOutOfRange 
                                                           (u ^. Ast.location)
                                                           i
                                                           intMax
                                        , Error.comment = outOfRangeComment
                                        }]
                        return u
                | otherwise = do return u
            f u@_ = do return u

