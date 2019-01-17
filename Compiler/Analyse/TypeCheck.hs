{-# LANGUAGE FlexibleContexts #-}
module Compiler.Analyse.TypeCheck (
    typeCheck,
    typeCheckA,
    typeCheckMain,
    traverseM'',
) where

import Control.Lens
import Control.Monad
import Data.Generics hiding (typeOf)
import Data.Either (isLeft)

import Compiler.Utils (foldrMr)
import Compiler.Type.HasType (symbolFunctionType, symbolVariableType)
import Compiler.Ast.Ast
import Compiler.Type (t', typeOf, uOpTypeOf, binOpTypeOf)

import qualified Compiler.Type as Type
import qualified Compiler.Error as Error
import qualified Data.List.NonEmpty as NonEmpty


traverseM'' :: (Monad m, Data a, Typeable b) => (b -> m b) -> a -> m a
-- | A special traverse that ensures we don't traverse symbol tables.
traverseM'' = traverseWhereNotM' b
    where
        b (SymbolTable {}) = True 


-- | Get the left value of an Either. If Either is Right, error
fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: not left"

-- | Expect type 't' on 'other'. Thows the appropriate error when this is
-- not the cae
expectType :: (Type.HasType a, HasLocation a Location) => Type.Type -> a -> Error.ErrorMonad a
expectType t other
    = do
        t1 <- Type.typeOf other
        if t == t1
            then do return other
            else do Error.throwError 
                        $ Error.UnexpectedType
                            { Error.location = other ^. location
                            , Error.expectedType = t
                            , Error.actualType = t1
                            }

-- | The same as expectType except that the type of 'other' is induced
-- using t' (the final type of 'other' is used). This is useful for example when the
-- result of a function call should be checked in a statement
expectType' :: (Type.HasType a, HasLocation a Location) => Type.Type -> a -> Error.ErrorMonad a
expectType' t other
    = do
        t1 <- (liftM ((:[]) . t') . Type.typeOf) other
        if t == t1
            then do return other
            else do Error.throwError 
                        $ Error.UnexpectedType
                            { Error.location = other ^. location
                            , Error.expectedType = t
                            , Error.actualType = t1
                            }

-- | Expect 'other' to have the type of one of the final types of 't'.
-- These final types are induced with t'
expectTypeOneOf' :: (Type.HasType a, HasLocation a Location) => [Type.Type] -> a -> Error.ErrorMonad a
expectTypeOneOf' t other
    = do
        t1 <- (liftM ((:[]) . t') . Type.typeOf) other
        if elem t1 t
            then do return other
            else do Error.throwError
                        $ Error.UnexpectedTypeOneOf
                            { Error.location = other ^. location
                            , Error.expectedTypes = t
                            , Error.actualType = t1
                            }

-- | The same as expectType' except that this time we already have a type and
-- a location and we want to see if that type matches the type of 'this' (t' is still applied to the
-- type of 'this').
expectTypeT' :: (Type.HasType a) => a -> Type.Type -> Location -> Error.ErrorMonad a
expectTypeT' this t tl
    = do
        t1 <- (liftM ((:[]) . t') . Type.typeOf) this
        if t == t1
            then do return this
            else do Error.throwError 
                        $ Error.UnexpectedType
                            { Error.location = tl
                            , Error.expectedType = t1
                            , Error.actualType = t
                            }

-- | The typeCheck traversal doesn't really transform the Ast, instead it
-- checks for type errors.
typeCheck :: Ast -> Error.ErrorMonad Ast
typeCheck = foldrMr ($) tcl
        where
            tcl = [ typeCheckDefinitions
                  , typeCheckBasicExpressions
                  , typeCheckUnaryExpressions
                  , typeCheckBinaryExpressions
                  , typeCheckStatements
                  ]

-- | Wraps a LocalDeclaration in a dummy Ast
wrapAstLD :: LocalDeclaration -> Ast
wrapAstLD l = Ast $ Program $ (fd l) NonEmpty.:| []
    where
        fd :: LocalDeclaration -> Declaration
        fd l = FunctionDefinition fh False (fb l) defaultLocation
            where
                fh :: FunctionHeader
                fh = FunctionHeader (Void defaultLocation) "#wrap" [] Nothing Neither Nothing defaultLocation
                fb :: LocalDeclaration -> FunctionBody
                fb l = FunctionBody [l] [] [] Nothing defaultLocation

wrapAstFD :: LocalFunctionDeclaration -> Ast
wrapAstFD l = Ast $ Program $ (fd l) NonEmpty.:| []
    where
        fd :: LocalFunctionDeclaration -> Declaration
        fd l = FunctionDefinition fh False (fb l) defaultLocation
            where
                fh :: FunctionHeader
                fh = FunctionHeader (Void defaultLocation) "#wrap" [] Nothing Neither Nothing defaultLocation
                fb :: LocalFunctionDeclaration -> FunctionBody
                fb l = FunctionBody [] [l] [] Nothing defaultLocation

wrapAstS :: Statement -> Ast
wrapAstS l = Ast $ Program $ (fd l) NonEmpty.:| []
    where
        fd :: Statement -> Declaration
        fd l = FunctionDefinition fh False (fb l) defaultLocation
            where
                fh :: FunctionHeader
                fh = FunctionHeader (Void defaultLocation) "#wrap" [] Nothing Neither Nothing defaultLocation
                fb :: Statement -> FunctionBody
                fb l = FunctionBody [] [] [l] Nothing defaultLocation

wrapAstD :: Declaration -> Ast
wrapAstD d = Ast $ Program $ d NonEmpty.:| []


-- | Aggregate version of typeCheck. Will print multiple errors
typeCheckA :: Ast -> Error.ErrorMonad Ast
typeCheckA = traverseM'' f
-- TODO: a lot of duplicate code here
    where
        f :: Program -> Error.ErrorMonad Program
        f p@(Program { declarations = d }) = do
                let ve = map typeCheckDeclarationA $ map wrapAstD $ NonEmpty.toList d
                let r = filter isLeft ve
                if length r == 0
                    then do return p
                    else do Error.throwError $ Error.Aggregate $ map fromLeft r


typeCheckDeclarationA :: Ast -> Error.ErrorMonad Ast
typeCheckDeclarationA = traverseM'' f
    where
        f :: Declaration -> Error.ErrorMonad Declaration 
        f d@(FunctionDefinition { body = (FunctionBody 
                                            { variableDeclarations = vd
                                            , functionDeclarations = fd
                                            , statements = s
                                            })}) = do 
                        let ve = map typeCheck $ concat [ map wrapAstLD vd
                                                        , map wrapAstFD fd
                                                        , map wrapAstS s
                                                        ]
                        let r = filter isLeft ve
                        if length r == 0
                            then do return d
                            else do Error.throwError $ Error.Aggregate $ map fromLeft r

        f d@(GlobalDefinition {}) = do
                                        typeCheck $ wrapAstD d
                                        return d
        f d@(GlobalDeclaration {}) = do
                                        typeCheck $ wrapAstD d
                                        return d
        f d = do return d

-- | Typecheck VariableDefinitions
typeCheckDefinitions :: Ast -> Error.ErrorMonad Ast
typeCheckDefinitions = traverseM'' f 
    where
        f e@(VariableDefinition 
                { typeName = t
                , initializer = Just i 
                }
            ) =  do
                t1 <- typeOf t
                -- We expect the final type of the initializer to be the same as the typeName 
                expectType' t1 i
                return e

        f e@(ArrayDefinition
                { dimensionExpressions = expr
                , arrayInitializer = Nothing
                }
            ) = do
                -- We expect the final type of the dimension expressions to
                -- be int
                mapM_ (expectType' [Type.Int 0] ) expr
                return e

        f e@(ArrayDefinition
                { typeName = tn
                , dimensionExpressions = expr
                , arrayInitializer = Just i
                }
            ) = do 
                mapM_ (expectType' [Type.Int 0]) expr
                let d = length expr
                t <- (liftM ((:[]) . Type.changeElemDimension d . t') . typeOf) tn

                -- We expect the type of the initializer to be the same
                -- type as the array. To allow scalar promotion we also
                -- allow the array type with a scalar dimensionality
                expectTypeOneOf' [t, Type.changeDimension 0 t] i
                return e
        
        f e@(ArrayDeclaration { dimensionIdentifiers = i})
            = do 
                 return e

        f e = do return e

typeCheckStatements :: Ast -> Error.ErrorMonad Ast
typeCheckStatements = traverseM'' f
    where
        f e@(AssignmentStatement { targetName = n
                                 , assigningValue = v
                                 , statementTable = t })
            = do 
                 tt <- symbolVariableType t n (e ^. location) 
                 expectType' tt v
                 return e

        f e@(ElemAssignmentStatement 
                { elemSizeExprs = s
                , elemTargetName = n
                , elemAssignValue = v
                , statementTable = t
                })
            = do
                 mapM_ (expectType' [Type.Int 0]) s
                 tt <- liftM t' $ symbolVariableType t n (e ^. location)

                 -- First, we will check if the statement has the proper
                 -- number of size expressions
                 let td = Type.changeElemDimension 0 tt

                 let trd = Type.changeElemDimension (length s) td


                 if trd /= tt
                     then do
                         -- We can assume the type of the array has
                         -- a dimension
                         let Just dm = Type.dimension tt
                         Error.throwError $ Error.IncorrectIndiceCount
                            { Error.location = e ^. location
                            , Error.expectedCount = dm
                            , Error.actualCount = length s
                            }
                     else do 
                        -- Next, we check if the assignment expression matches the type of
                        -- the array
                        expectType' [td] v
                        return e

        f e@(FunctionCallStatement {call = fc@FunctionCall 
                                                { functionName = n
                                                , arguments = a
                                                , functionSymbolTable = s
                                                }
                                   })
            = do
                eft <- liftM tail $ symbolFunctionType s n (fc ^. location)
                elt <- mapM (liftM t' . typeOf) a
                
                -- We reverse eft here since the parameters of a function
                -- occur in reverse order in the type of a function
                -- For more info see Compuler/Type/Type.hs
                if reverse eft == elt
                    then do return e
                    else do Error.throwError 
                                $ Error.MismatchedParameterTypes
                                    { Error.location = (e ^. location)
                                    , Error.expectedType = eft
                                    , Error.actualType = elt
                                    }
                return e

        f e@(IfStatement {condition = c})
            = do
                expectType' [Type.Bool 0] c
                return e

        f e@(WhileStatement {condition = c})
            = do
                expectType' [Type.Bool 0] c
                return e

        f e@(DoWhileStatement {condition = c})
            = do
                expectType' [Type.Bool 0] c
                return e

        f e@(ForStatement {start = s, end = en, step = Just st})
            = do
                -- All types (step, begin and end) in a for loop must be Int 0
                expectType' [Type.Int 0] s
                expectType' [Type.Int 0] en
                expectType' [Type.Int 0] st
                return e

        f e@(ForStatement {start = s, end = en, step = Nothing})
            = do
                expectType' [Type.Int 0] s
                expectType' [Type.Int 0] en
                return e

        f e@(ReturnStatement {owningFunction = Nothing})
            = do Error.throwError $ Error.OrphanReturn {Error.location = (e ^. location)}

        f e@(ReturnStatement {returnValue = r, owningFunction = Just f})
            = do 
                rt <- typeOfM r
                case rt of
                    Just s -> do 
                                 let Just rv = r
                                 expectTypeT' f ((:[]) $ t' s) (rv ^. location)
                                 return e
                    Nothing -> do expectTypeT' f [Type.Void] (e ^. location)
                                  return e

                    where
                        typeOfM :: (Type.HasType a) => Maybe a -> Error.ErrorMonad (Maybe Type.Type)
                        typeOfM Nothing = do return Nothing
                        typeOfM (Just t) = do 
                                              t' <- typeOf t
                                              return $ Just t'

typeCheckUnaryExpressions :: Ast -> Error.ErrorMonad Ast
typeCheckUnaryExpressions = traverseM'' f
    where
        f c@(CastExpression
                { castType = ct
                , unaryValue = v
                }) = do
                    tt <- typeOf v
                    let t = [t' tt]
                    rct <- typeOf ct
                    case (rct, t) of 
                        (rct@[Type.Void], ct) -> do Error.throwError $ dc ct rct
                        (rct, ct@[Type.Void]) -> do Error.throwError $ dc ct rct
                        (rct, _) -> do return c
                        where
                            dc ct rct = Error.DisallowedCast
                                    { Error.location = (c ^. location)
                                    , Error.fromType = rct
                                    , Error.toType = ct
                                    }

        f c@(UnaryExpression
            { operator = op
            , unaryValue = v
            }) = do
                vt <- liftM t' $ typeOf v
                top <- uOpTypeOf op vt
                expectType' (tail top) v
                return c
        f c@(_) = do return c

typeCheckBinaryExpressions :: Ast -> Error.ErrorMonad Ast
typeCheckBinaryExpressions = traverseM'' f
    where
        -- And and Or need special treatment since they only work on
        -- specific types
        f e@(BinaryExpression 
                { left = l
                , right = r
                , binaryOperator = op@(Or {})
                }) = do
                    lt' <- typeOf l
                    rt' <- typeOf r

                    -- First we check if the left and right hand arguments
                    -- fit the operator
                    ot <- binOpTypeOf op (t' rt') (t' rt')
                    let [ro,rto, lto] = ot

                    expectType' [rto] r
                    expectType' [lto] l
                    return e
        f e@(BinaryExpression 
                { left = l
                , right = r
                , binaryOperator = op@(And {})
                }) = do
                    lt' <- typeOf l
                    rt' <- typeOf r

                    -- First we check if the left and right hand arguments
                    -- fit the operator
                    ot <- binOpTypeOf op (t' rt') (t' rt')
                    let [ro,rto, lto] = ot

                    expectType' [rto] r
                    expectType' [lto] l
                    return e

        f e@(BinaryExpression 
                { left = l
                , right = r
                , binaryOperator = op
                }) = do
                    lt' <- typeOf l
                    rt' <- typeOf r
                    let (lt, rt) = ([t' lt'], [t' rt'])


                    -- The allowed type combinations in binary expressions
                    -- are very simple. Only type combinations that are the
                    -- same are allowed.
                    
                    if lt == rt
                        then do return e
                        else do Error.throwError 
                                $ Error.DisallowedTypeCombination
                                (op ^. location)
                                [(lt, l ^. location), (rt, r ^. location)]
                                (Type.BinaryCombiner op)
        f e@(_) = do return e

-- Typecheck variables
typeCheckVariables :: Ast -> Error.ErrorMonad Ast
typeCheckVariables = traverseM'' f
    where
        -- Note here that we cannot match variables directly. We need
        -- a symbol table, which is only present at function headers and
        -- function calls
        f fd@(FunctionDefinition {header = h, body = b})
            = do return fd

typeCheckBasicExpressions :: Ast -> Error.ErrorMonad Ast
typeCheckBasicExpressions = traverseM'' f
    where
        f be@(ArrayExpression { arrayId          = id
                              , arrayElemExpr    = e
                              , arraySymbolTable = t
                              })
                    = do 
                         -- Check if the correct number of indices are
                         -- supplied
                        tt <- liftM t' $ symbolVariableType t id (be ^. location)

                        let td = Type.changeElemDimension 0 tt
                        let trd = Type.changeElemDimension (length e) td


                        when (trd /= tt) $ do
                                -- We can assume the type of the array has
                                -- a dimension
                                let Just dm = Type.dimension tt
                                Error.throwError $ Error.IncorrectIndiceCount
                                    { Error.location = be ^. location
                                    , Error.expectedCount = dm
                                    , Error.actualCount = length e
                                    }

                        -- Check if the indices are of type int
                        mapM_ (expectType' [Type.Int 0]) $ NonEmpty.toList e
                        return be

        f be = do return be

typeCheckMain :: Ast -> Error.ErrorMonad Ast
typeCheckMain= traverseM'' f
    where
        f be@(FunctionDefinition {header = h@(FunctionHeader { funcName = "main"}), exported = ex }) 
                    = do 
                        expectType [Type.Int 0] h
                        if ex == True 
                            then do return be
                            else do Error.throwError
                                    $ Error.UnexportedMain 
                                    (h ^. location)

        f be = do return be
