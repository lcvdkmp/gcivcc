module Compiler.Type.HasType (
    typeOf,
    binOpTypeOf,
    uOpTypeOf,
    HasType,
    symbolFunctionType,
    symbolVariableType
) where

import qualified Compiler.Ast.Ast as Ast
import Compiler.Error
import Control.Lens
import Compiler.Type.Type
import Control.Monad

import qualified Data.List.NonEmpty as NonEmpty

-- | Get the type of a function with name n. Throws a UnknownType error
-- when a function with that name is not found in s or the symbol table is
-- nothing. l should be the location of the function
symbolFunctionType :: Maybe Ast.SymbolTable -> String -> Ast.Location -> ErrorMonad Type
symbolFunctionType Nothing _ l  = do throwError $ UnknownType l
symbolFunctionType (Just s) n l = do 
                                    case Ast.functionInfoOf s n of
                                        Just fh -> typeOf fh
                                        Nothing -> do throwError $ UnknownType l

-- | The variable equivalent of symbolFunctionType
symbolVariableType :: Maybe Ast.SymbolTable -> String -> Ast.Location -> ErrorMonad Type
symbolVariableType Nothing _ l  = do throwError $ UnknownType l
symbolVariableType (Just s) n l = do 
                                    case Ast.variableInfoOf s n of
                                        Just vd -> typeOf vd
                                        Nothing -> do throwError $ UnknownType l

class HasType a where
        -- | Should return the type of the HasType
        typeOf :: a -> ErrorMonad Type

class HasBinaryOperatorType a where
        -- | The special HasType typeclass for binary operators.
        -- To infer the type of a binary operator, the type of the left- and
        -- right-hand side must be given
        binOpTypeOf :: a -> TypeElem -> TypeElem -> ErrorMonad Type

class HasUnaryOperatorType a where
        -- | The special HasType typeclass for unary operators.
        -- The type of the operator argument must be given to infer the
        -- type of a unary operator.
        uOpTypeOf :: a -> TypeElem -> ErrorMonad Type

instance HasType a => HasType [a] where
        typeOf l = do 
                      r <- mapM typeOf l
                      return $ concat r

instance HasType Ast.BasicType where
        typeOf Ast.Bool  {} = do return [Bool 0]
        typeOf Ast.Float {} = do return [Float 0]
        typeOf Ast.Int   {} = do return [Int 0]

instance HasType Ast.ReturnType where
        typeOf Ast.BasicType {Ast.basicReturnType = t} = typeOf t
        typeOf Ast.Void      {}                        = do return [Void]

instance HasType Ast.FunctionHeader where
        typeOf Ast.FunctionHeader 
                { Ast.returnType = r, Ast.parameters = p}
            = do
                rt <- typeOf r
                pt <- typeOf $ reverse p
                return $ concat [rt, pt]

instance HasType Ast.Parameter where
        typeOf (Ast.Parameter 
                    { Ast.argumentType = at }) = typeOf at
        typeOf (Ast.ArrayParameter 
                    { Ast.argumentType = at
                    , Ast.argumentSizeIds = ids
                    }) = liftM (changeDimension (NonEmpty.length ids)) $ typeOf at

instance HasType Ast.UnaryExpression where
        typeOf Ast.UnaryExpression {Ast.operator = op, Ast.unaryValue = arg} 
            = do 
                 at <- typeOf arg
                 t <- uOpTypeOf op $ t' at
                 return t

        typeOf e@Ast.CastExpression 
                { Ast.castType = c
                , Ast.unaryValue = v
                } = do
                    vt <- typeOf v
                    ct <- typeOf c 
                    return $ concat [ct, vt]
        
        typeOf Ast.UnaryPassThrough { Ast.passBasicExpression = e } = typeOf e

instance HasType Ast.BasicExpression where
        typeOf Ast.Literal {Ast.value = v} = typeOf v
        typeOf Ast.ParenthesizedExpression {Ast.innerExpression = e} 
                    = typeOf e
        typeOf Ast.FunctionCallExpression {Ast.callee = c}
                    = typeOf c

        typeOf i@(Ast.IdentifierExpression 
                        { Ast.identifierName        = n
                        , Ast.identifierSymbolTable = tab
                        })
                    = symbolVariableType tab n $ i ^. Ast.location

        typeOf e@(Ast.ArrayExpression 
                    { Ast.arrayId          = id
                    , Ast.arraySymbolTable = tab 
                    })
                    = do 
                         t <- symbolVariableType tab id $ e ^. Ast.location
                         return $ changeDimension 0 t

instance HasType Ast.LiteralValue where
    typeOf (Ast.IntegerValue {}) = do return [Int 0]
    typeOf (Ast.FloatValue {}) = do return [Float 0]
    typeOf (Ast.BooleanValue {}) = do return [Bool 0]

instance HasType Ast.BinaryExpression where
        typeOf b@(Ast.BinaryExpression 
                    { Ast.left           = l
                    , Ast.right          = r
                    , Ast.binaryOperator = op
                    }) = do
                        rt <- typeOf r
                        lt <- typeOf l
                        ot <- binOpTypeOf op (t' rt) (t' lt)
                        return $ concat [[t' ot], [NestedType rt], [NestedType lt]]
                                                
        typeOf Ast.PassThrough { Ast.passExpression = p } = typeOf p

instance HasType Ast.Expression where
        typeOf Ast.Expression { Ast.binaryExpression = e } = typeOf e

instance HasType Ast.ArrayInitializer where
        typeOf ai@(Ast.ArrayInitializer { Ast.initializers = i })
            = do 
                 tl <- mapM (liftM t' . typeOf) $ NonEmpty.toList i
                 let ll  = fmap (^. Ast.location) $ NonEmpty.toList i
                 let t = head tl
                 let zl = zip tl ll
                 let rt = filter (not . sameDimension t) tl
                 let rtl = filter (\e -> (not . sameDimension t) $ fst e) zl

                 d <- case dimension t of
                          Nothing -> do throwError
                                            $ NonDimensionalArrayType
                                                { arrayInitializer   = ai
                                                , nonDimensionalType = [t]
                                                }
                          Just r -> do return r

                 if rt /= []
                     then do throwError
                                $ MismatchedArrayDimension
                                    { arrayInitializer = ai
                                    , assumedType = [t]
                                    , mismatchedTypes = map (\e -> ((fst e):[], snd e)) rtl
                                    }
                     else do 
                             let ttl = filter (\e -> (fst e) /= t) zl
                             if ttl /= []
                                 then do throwError
                                         $ AmbiguousArrayType
                                            { location = (NonEmpty.head i) ^. Ast.location
                                            , assumedType = [t]
                                            , encounteredTypes = map (\e -> ((fst e:[]), snd e)) ttl
                                            }
                                  else
                                      do return [changeElemDimension (d + 1) t]

        typeOf Ast.ArrayInitializerExpression { Ast.initializerExpression = e } 
            = (liftM ((:[]) . t') . typeOf) e

instance HasType Ast.FunctionCall where
        typeOf c@(Ast.FunctionCall 
                    { Ast.functionName = n
                    , Ast.functionSymbolTable = s
                    , Ast.arguments = a
                    })
            = do 
                 rt' <- symbolFunctionType s n (c  ^. Ast.location)
                 let rt = [t' rt']
                 elt <- mapM (liftM t' . typeOf) a
                 return $ concat [rt, elt]

instance HasType Ast.VariableDefinition where
        typeOf d@(Ast.VariableDefinition { Ast.typeName = t})
            = typeOf t

        typeOf Ast.ArrayDefinition { Ast.typeName = t, Ast.dimensionExpressions = e}
                    = do 
                         at <- typeOf t
                         return $ changeDimension (NonEmpty.length e) at

        typeOf Ast.ArrayDeclaration { Ast.typeName = t, Ast.dimensionIdentifiers = e} 
                    = do
                        at <- typeOf t
                        return $ changeDimension (NonEmpty.length e) at

instance HasType Ast.Statement where
        typeOf s@(Ast.AssignmentStatement 
                        { Ast.targetName = n
                        , Ast.assigningValue = v
                        , Ast.statementTable = t})
                    = do
                        assigneeT <- symbolVariableType t n $ s ^. Ast.location
                        assignmentT <- typeOf v

                        if length assigneeT == 1
                            then do return $ concat [assigneeT, assignmentT]
                            -- Should never happen (assigneeT comes from
                            -- a variable). Just in case we still throw an
                            -- error.
                            else do throwError $ NonSingletonAssignee
                                                    ( s ^. Ast.location)
                                                    assigneeT
                                                    assignmentT

instance HasBinaryOperatorType Ast.BinaryOperator where
        -- TODO: change all of these. We need some way of typeclass binding to type
        -- variables (Just like haskell e.g. Num a => a -> a -> a)
        binOpTypeOf (Ast.NotEqual {}) _ _     = do return $ [Bool 0, numberClass 0, numberClass 0]
        binOpTypeOf (Ast.Equal {}) _ _        = do return $ [Bool 0, numberClass 0, numberClass 0]
        binOpTypeOf (Ast.LessThan {}) _ _     = do return $ [Bool 0, numberClass 0, numberClass 0]
        binOpTypeOf (Ast.GreaterThan {}) _ _  = do return $ [Bool 0, numberClass 0, numberClass 0]
        binOpTypeOf (Ast.LessEqual {}) _ _    = do return $ [Bool 0, numberClass 0, numberClass 0]
        binOpTypeOf (Ast.GreaterEqual {}) _ _ = do return $ [Bool 0, numberClass 0, numberClass 0]
        binOpTypeOf (Ast.Or {}) _ _           = do return $ [Bool 0, Bool 0, Bool 0]
        binOpTypeOf (Ast.And {}) _ _          = do return $ [Bool 0, Bool 0, Bool 0]

        binOpTypeOf (Ast.Add {}) lt rt 
                         | lt == rt = do return [lt]
                         | otherwise = do return $ [TypeClass [lt, rt]]

        binOpTypeOf (Ast.Subtract {}) lt rt 
                         | lt == rt && lt == numberClass 0            = do return [lt]
                         | lt == numberClass 0 || rt == numberClass 0 = do return $ [TypeClass [lt, rt]]
                         | otherwise                                  = do return $ errorType

        binOpTypeOf (Ast.Divide {}) lt rt 
                         | lt == rt && lt == numberClass 0            = do return [lt]
                         | lt == numberClass 0 || rt == numberClass 0 = do return $ [TypeClass [lt, rt]]
                         | otherwise                                  = do return $ errorType

        binOpTypeOf (Ast.Modulus {}) lt rt 
                         | lt == rt && lt == numberClass 0            = do return [lt]
                         | lt == numberClass 0 || rt == numberClass 0 = do return $ [TypeClass [lt, rt]]
                         | otherwise                                  = do return $ errorType

        binOpTypeOf (Ast.Multiply {}) lt rt 
                         | lt == rt  = do return [lt]
                         | otherwise = do return $ [TypeClass [lt, rt]]

instance HasUnaryOperatorType Ast.UnaryOperator where
        -- Again, it would be really nice to have type variables here.
        uOpTypeOf (Ast.Negate {}) at 
                        | at == numberClass 0 = do return $ [at, at]
                        | otherwise           = do return $ [at, numberClass 0]

        uOpTypeOf (Ast.Not {}) _ = do return $ [Bool 0, Bool 0]
