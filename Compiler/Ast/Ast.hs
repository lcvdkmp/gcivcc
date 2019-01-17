{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
--
-- Authors: Bjarne de Jong & Luca van der Kamp
--
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Ast.Ast (
    Ast(..),
    Program(..),
    BasicType(..),
    ReturnType(..),
    Declaration(..),
    FunctionHeader(..),
    Parameter(..),
    FunctionBody(..),
    LocalDeclaration(..),
    Statement(..),
    FunctionCall(..),
    Expression(..),
    BinaryOperator(..),
    BinaryExpression(..),
    UnaryOperator(..),
    UnaryExpression(..),
    BasicExpression(..),
    LocalFunctionDeclaration(..),
    ArrayInitializer(..),
    VariableDefinition(..),
    Origin(..),
    LiteralValue(..),
    traverse,
    traverseUntil',
    traverseM,
    traverseUntilM',
    Location(..),
    location,
    defaultLocation,
    HasLocation,
    SymbolTable(..),
    addVariableInfo,
    addFunctionInfo,
    variableInfoOf,
    functionInfoOf,
    genVarName,
    traverseWhereNotM',
    traverse'
) where

import qualified Data.List.NonEmpty
import Data.List.NonEmpty(NonEmpty)
import Data.Generics
import Data.Data
import Prelude hiding (traverse)
import Control.Lens hiding (traverse)
import Compiler.Position

data SymbolTable
        = SymbolTable
            { parent    :: Maybe SymbolTable
            , variables :: [(String, VariableDefinition)]
            , functions :: [(String, FunctionHeader)]
            }
        deriving (Show, Data, Typeable, Eq)

addVariableInfo :: SymbolTable -> String -> VariableDefinition -> Maybe SymbolTable
addVariableInfo (SymbolTable parent vars funcs) n v
    | member n vars = Nothing
    | otherwise     = Just $ SymbolTable parent (insert n v vars) funcs
    where
        member :: String -> [(String, VariableDefinition)] -> Bool
        member _ [] = False
        member s ((x, _) : xs)
            | s == x    = True
            | otherwise = member s xs
        insert :: String -> VariableDefinition -> [(String, VariableDefinition)] -> [(String, VariableDefinition)]
        insert s v xs = xs ++ [(s, v)]

variableInfoOf :: SymbolTable -> String -> Maybe VariableDefinition
variableInfoOf (SymbolTable Nothing vars _) n = lookup n vars
    where
        lookup :: String -> [(String, VariableDefinition)] -> Maybe VariableDefinition
        lookup _ [] = Nothing
        lookup s ((x, v) : xs)
            | s == x    = Just v
            | otherwise = lookup s xs
variableInfoOf (SymbolTable (Just parent) vars _) n 
    | member n vars = lookup n vars
    | otherwise     = variableInfoOf parent n
    where
        member :: String -> [(String, VariableDefinition)] -> Bool
        member _ [] = False
        member s ((x, _) : xs)
            | s == x    = True
            | otherwise = member s xs
        lookup :: String -> [(String, VariableDefinition)] -> Maybe VariableDefinition
        lookup _ [] = Nothing
        lookup s ((x, v) : xs)
            | s == x    = Just v
            | otherwise = lookup s xs

addFunctionInfo :: SymbolTable -> String -> FunctionHeader -> Maybe SymbolTable
addFunctionInfo (SymbolTable parent vars funcs) n h
    | member n funcs = Nothing
    | otherwise      = Just $ SymbolTable parent vars (insert n h funcs)
    where
        member :: String -> [(String, FunctionHeader)] -> Bool
        member _ [] = False
        member s ((x, _) : xs)
            | s == x    = True
            | otherwise = member s xs
        insert :: String -> FunctionHeader -> [(String, FunctionHeader)] -> [(String, FunctionHeader)]
        insert s v xs = xs ++ [(s, v)]

functionInfoOf :: SymbolTable -> String -> Maybe FunctionHeader
functionInfoOf (SymbolTable Nothing _ funcs) n = lookup n funcs
    where
        lookup :: String -> [(String, FunctionHeader)] -> Maybe FunctionHeader
        lookup _ [] = Nothing
        lookup s ((x, v) : xs)
            | s == x    = Just v
            | otherwise = lookup s xs
functionInfoOf (SymbolTable (Just parent) _ funcs) n
    | member n funcs = lookup n funcs
    | otherwise      = functionInfoOf parent n
    where
        member :: String -> [(String, FunctionHeader)] -> Bool
        member _ [] = False
        member s ((x, _) : xs)
            | s == x    = True
            | otherwise = member s xs
        lookup :: String -> [(String, FunctionHeader)] -> Maybe FunctionHeader
        lookup _ [] = Nothing
        lookup s ((x, v) : xs)
            | s == x    = Just v
            | otherwise = lookup s xs

genVarName :: SymbolTable -> String
genVarName (SymbolTable _ vars _) = "#" ++ show (length vars)

data Ast = Ast {program :: Program} deriving (Typeable, Data)

data Program = Program {declarations :: (NonEmpty Declaration)} deriving (Show, Typeable, Data)


data Declaration 
        = FunctionDeclaration 
            { header               :: FunctionHeader
            , _declarationLocation :: Location
            }
        | FunctionDefinition 
            { header               :: FunctionHeader
            , exported             :: Bool
            , body                 :: FunctionBody
            , _declarationLocation :: Location
            }
        | GlobalDefinition 
            { definition           :: VariableDefinition
            , exported             :: Bool
            , _declarationLocation :: Location
            }
        | GlobalDeclaration
            { definition           :: VariableDefinition
            , _declarationLocation :: Location
            }
        deriving (Show, Typeable, Data)

data Origin = Exported | Imported | Neither deriving(Eq, Show, Data, Typeable)

data VariableDefinition
        = VariableDefinition
            { typeName                    :: BasicType
            , variableName                :: String
            , initializer                 :: Maybe Expression
            , origin                      :: Origin
            , isConst                     :: Maybe Bool
            , _variableDefinitionLocation :: Location
            }
        | ArrayDefinition
            { typeName                    :: BasicType
            , variableName                :: String
            , dimensionExpressions        :: NonEmpty Expression
            , arrayInitializer            :: Maybe ArrayInitializer
            , origin                      :: Origin
            , isConst                     :: Maybe Bool
            , _variableDefinitionLocation :: Location
            }
        | ArrayDeclaration
            { typeName                    :: BasicType
            , variableName                :: String
            , dimensionIdentifiers        :: NonEmpty String
            , origin                      :: Origin
            , _variableDefinitionLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

-- | Local declarations (inside a function). Either a variable declaration
-- or a local function declaration.
data LocalDeclaration 
        = LocalDeclaration
            { declaration               :: VariableDefinition
            , _localDeclarationLocation :: Location
            } deriving (Show, Typeable, Data)

data LocalFunctionDeclaration = 
        LocalFunctionDeclaration 
            { localHeader                       :: FunctionHeader
            , localBody                         :: FunctionBody 
            , _localFunctionDeclarationLocation :: Location
            } deriving (Show, Typeable, Data)

data BasicType 
        = Bool 
            { _basicTypeLocation :: Location
            }
        | Float 
            { _basicTypeLocation :: Location
            }
        | Int 
            { _basicTypeLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data ReturnType 
        = 
        BasicType
            { basicReturnType     :: BasicType
            , _returnTypeLocation :: Location
            }
        | Void
            { _returnTypeLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data FunctionHeader 
        = FunctionHeader 
            { returnType              :: ReturnType
            , funcName                :: String
            , parameters              :: [Parameter]
            , parentFunction          :: Maybe FunctionHeader
            , functionOrigin          :: Origin
            , isPure                  :: Maybe Bool
            , _functionHeaderLocation :: Location
            } deriving (Show, Typeable, Data, Eq)

data Parameter 
        = Parameter 
            { argumentType       :: BasicType
            , argumentName       :: String
            , _parameterLocation :: Location
            }
        | ArrayParameter
            { argumentType       :: BasicType
            , argumentName       :: String
            , argumentSizeIds    :: NonEmpty String
            , _parameterLocation :: Location
            } deriving (Show, Typeable, Data, Eq)

data FunctionBody 
        = FunctionBody 
            { variableDeclarations  :: [LocalDeclaration]
            , functionDeclarations  :: [LocalFunctionDeclaration]
            , statements            :: [Statement]
            , bodySymbolTable       :: Maybe SymbolTable
            , _functionBodyLocation :: Location
            } deriving (Show, Typeable, Data)

data Statement = AssignmentStatement 
                    { targetName         :: String
                    , assigningValue     :: Expression
                    , statementTable     :: Maybe SymbolTable
                    , _statementLocation :: Location
                    }
               | ElemAssignmentStatement 
                    { elemTargetName     :: String
                    , elemSizeExprs      :: NonEmpty Expression
                    , elemAssignValue    :: Expression
                    , statementTable     :: Maybe SymbolTable
                    , _statementLocation :: Location
                    }
               | ArrayInitializationStatement
                    { targetName             :: String
                    , elementSizeExpressions :: NonEmpty Expression
                    , statementTable         :: Maybe SymbolTable
                    , _statementLocation     :: Location
                    }
               | FunctionCallStatement
                    { call               :: FunctionCall
                    , _statementLocation :: Location
                    }
               | IfStatement 
                    { condition          :: Expression
                    , ifBranch           :: [Statement]
                    , elseBranch         :: (Maybe [Statement])
                    , _statementLocation :: Location
                    }
               | WhileStatement 
                    { condition          :: Expression
                    , loopBody           :: [Statement]
                    , _statementLocation :: Location
                    }
               | DoWhileStatement 
                    { condition          :: Expression
                    , loopBody           :: [Statement]
                    , _statementLocation :: Location
                    }
               | ForStatement 
                    { counterName        :: String
                    , start              :: Expression
                    , end                :: Expression
                    , step               :: Maybe Expression
                    , loopBody           :: [Statement]
                    , _statementLocation :: Location
                    }
               | ReturnStatement 
                    { returnValue        :: Maybe Expression
                    , owningFunction     :: Maybe FunctionHeader
                    , _statementLocation :: Location
                    }
               deriving (Show, Typeable, Data)

data FunctionCall 
        = FunctionCall 
            { functionName          :: String
            , arguments             :: [Expression]
            , functionSymbolTable   :: Maybe SymbolTable
            , _functionCallLocation :: Location
            } deriving (Show, Typeable, Data, Eq)

data ArrayInitializer 
        = ArrayInitializer 
            { initializers              :: NonEmpty ArrayInitializer
            , _arrayInitializerLocation :: Location
            }
        | ArrayInitializerExpression 
            { initializerExpression     :: Expression
            , _arrayInitializerLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data Expression 
        = Expression 
            { binaryExpression    :: BinaryExpression
            , _expressionLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data BinaryExpression 
        = BinaryExpression 
            { left                      :: BinaryExpression
            , right                     :: BinaryExpression
            , binaryOperator            :: BinaryOperator
            , _binaryExpressionLocation :: Location
            }
        | PassThrough 
            { passExpression            :: UnaryExpression
            , _binaryExpressionLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data BinaryOperator 
        = NotEqual 
            { _binaryOperatorLocation :: Location
            }
        | Equal 
            { _binaryOperatorLocation :: Location
            }
        | LessThan 
            { _binaryOperatorLocation :: Location
            }
        | GreaterThan 
            { _binaryOperatorLocation :: Location
            }
        | LessEqual 
            { _binaryOperatorLocation :: Location
            }
        | GreaterEqual 
            { _binaryOperatorLocation :: Location
            }
        | Add 
            { _binaryOperatorLocation :: Location
            }
        | Subtract 
            { _binaryOperatorLocation :: Location
            }
        | Multiply 
            { _binaryOperatorLocation :: Location
            }
        | Divide 
            { _binaryOperatorLocation :: Location
            }
        | Modulus 
            { _binaryOperatorLocation :: Location
            }
        | Or
            { _binaryOperatorLocation :: Location
            }
        | And
            { _binaryOperatorLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data UnaryOperator 
        = Negate 
            { _unaryOperatorLocation :: Location
            }
        | Not 
            { _unaryOperatorLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data UnaryExpression 
        = UnaryExpression 
            { operator                 :: UnaryOperator
            , unaryValue               :: UnaryExpression
            , _unaryExpressionLocation :: Location
            }
        | CastExpression
            { castType                 :: BasicType
            , unaryValue               :: UnaryExpression
            , _unaryExpressionLocation :: Location
            }
        | UnaryPassThrough 
            { passBasicExpression      :: BasicExpression
            , _unaryExpressionLocation :: Location
            }
        deriving (Show, Typeable, Data, Eq)

data BasicExpression 
        = Literal 
            { value                    :: LiteralValue
            , _basicExpressionLocation :: Location
            }
        | ParenthesizedExpression 
            { innerExpression          :: Expression 
            , _basicExpressionLocation :: Location
            }
        | FunctionCallExpression 
            { callee                   :: FunctionCall
            , _basicExpressionLocation :: Location
            }
        | IdentifierExpression
            { identifierName           :: String
            , identifierSymbolTable    :: Maybe SymbolTable
            , _basicExpressionLocation :: Location
            }

        -- | Note that this is NOT the same as the ArrExpr definition used in
        -- the CiviC specification. ArrayExpression is an `array element access
        -- expression' (like `arr[1]'). What the specification calls an
        -- ArrExpr we call an ArrayInitializer.
        | ArrayExpression 
            { arrayId                  :: String
            , arrayElemExpr            :: NonEmpty Expression
            , arraySymbolTable         :: Maybe SymbolTable
            , _basicExpressionLocation :: Location
            } deriving (Show, Typeable, Data, Eq)

data LiteralValue
        = IntegerValue Integer
        | BooleanValue Bool
        | FloatValue   Double
        deriving (Show, Typeable, Data, Eq, Ord)

makeFields ''BasicType
makeFields ''ReturnType
makeFields ''Declaration
makeFields ''FunctionHeader
makeFields ''Parameter
makeFields ''FunctionBody
makeFields ''LocalDeclaration
makeFields ''Statement
makeFields ''FunctionCall
makeFields ''Expression
makeFields ''BinaryOperator
makeFields ''BinaryExpression
makeFields ''UnaryOperator
makeFields ''UnaryExpression
makeFields ''BasicExpression
makeFields ''LocalFunctionDeclaration
makeFields ''ArrayInitializer
makeFields ''VariableDefinition

-- Traverse the given datastructure and map every matching
-- sub-term with the given mapping function in a bottom up fashion.
traverse :: (Data a, Typeable b) => (b -> b) -> a -> a
traverse f = everywhere (mkT f)

traverse' :: (Data a, Typeable b) => (b -> b) -> a -> a
traverse' f = everywhere' (mkT f)

-- Traverse the given data structure until the predicate function return true.
-- The traversal happens in a top-down fashion
traverseUntil' :: (Data a, Typeable b, Typeable c) => (b -> Bool) -> (c -> c) -> a -> a
traverseUntil' q f = everywhereGiven' (mkQ False q) (mkT f)

-- Monadic version of traverse.
traverseM :: (Monad m, Data a, Typeable b) => (b -> m b) -> a -> m a
traverseM f = everywhereM (mkM f)

-- Monadic version of traverseWhere'.
traverseUntilM' :: (Monad m, Data a, Typeable b, Typeable c) => (b -> Bool) -> (c -> m c) -> a -> m a
traverseUntilM' q f = everywhereGivenM' (mkQ False q) (mkM f)

-- Monadic version of traverseWhere'.
traverseWhereNotM' :: (Monad m, Data a, Typeable b, Typeable c) => (b -> Bool) -> (c -> m c) -> a -> m a
traverseWhereNotM' q f = everywhereGivenM' (mkQ False q) (mkM f)

-- Map the given datastructure if the predicate function holds true.
-- Otherwise descend into it's children.
everywhereGiven' :: GenericQ Bool -> GenericT -> GenericT
everywhereGiven' q f x
    | q x       = f x
    | otherwise = f (gmapT (everywhereGiven' q f) x)

-- Monadic version of everywhereGiven'.
everywhereGivenM' :: Monad m => GenericQ Bool -> GenericM m -> GenericM m
everywhereGivenM' q f x
    | q x       = f x
    | otherwise = do
                    x' <- gmapM (everywhereGivenM' q f) x
                    f x'
