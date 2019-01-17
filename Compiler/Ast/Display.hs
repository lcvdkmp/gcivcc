-- {-# LANGUAGE FlexibleInstances #-}

module Compiler.Ast.Display (display, toTree) where

import Compiler.Ast.Ast

import Data.List (intercalate, map)
import qualified Data.List.NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (maybe)
import Text.Printf

import Data.Tree (Tree(..), drawTree)

-- Print an ast as a tree.
--

-- TODO: currently empty lists are also displated (as ": ()"). 
--       find a way to make this disappear
--
instance Show Ast where
        show a = display a

display :: Ast -> String
display a = drawTree $ toTree a

class (Show a) => ToTree a where
        toTree :: a -> Tree String
        toTree t = Node (nodeTag t) $ childTrees t

        -- | Should return the children of the node
        childTrees :: a -> [Tree String]
        childTrees _ = []

        -- | Chould return a list of attribute name - attribute value pairs
        -- TODO: find a way to print this in a nicely indented block
        attributes :: a -> [(String, String)]
        attributes _ = []

        nodeTag :: a -> String
        nodeTag n = printf "%s: (%s)" 
                        (nodeName n)
                        (intercalate " " $ map print' $ attributes n)
                        where
                            print' :: (String, String) -> String
                            print' (attr, val) = printf "%s: %s" attr val

        -- | Should return the node name
        nodeName :: a -> String
        nodeName a = show a
                    
instance (ToTree a, Show a) => ToTree [a] where
        childTrees [] = []
        childTrees l  = map toTree l

        nodeName (h:t) = printf "%s list" $ nodeName h
        nodeName _     = ""


instance ToTree Ast where
        childTrees (Ast p) = [toTree p]

        nodeName _ = "Ast"

instance ToTree Program where
        childTrees (Program d) = map toTree $ Data.List.NonEmpty.toList d

        nodeName _ = "Program"

instance ToTree Declaration where
        attributes (FunctionDefinition { exported = e})               = [("Exported", show e)]
        attributes (FunctionDeclaration {})                           = [("extern", show True)]
        attributes (GlobalDeclaration {definition = d})               = attributes d
        attributes (GlobalDefinition { definition = d, exported = e}) = attributes d ++ [("Exported", show e)]

        childTrees (FunctionDeclaration {header = h})          = [toTree h]
        childTrees (FunctionDefinition {header = h, body = b}) = [toTree h, toTree b]

        childTrees (GlobalDefinition {definition = d}) = childTrees d
        childTrees _                                   = []


        nodeName (FunctionDefinition {})  = "Function definition"
        nodeName (GlobalDeclaration {})   = "Global declaration"
        nodeName (GlobalDefinition {})    = "Global definition"
        nodeName (FunctionDeclaration {}) = "Function declaration"

instance ToTree VariableDefinition where
        attributes (VariableDefinition
                        { typeName = t
                        , variableName = v
                        , isConst = c
                        }
                    ) = [("Type", show t), ("Name", v), ("const", show $ any id c)]
        attributes (ArrayDefinition
                        { typeName = t
                        , variableName = v
                        , isConst = c
                        }
                   ) = [("Type", show t), ("Name", v), ("const", show $ any id c)]
        attributes (ArrayDeclaration
                        {typeName = t
                        , variableName = v
                        , dimensionIdentifiers = ids
                        }
                   ) = [("Type", show t), ("Name", v), ("Size identifiers", show ids)]

        childTrees (VariableDefinition {initializer = (Just i)}) = [toTree i]
        childTrees (ArrayDefinition 
                        { dimensionExpressions = e
                        , arrayInitializer = (Just a)}
                   )                                             = [toTree a] ++ (map toTree $ Data.List.NonEmpty.toList e)
        childTrees (ArrayDefinition {dimensionExpressions = e})  = map toTree $ Data.List.NonEmpty.toList e
        childTrees _                                             = []

        nodeName (VariableDefinition {}) = "Variable definition"
        nodeName (ArrayDefinition {})    = "Array definition"
        

instance ToTree FunctionHeader where
        attributes (FunctionHeader 
                        { returnType = t
                        , funcName = n
                        , parentFunction = p
                        , functionOrigin = o
                        } 
                    ) =
                        [ ("Name", n)
                        , ("Type", show t)
                        , ("Parent function", maybe "#Global#" funcName p)
                        , ("Origin", show o)
                        ]

        childTrees (FunctionHeader { parameters = p}) = map toTree p

        nodeName _ = "Function header"

instance ToTree Parameter where
        attributes (Parameter 
                        { argumentType = t
                        , argumentName = n
                        }
                    ) =
                        [ ("Name", n)
                        , ("Type", show t)
                        ]
        attributes (ArrayParameter 
                        { argumentType = t
                        , argumentName = n
                        , argumentSizeIds = s
                        }
                    ) =
                        [ ("Name", n)
                        , ("Type", show t)
                        , ("Size IDs", show s)
                        ]
        nodeName (Parameter {})      = "Parameter"
        nodeName (ArrayParameter {}) = "parameter"

instance ToTree FunctionBody where
        childTrees (FunctionBody 
                        { variableDeclarations = v
                        , functionDeclarations = f
                        , statements = s
                        }
                    ) = [toTree v, toTree f, toTree s]

        nodeName (FunctionBody {}) = "Function body"

instance ToTree Expression where
        attributes (Expression {binaryExpression = e}) = []

        childTrees (Expression {binaryExpression = e}) = childTrees e
        nodeName (Expression {}) = "Expression"

instance ToTree BinaryExpression where
        childTrees (BinaryExpression 
                        { left = l
                        , right = r
                        }
                    ) =
            [toTree l, toTree r]

        childTrees (PassThrough { passExpression = u }) = [toTree u]

        attributes (BinaryExpression { binaryOperator = o }) =
            [("Binary operator", show o)]

        attributes (PassThrough { passExpression = u }) = attributes u

        nodeName (BinaryExpression {}) = "Binary expression"
        nodeName (PassThrough {})      = "Passthrough"

instance ToTree UnaryExpression where
        childTrees (UnaryExpression { unaryValue = e })           = [toTree e]
        childTrees (CastExpression { unaryValue = e })            = [toTree e]
        childTrees (UnaryPassThrough { passBasicExpression = e }) = [toTree e]

        attributes (UnaryExpression  { operator = o }) = [("Operator", show o)]
        attributes (CastExpression { castType = t })   = [("Type", show t)]
        attributes _                                   = []

        nodeName (UnaryExpression {})  = "Unary expression"
        nodeName (CastExpression {})   = "Cast expression"
        nodeName (UnaryPassThrough {}) = "Unary Passthrough"

instance ToTree BasicExpression where
        attributes (Literal {value = v}) = [("value", show v)]
        attributes (IdentifierExpression 
                        { identifierName = v
                        , identifierSymbolTable = t
                        }) = [ ("value", v) ]
        attributes (ArrayExpression 
                        { arrayId = i
                        , arraySymbolTable = t
                        }) = [ ("id", i) ]
        attributes _ = []

        childTrees (ParenthesizedExpression {innerExpression = i}) = [toTree i]
        childTrees (FunctionCallExpression {callee = f})           = [toTree f]
        childTrees (ArrayExpression {arrayElemExpr = e})           = [toTree $ Data.List.NonEmpty.toList e]
        childTrees _ = []

        nodeName (Literal {value = IntegerValue _}) = "Integer literal"
        nodeName (Literal {value = FloatValue _})   = "Float literal"
        nodeName (Literal {value = BooleanValue _}) = "Boolean literal"
        nodeName (IdentifierExpression {})          = "Identifier expression"
        nodeName (ParenthesizedExpression {})       = "Parenthesized expression"
        nodeName (FunctionCallExpression {})        = "Function call expression"
        nodeName (ArrayExpression {})               = "Array expression"

instance ToTree FunctionCall where
        attributes (FunctionCall 
                        { functionName = n
                        , functionSymbolTable = t
                        }) = [ ("name", n) ]
        childTrees (FunctionCall {arguments =  e}) = [toTree e]

        nodeName (FunctionCall {}) = "Function call "


instance ToTree ArrayInitializer where
        childTrees (ArrayInitializer {initializers = i})                    = [toTree $ Data.List.NonEmpty.toList i]
        childTrees (ArrayInitializerExpression {initializerExpression = e}) = [toTree e]

        nodeName (ArrayInitializer {})           = "Array initializer"
        nodeName (ArrayInitializerExpression {}) = "Array initializer expression"

instance ToTree LocalDeclaration where
        attributes (LocalDeclaration {declaration = d}) = attributes d

        childTrees (LocalDeclaration {declaration = d}) = childTrees d

        nodeName (LocalDeclaration {declaration = d}) = nodeName d


instance ToTree LocalFunctionDeclaration where
        childTrees (LocalFunctionDeclaration {localHeader = h, localBody =  b}) = [toTree h, toTree b]

        nodeName (LocalFunctionDeclaration {}) = "Local function declaration"

instance ToTree Statement where
        attributes (AssignmentStatement 
                        {targetName = s
                        , statementTable = t
                        }) = [("Target nodeName", s) ]
        attributes (ElemAssignmentStatement 
                        { elemTargetName = s
                        , statementTable = t
                        }) = [("Target nodeName", s) ]

        attributes (ArrayInitializationStatement {targetName=d, statementTable=s}) = [("Target", d)]
        attributes (ForStatement {counterName = c})                                = [("Counter nodeName", c)]
        attributes _                                                               = []

        childTrees (AssignmentStatement { assigningValue = v }) = [toTree v]

        childTrees (ElemAssignmentStatement {elemSizeExprs = e,  elemAssignValue = v}) = 
            [toTree $ Data.List.NonEmpty.toList e, toTree v]

        childTrees (ArrayInitializationStatement {elementSizeExpressions = e})
                = [toTree $ Data.List.NonEmpty.toList e]

        childTrees (FunctionCallStatement {call = c}) = [toTree c]
        childTrees s@(IfStatement 
                        { condition = c
                        , ifBranch = i
                        , elseBranch = (Just l)
                        }
                      ) = 
                    [toTree c, toTree i] 
            ++ (childTrees $ s {elseBranch = Nothing})
        childTrees (IfStatement 
                        { condition = c
                        , ifBranch = i
                        }
                    ) = [toTree c, toTree i]
        childTrees (WhileStatement 
                        { condition = c
                        , loopBody = b
                        }
                    ) = [toTree c, toTree b]
        childTrees (DoWhileStatement 
                        { condition = c
                        , loopBody = b
                        }
                    ) = [toTree c, toTree b]
        childTrees f@(ForStatement {step = (Just ex)}) = 
            childTrees (f {step = Nothing})
            ++ [toTree ex]
        childTrees (ForStatement 
                        { start = s
                        , end = e
                        , step = Nothing
                        , loopBody = b
                        }
                    ) = 
                        [ toTree s
                        , toTree b
                        , toTree e
                        , toTree b]
        childTrees (ReturnStatement {returnValue = (Just e)}) = [toTree e]
        childTrees _ = []

        nodeName (AssignmentStatement {})           = "Assignment statement"
        nodeName (ElemAssignmentStatement {})       = "Element Assignment statement"
        nodeName (ArrayInitializationStatement {})  = "Array initialization statement"
        nodeName (ForStatement {})                  = "For statement"
        nodeName (FunctionCallStatement {})         = "Function call statement"
        nodeName (IfStatement {})                   = "If statement"
        nodeName (WhileStatement {})                = "While statement"
        nodeName (DoWhileStatement {})              = "Do while statement"
        nodeName (ReturnStatement {})               = "Return statement"
