{-# LANGUAGE FlexibleInstances #-}

module Compiler.Ast.Print (
    programPrint
) where

import Compiler.Ast.Ast

import Data.List (intercalate)
import qualified Data.List.NonEmpty
import Data.List.NonEmpty (NonEmpty)

import Text.Printf (printf)

-- | A ProgramPrintable is a type that can be transformed to valid
-- (CiviC) program source
-- TODO: indent the printed program
class ProgramPrintable a where
        -- | Should return the ProgramPrintable as (CiviC) source
        programPrint :: a -> String

instance ProgramPrintable UnaryOperator where
        programPrint Negate {} = "-"
        programPrint Not {} = "!"

instance ProgramPrintable Ast where
        programPrint (Ast p) = programPrint p

instance ProgramPrintable Program where
        programPrint (Program d) = intercalate "\n" $ map programPrint $ Data.List.NonEmpty.toList d

instance ProgramPrintable Declaration where
        programPrint (FunctionDeclaration {header = h}) = printf "extern %s" $ programPrint h

        programPrint d@(FunctionDefinition { exported = True }
                        ) = 
            "export " ++ (programPrint (d { exported = False }))

        programPrint (FunctionDefinition 
                        { header = h
                        , exported = False
                        , body = body
                        }
                     ) = 
            printf "%s {\n%s\n}" (programPrint h) (programPrint body)

        programPrint (GlobalDeclaration 
                        { definition = d
                        }
                     ) = 
            printf "extern %s" (programPrint d)

        programPrint (GlobalDefinition {definition = d, exported = False}) = programPrint d
        programPrint (GlobalDefinition {definition = d, exported = True}) = printf "export %s" $ programPrint d

instance ProgramPrintable ArrayInitializer where
        programPrint (ArrayInitializer {initializers = i}) =
            printf "[%s]" $ intercalate ", " $ map programPrint $ Data.List.NonEmpty.toList i
        programPrint (ArrayInitializerExpression {initializerExpression = e}) = programPrint e

instance ProgramPrintable BasicType where
        programPrint Bool {} = "bool"
        programPrint Float {} = "float"
        programPrint Int {} = "int"


instance ProgramPrintable ReturnType where
        programPrint (BasicType {basicReturnType = t}) = programPrint t
        programPrint Void {} = "void"

instance ProgramPrintable FunctionHeader where
        programPrint (FunctionHeader 
                        { returnType = r
                        , funcName = n
                        , parameters = p
                        }
                     ) = 
            printf "%s %s (%s)" (programPrint r) n (intercalate ", " $ map programPrint p)

instance ProgramPrintable Parameter where
        programPrint (Parameter 
                        { argumentType = t
                        , argumentName = n
                        }
                     ) =
            printf "%s %s" (programPrint t) n

        programPrint (ArrayParameter
                        { argumentType = t
                        , argumentName = n
                        , argumentSizeIds = s
                        }
                      ) = 
            printf "%s %s[%s];"
                (programPrint t)
                n
                (intercalate ", " $ Data.List.NonEmpty.toList s)

ppSemicolon :: (ProgramPrintable a) => a -> String
ppSemicolon p = (programPrint p) ++ ";"

instance ProgramPrintable [LocalDeclaration] where
        programPrint l = intercalate "\n" (map programPrint l)


instance ProgramPrintable [Statement] where
        programPrint l = intercalate "\n" $ map programPrint l

instance ProgramPrintable FunctionBody where
        programPrint (FunctionBody 
                        { variableDeclarations = v
                        , functionDeclarations = f
                        , statements = s
                        }
                     ) = 
            printf "%s\n%s\n%s" (programPrint v) (programPrint f) (programPrint s)

instance ProgramPrintable VariableDefinition where
        programPrint (VariableDefinition
                        { typeName = t
                        , variableName = n
                        , initializer = Nothing
                        }
                     ) =
            printf "%s %s;" (programPrint t) n
        programPrint (VariableDefinition
                        { typeName = t
                        , variableName = n
                        , initializer = Just e
                        }
                     ) =
            printf "%s %s = %s;" (programPrint t) n (programPrint e)
        programPrint (ArrayDefinition
                        { typeName = t
                        , variableName = n
                        , dimensionExpressions = d
                        , arrayInitializer = Nothing
                        }
                     ) =
            printf "%s[%s] %s;"
                (programPrint t)
                (intercalate ", " $ map programPrint $ Data.List.NonEmpty.toList d)
                n
        programPrint (ArrayDefinition
                        { typeName = t
                        , variableName = n
                        , dimensionExpressions = d
                        , arrayInitializer = Just a
                        }
                     ) =
            printf "%s[%s] %s = %s;"
                (programPrint t)
                (intercalate ", " $ map programPrint $ Data.List.NonEmpty.toList d)
                n
                (programPrint a)
        programPrint (ArrayDeclaration
                        { typeName = t
                        , variableName = n
                        , dimensionIdentifiers = d
                        }
                     ) =
            printf "%s[%s] %s;"
                (programPrint t)
                (intercalate ", " $ Data.List.NonEmpty.toList d)
                n

instance ProgramPrintable LocalDeclaration where
        programPrint (LocalDeclaration {declaration = d}) = programPrint d
                
instance ProgramPrintable LocalFunctionDeclaration where
        programPrint (LocalFunctionDeclaration 
                        { localHeader = h
                        , localBody = b
                        }
                     ) =
            printf "%s {\n%s\n}\n"
                (programPrint h)
                (programPrint b) 

instance ProgramPrintable [LocalFunctionDeclaration] where
        programPrint l = intercalate "\n" $ map programPrint l

instance ProgramPrintable Statement where
        programPrint (AssignmentStatement
                        { targetName = n
                        , assigningValue = v
                        }
                     ) =
            n ++ " = " ++ (programPrint v) ++ ";"

        programPrint (ElemAssignmentStatement 
                        { elemTargetName = id
                        , elemSizeExprs = indices
                        , elemAssignValue = value
                        }
                     ) =
            printf "%s[%s] = %s;"
                id
                (intercalate "," $ map programPrint $ Data.List.NonEmpty.toList indices)
                (programPrint value)

        programPrint (ArrayInitializationStatement
                        { targetName = id
                        , elementSizeExpressions = idxs
                        }
                     ) =
            printf "%s = new [%s];"
                id
                (intercalate ", " $ map programPrint $ Data.List.NonEmpty.toList idxs)

        programPrint (FunctionCallStatement { call = f}) = 
            programPrint f ++ ";"

        programPrint s@(IfStatement {elseBranch = (Just eb)}) =
            printf "%s} else {\n%s\n}\n"
                (programPrint (s {elseBranch = Nothing}))
                (programPrint eb)

        programPrint (IfStatement 
                        { condition = c
                        , ifBranch = ib
                        , elseBranch = Nothing
                        }
                     ) =
            printf "if (%s) {\n%s\n}\n"
                (programPrint c)
                (programPrint ib)

        programPrint (WhileStatement 
                        { condition = e
                        , loopBody = b
                        }
                     ) =
            printf "while (%s) {\n%s\n}\n"
                (programPrint e)
                (programPrint b)

        programPrint (DoWhileStatement 
                        { condition = e
                        , loopBody = b
                        }
                     ) =
            printf "do {\n%s\n} while (%s);"
                (programPrint b)
                (programPrint e)

        programPrint (ForStatement 
                        { counterName = n
                        , start = s
                        , end = e
                        , step = (Just st)
                        , loopBody = b
                        }
                     ) =
            printf "for (%s = %s, %s, %s) {\n%s\n}\n"
                n 
                (programPrint s)
                (programPrint e)
                (programPrint st) 
                (programPrint b)

        programPrint (ForStatement
                        { counterName = n
                        , start = s
                        , end = e
                        , step = Nothing
                        , loopBody = b
                        }
                     ) =
            printf "for (%s = %s, %s) {\n%s\n}\n"
            n
            (programPrint s)
            (programPrint e)
            (programPrint b) ++ "\n}\n"

        programPrint (ReturnStatement { returnValue = (Just rv) }) =
            "return " ++ (programPrint rv) ++ ";"
        programPrint (ReturnStatement { returnValue = Nothing }) = "return;"


instance ProgramPrintable FunctionCall where
        programPrint (FunctionCall 
                        { functionName = n
                        , arguments = a
                        }
                     ) =
            printf "%s(%s)"
                n
                (intercalate ", " (map programPrint a))
instance ProgramPrintable Expression where
        programPrint (Expression {binaryExpression = b}) = programPrint b

instance ProgramPrintable BinaryExpression where
        programPrint (BinaryExpression 
                        { left = l
                        , right = r
                        , binaryOperator = o
                        }
                     ) =
            printf "%s %s %s"
                (programPrint l)
                (programPrint o)
                (programPrint r)

        programPrint (PassThrough {passExpression = u}) = programPrint u

instance ProgramPrintable BinaryOperator where
        programPrint NotEqual {} = "!="
        programPrint Equal {} = "=="
        programPrint LessThan {} = "<"
        programPrint GreaterThan {} = ">"
        programPrint LessEqual {} = "<="
        programPrint GreaterEqual {} = ">="
        programPrint Add {} = "+"
        programPrint Subtract {} = "-"
        programPrint Multiply {} = "*"
        programPrint Divide {} = "/"
        programPrint Modulus {} = "%"
        programPrint Or {} = "||"
        programPrint And {} = "&&"

instance ProgramPrintable UnaryExpression where
        programPrint (UnaryExpression 
                        { operator = op
                        , unaryValue = e
                        }
                     ) = 
            (programPrint op) ++ (programPrint e)
        programPrint (CastExpression 
                        { castType = t
                        , unaryValue = e
                        }
                     ) =
            printf "(%s) %s" (programPrint t) (programPrint e)
        programPrint (UnaryPassThrough {passBasicExpression = e}) =
            programPrint e

instance ProgramPrintable BasicExpression where
        programPrint (Literal {value = v}) = programPrint v
        programPrint (ParenthesizedExpression {innerExpression = i}) =
            printf "(%s)" (programPrint i)
        programPrint (FunctionCallExpression {callee = f}) =
            programPrint f
        programPrint (IdentifierExpression {identifierName = n}) = n
        programPrint (ArrayExpression 
                        { arrayId = i
                        , arrayElemExpr = e
                        }
                     ) =
            printf "%s[%s]" i $ intercalate ", " $ map programPrint $ Data.List.NonEmpty.toList e

instance ProgramPrintable LiteralValue where
        programPrint (IntegerValue v) = show v
        programPrint (BooleanValue True) = "true"
        programPrint (BooleanValue False) = "false"
        programPrint (FloatValue v) = show v
