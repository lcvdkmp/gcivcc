module Compiler.Pass.ArrayRewriter (rewriteArrays, rewriteIndexing) where

import Compiler.Ast.Ast as Ast
import Compiler.Analyse.SemanticAnalyser (anotate, unAnotate)

import Data.Either
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty as Ne

rewriteArrays :: Ast.Ast -> Ast.Ast
rewriteArrays ast = (fromRight . anotate . unAnotate . redifineArrays . rewriteIndexing) ast

rewriteIndexing = (Ast.traverse rewriteUsage) . (Ast.traverse rewriteStorage)

rewriteUsage :: Ast.BasicExpression -> Ast.BasicExpression
rewriteUsage (Ast.ArrayExpression id idxs (Just st) loc) = Ast.ArrayExpression id (idxs' :| []) (Just st) loc
    where
        idxs' = foldl collapse
                      (Ne.head idxs)
                      (Prelude.zip (dimensionIds $ variableInfoOf st id)
                                   (Ne.tail idxs))


        dimensionIds :: Maybe VariableDefinition -> [String]
        dimensionIds (Just (ArrayDefinition {})) = Prelude.map (\c -> id ++ "#" ++ show c)
                                                               (Prelude.iterate ((+) 1) 1)
        dimensionIds (Just (ArrayDeclaration {dimensionIdentifiers=ids})) = Ne.tail ids

        collapse :: Ast.Expression -> (String, Ast.Expression) -> Ast.Expression
        collapse expr@(Ast.Expression _ loc) (id, (Ast.Expression expr' _))
                = Ast.Expression (binary (Ast.Add defaultLocation)
                                         (mulExpr (parens expr)
                                                  (id))
                                         (expr'))
                                 loc


        mulExpr :: Ast.BinaryExpression -> String -> Ast.BinaryExpression
        mulExpr l id = binary (Multiply defaultLocation) l $ passThrough id

        binary :: Ast.BinaryOperator -> Ast.BinaryExpression -> Ast.BinaryExpression -> Ast.BinaryExpression
        binary op l r = Ast.BinaryExpression l r op defaultLocation

        passThrough :: String -> Ast.BinaryExpression
        passThrough id = Ast.PassThrough (unary id) defaultLocation

        unary :: String -> Ast.UnaryExpression
        unary id = Ast.UnaryPassThrough (Ast.IdentifierExpression id Nothing defaultLocation) defaultLocation

        parens :: Ast.Expression -> Ast.BinaryExpression
        parens e = Ast.PassThrough (Ast.UnaryPassThrough (Ast.ParenthesizedExpression e defaultLocation)
                                                          defaultLocation)
                                    defaultLocation
rewriteUsage expr = expr

rewriteStorage :: Ast.Statement -> Ast.Statement
rewriteStorage (Ast.ElemAssignmentStatement id idxs value (Just st) loc) = Ast.ElemAssignmentStatement id (idxs' :| []) value (Just st) loc
    where
        idxs' = foldl collapse
                      (Ne.head idxs)
                      (Prelude.zip (dimensionIds $ variableInfoOf st id)
                                   (Ne.tail idxs))


        dimensionIds :: Maybe VariableDefinition -> [String]
        dimensionIds (Just (ArrayDefinition {})) = Prelude.map (\c -> id ++ "#" ++ show c)
                                                               (Prelude.iterate ((+) 1) 1)
        dimensionIds (Just (ArrayDeclaration {dimensionIdentifiers=ids})) = Ne.tail ids

        collapse :: Ast.Expression -> (String, Ast.Expression) -> Ast.Expression
        collapse expr@(Ast.Expression _ loc) (id, (Ast.Expression expr' _))
                = Ast.Expression (binary (Ast.Add defaultLocation)
                                         (mulExpr (parens expr)
                                                  (id))
                                         (expr'))
                                 loc


        mulExpr :: Ast.BinaryExpression -> String -> Ast.BinaryExpression
        mulExpr l id = binary (Multiply defaultLocation) l $ passThrough id

        binary :: Ast.BinaryOperator -> Ast.BinaryExpression -> Ast.BinaryExpression -> Ast.BinaryExpression
        binary op l r = Ast.BinaryExpression l r op defaultLocation

        passThrough :: String -> Ast.BinaryExpression
        passThrough id = Ast.PassThrough (unary id) defaultLocation

        unary :: String -> Ast.UnaryExpression
        unary id = Ast.UnaryPassThrough (Ast.IdentifierExpression id (Just st) defaultLocation) defaultLocation

        parens :: Ast.Expression -> Ast.BinaryExpression
        parens e = Ast.PassThrough (Ast.UnaryPassThrough (Ast.ParenthesizedExpression e defaultLocation)
                                                          defaultLocation)
                                    defaultLocation
rewriteStorage stmnt = stmnt

redifineArrays :: Ast.Ast -> Ast.Ast
redifineArrays ast = Ast.traverse rewriteLocalDefinitions $ (Ast.Ast . Ast.Program . Ne.fromList) decls'
    where
        decls' = concatMap rewriteGlobalDeclarations $ (Ne.toList . Ast.declarations . Ast.program) ast

rewriteGlobalDeclarations :: Declaration -> [Declaration]
rewriteGlobalDeclarations (Ast.GlobalDefinition (Ast.ArrayDefinition t id dims init origin const loc) e loc')
        =  (Prelude.map (uncurry declareDim) $ Prelude.zip (Ne.toList dims) (Prelude.iterate ((+) 1) 0))
        ++ [Ast.GlobalDefinition (Ast.ArrayDefinition t id dims' init origin const loc) e loc']
    where
        declareDim :: Ast.Expression -> Int -> Ast.Declaration
        declareDim expr dim = Ast.GlobalDefinition (Ast.VariableDefinition (Ast.Int defaultLocation)
                                                                           (id ++ "#" ++ show dim)
                                                                           (Just expr)
                                                                           origin
                                                                           Nothing
                                                                           loc)
                                                   e
                                                   loc'

        dims' = Ne.fromList $ Prelude.take (Ne.length dims) $ Prelude.map idExpr $ Prelude.iterate ((+) 1) 0

        idExpr :: Int -> Ast.Expression
        idExpr dim = Ast.Expression (Ast.PassThrough (unary dim) loc) loc

        unary :: Int -> Ast.UnaryExpression
        unary dim = Ast.UnaryPassThrough (Ast.IdentifierExpression (id ++ "#" ++ show dim) Nothing loc) loc
rewriteGlobalDeclarations decl = [decl]

rewriteLocalDefinitions :: Ast.FunctionBody -> Ast.FunctionBody
rewriteLocalDefinitions (Ast.FunctionBody vars funcs stmnts table loc) = Ast.FunctionBody vars' funcs stmnts table loc
    where
        vars' = concatMap expand vars
        
        expand :: LocalDeclaration -> [LocalDeclaration]
        expand (Ast.LocalDeclaration (Ast.ArrayDefinition t id dims init origin const loc) loc')
                =  (Prelude.map (uncurry declareDim) $ Prelude.zip (Ne.toList dims) (Prelude.iterate ((+) 1) 0))
                ++ [Ast.LocalDeclaration (Ast.ArrayDefinition t id dims' init origin const loc) loc']
            where
                declareDim :: Ast.Expression -> Int -> Ast.LocalDeclaration
                declareDim expr dim = Ast.LocalDeclaration (Ast.VariableDefinition (Ast.Int defaultLocation)
                                                                                   (id ++ "#" ++ show dim)
                                                                                   (Just expr)
                                                                                   origin
                                                                                   Nothing
                                                                                   loc)
                                                           loc'
                dims' = Ne.fromList $ Prelude.take (Ne.length dims) $ Prelude.map idExpr $ Prelude.iterate ((+) 1) 0

                idExpr :: Int -> Ast.Expression
                idExpr dim = Ast.Expression (Ast.PassThrough (unary dim) loc) loc

                unary :: Int -> Ast.UnaryExpression
                unary dim = Ast.UnaryPassThrough (Ast.IdentifierExpression (id ++ "#" ++ show dim) Nothing loc) loc
        expand d = [d]

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "We messed up array rewriting..."