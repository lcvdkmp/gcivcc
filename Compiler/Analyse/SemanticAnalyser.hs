{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.Analyse.SemanticAnalyser (
    anotate,
    unAnotate
) where

import Compiler.Ast.Ast
import Data.Either
import Data.Maybe
import Data.Foldable
import Control.Lens hiding (traverse)
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as Ne
import qualified Compiler.Error as Error

-- Anotate the ast with symbol tables.
-- Every symbol table that gets anotated holds all the variables and functions
-- that should be available until that point.
anotate :: Ast -> Error.ErrorMonad Ast
anotate (Ast {program = p})  = fmap Ast $ (anotateProgram . linkFunctions) p

-- Remove all symbol tables from the ast.
-- This is usefull when the tree needs to be re-anotated wild recursion problems
-- where the anotate functions start anotating the things in the symbol table...
unAnotate :: Ast -> Ast
unAnotate ast = Compiler.Ast.Ast.traverse f ast
    where
        f :: Maybe SymbolTable -> Maybe SymbolTable
        f _ = Nothing

-- Anotates an entire program.
anotateProgram :: Program -> Error.ErrorMonad Program
anotateProgram (Program {declarations=d})
            = do
                -- First we collect all the functions.
                (st, decls) <- specialMapM f (SymbolTable Nothing [] []) $ Ne.toList d
                -- Then we collect all globals
                (st', decls') <- specialMapM g st decls
                
                -- Then we slowly go through all the declarations.
                (st'', decls'') <- specialMapM h st' decls'
                return $ Program $ Ne.fromList decls''
        where
            -- Simply collect the names of functions into the symbol table.
            f :: SymbolTable -> Declaration -> Error.ErrorMonad (SymbolTable, Declaration)
            f s func@(FunctionDeclaration {header=h})
                    = do
                        let name = funcName h
                        s' <- addFunctionOrErr s name h
                        return (s', func)
            f s func@(FunctionDefinition {header=h})
                    = do
                        let name = funcName h
                        s' <- addFunctionOrErr s name h
                        return (s', func)
            f s d = Right (s, d)

            -- Collect the names of global variables
            g :: SymbolTable -> Declaration -> Error.ErrorMonad (SymbolTable, Declaration)
            g s (GlobalDeclaration d loc)
                    = do
                        (s', d') <- anotateVariableDefinition s d
                        return (s', GlobalDeclaration d' loc)
            g s (GlobalDefinition {definition=d, exported=e, _declarationLocation=loc})
                    = do
                        (s', d') <- anotateVariableDefinition s d
                        return (s', GlobalDefinition d' e loc)
            
            g s d = Right (s, d)

            -- Recurse into functions.
            h :: SymbolTable -> Declaration -> Error.ErrorMonad (SymbolTable, Declaration)
            h s (FunctionDefinition {header=h, exported=e, body=b, _declarationLocation=loc})
                    = do
                        let s' = SymbolTable (Just s) [] []
                        (h', b') <- anotateFunction s' h b
                        return (s, FunctionDefinition h' e b' loc)
            h s d = Right (s, d)

-- Add the variable to the symbol table and anotate it's initializer.
anotateVariableDefinition :: SymbolTable -> VariableDefinition -> Error.ErrorMonad (SymbolTable, VariableDefinition)
anotateVariableDefinition s d@(VariableDefinition t n Nothing _ _ _)
        = do
            s' <- addVariableOrErr s n d
            return (s', d)
anotateVariableDefinition s d@(VariableDefinition t n (Just i) origin const loc)
        = do
            s' <- addVariableOrErr s n d
            return (s', VariableDefinition t n (Just $ anotateExpression s i) origin const loc) 
anotateVariableDefinition s d@(ArrayDefinition t n de i origin const loc)
        = do
            s' <- addVariableOrErr s n d
            return (s', ArrayDefinition t n
                            (Ne.map (anotateExpression s) de)
                            (fmap (anotateArrayInitializer s) i)
                            origin
                            const
                            loc)
anotateVariableDefinition s d@(ArrayDeclaration t n ids origin loc)
            = do
                s' <- foldlM g s ids
                s'' <- addVariableOrErr s' n d
                return (s'', d)
        where
            g :: SymbolTable -> String -> Error.ErrorMonad SymbolTable
            g s n = addVariableOrErr s n (VariableDefinition (Int defaultLocation) n Nothing origin (Just False) loc)

-- Anotate the various parts of an expression with a symbol table.
anotateExpression :: SymbolTable -> Expression -> Expression
anotateExpression s = Compiler.Ast.Ast.traverse $ f s
    where
        f :: SymbolTable -> BasicExpression -> BasicExpression
        f s (IdentifierExpression id _ loc) = IdentifierExpression id (Just s) loc
        f s (ArrayExpression id elem _ loc) = ArrayExpression id elem (Just s) loc
        f s (FunctionCallExpression callee loc) = FunctionCallExpression (g callee) loc
            where
                g :: FunctionCall -> FunctionCall
                g (FunctionCall n args _ loc) = FunctionCall n args (Just s) loc
        f _ e = e

-- Anotate the expression in an array initializer.
anotateArrayInitializer :: SymbolTable -> ArrayInitializer -> ArrayInitializer
anotateArrayInitializer s a = Compiler.Ast.Ast.traverse (anotateExpression s) a

-- Anotate a function.
-- The symbol table should already consist of a new scope.
anotateFunction :: SymbolTable -> FunctionHeader -> FunctionBody -> Error.ErrorMonad (FunctionHeader, FunctionBody)
anotateFunction s header body
        = do
            -- Add all the parameters to the symbol table.
            s' <- foldlM f s $ parameters header
            -- Add all the functions to the symbol table.
            s'' <- foldlM g s' $ functionDeclarations body
            -- Add and anotate all the variable declarations.
            (s''', varDecs) <- specialMapM h s'' $ variableDeclarations body
            -- Anotate all the local functions.
            funcDecs <- mapM (k s''') $ functionDeclarations body
            -- Anotate all the various statements and expression
            -- with the final symbol table.
            stmnts <- mapM (anotateStatement s''') $ statements body
            -- Anotate return statements with the function header.
            let stmnts' = Compiler.Ast.Ast.traverse m stmnts
            -- Return the new anotated function
            return (header, FunctionBody varDecs funcDecs stmnts' (Just s''') (body ^. location))
    where
        -- Used to fold the parameters into the symbol table.
        f :: SymbolTable -> Parameter -> Error.ErrorMonad SymbolTable
        f s (Parameter t n loc) = addVariableOrErr s n (VariableDefinition t n Nothing Neither (Just False) loc)
        f s (ArrayParameter t n ids loc)
                = do
                    (s', _) <- anotateVariableDefinition s (ArrayDeclaration t n ids Neither loc)
                    return s'

        -- Add a local function to the symbol table.
        g :: SymbolTable -> LocalFunctionDeclaration -> Error.ErrorMonad SymbolTable
        g s (LocalFunctionDeclaration header body loc)
                = do
                    s' <- addFunctionOrErr s (funcName header) header
                    return s'
        
        -- Add a local declaration to the symbol table.
        h :: SymbolTable -> LocalDeclaration -> Error.ErrorMonad (SymbolTable, LocalDeclaration)
        h s (LocalDeclaration decl loc)
                = do
                    (s', decl') <- anotateVariableDefinition s decl
                    return (s', LocalDeclaration decl' loc)

        -- Anotate the local function.
        k :: SymbolTable -> LocalFunctionDeclaration -> Error.ErrorMonad LocalFunctionDeclaration
        k s (LocalFunctionDeclaration header body loc)
                = do
                    (h', b') <- anotateFunction (SymbolTable (Just s) [] []) header body
                    return $ LocalFunctionDeclaration h' b' loc

        -- Anotate a return statement with it function header.
        m :: Statement -> Statement
        m (ReturnStatement e _ loc) = ReturnStatement e (Just header) loc
        m st = st

-- Anotate the various kinds of statements.
anotateStatement :: SymbolTable -> Statement -> Error.ErrorMonad Statement
anotateStatement s (AssignmentStatement t e _ loc)
        = do
            let e' = anotateExpression s e
            return $ AssignmentStatement t e' (Just s) loc
anotateStatement s (ElemAssignmentStatement t idx e _ loc)
        = do
            let idx' = Ne.map (anotateExpression s) idx
            let e' = anotateExpression s e
            return $ ElemAssignmentStatement t idx' e' (Just s) loc
anotateStatement s (ArrayInitializationStatement id dims _ loc)
        = do
            let dims' = Ne.map (anotateExpression s) dims
            return $ ArrayInitializationStatement id dims' (Just s) loc
anotateStatement s (FunctionCallStatement c loc)
        = do
            return $ FunctionCallStatement (f c) loc
    where
        f :: FunctionCall -> FunctionCall
        f (FunctionCall n args _ loc) = FunctionCall n (map (anotateExpression s) args) (Just s) loc
anotateStatement s (IfStatement c ib Nothing loc)
        = do
            let c' = anotateExpression s c
            ib' <- mapM (anotateStatement s) ib
            return $ IfStatement c' ib' Nothing loc
anotateStatement s (IfStatement c ib (Just eb) loc)
        = do
            let c' = anotateExpression s c
            ib' <- mapM (anotateStatement s) ib
            eb' <- mapM (anotateStatement s) eb
            return $ IfStatement c' ib' (Just eb') loc
anotateStatement s (WhileStatement c b loc)
        = do
            let c' = anotateExpression s c
            b' <- mapM (anotateStatement s) b
            return $ WhileStatement c' b' loc
anotateStatement s (DoWhileStatement c b loc)
        = do
            let c' = anotateExpression s c
            b' <- mapM (anotateStatement s) b
            return $ DoWhileStatement c' b' loc
anotateStatement s (ForStatement n i e step b loc)
        = do
            let i' = anotateExpression s i
            let s' = (SymbolTable (Just s) [] [])
            s'' <- addVariableOrErr s' n (VariableDefinition (Int loc) n (Just i') Neither Nothing loc)
            let e' = anotateExpression s'' e
            let step' = f s'' step
            b' <- mapM (anotateStatement s'') b
            return $ (ForStatement n i' e' step' b' loc)
    where
        f :: SymbolTable -> Maybe Expression -> Maybe Expression
        f _ Nothing = Nothing
        f s (Just e) = Just $ anotateExpression s e
anotateStatement s (ReturnStatement (Just e) o loc)
        = do
            let e' = anotateExpression s e
            return $ ReturnStatement (Just e') o loc
anotateStatement _ (ReturnStatement Nothing o loc)
        = do
            return $ ReturnStatement Nothing o loc

-- A sort of combined map/fold.
-- It passes state from left to right while mapping
-- the individual elements and passing state from the previous
-- element to the next.
specialMap :: (b -> a -> (b, c)) -> b -> [a] -> (b, [c])
specialMap _ v [] = (v, [])
specialMap f v (x : xs) = (v'', x' : xs')
    where
        (v', x') = f v x
        (v'', xs') = specialMap f v' xs

-- Monadic version of specialMap.
specialMapM :: (b -> a -> Error.ErrorMonad (b, c)) -> b -> [a] -> Error.ErrorMonad (b, [c])
specialMapM _ v [] = return (v, [])
specialMapM f v (x : xs) = do
                             (v', x') <- f v x
                             (v'', xs') <- specialMapM f v' xs
                             return (v'', x' : xs')

addVariableOrErr :: SymbolTable -> String -> VariableDefinition -> Error.ErrorMonad SymbolTable
addVariableOrErr s n v = ret
    where
        a = addVariableInfo s n v
        ret = case a of Nothing   -> Left $ Error.VariableAlreadyDefined (fromJust $ variableInfoOf s n) v
                        (Just s') -> Right s'

addFunctionOrErr :: SymbolTable -> String -> FunctionHeader -> Error.ErrorMonad SymbolTable
addFunctionOrErr s n h = ret
    where
        a = addFunctionInfo s n h
        ret = case a of Nothing   -> Left $ Error.FunctionAlreadyDefined (fromJust $ functionInfoOf s n) h
                        (Just s') -> Right s'

-- Links the function header of local functions to their parent functions.
-- This is used for name mangling purposes.
linkFunctions :: Program -> Program
linkFunctions (Program decls) = Program $ Ne.map f decls
    where
        f :: Declaration -> Declaration
        f (FunctionDefinition header exported body loc) = FunctionDefinition header exported body' loc
            where
                body' = (FunctionBody (variableDeclarations body)
                                      (map (linkLocals header) $ functionDeclarations body)
                                      (statements body)
                                      (bodySymbolTable body)
                                      (body ^. location))
        f x = x

linkLocals :: FunctionHeader -> LocalFunctionDeclaration -> LocalFunctionDeclaration
linkLocals parent (LocalFunctionDeclaration header body loc) = LocalFunctionDeclaration header' body' loc
    where
        header' = (FunctionHeader (returnType header)
                                  (funcName header)
                                  (parameters header)
                                  (Just parent)
                                  (functionOrigin header)
                                  Nothing
                                  (header ^. location))
        body' = (FunctionBody (variableDeclarations body)
                              (map (linkLocals header') $ functionDeclarations body)
                              (statements body)
                              (bodySymbolTable body)
                              (body ^. location))
