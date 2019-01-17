module Compiler.Pass.LoopRewriter (rewriteLoops) where

import Compiler.Ast.Ast
import Compiler.Analyse.SemanticAnalyser

import Data.Maybe
import Data.Either
import Prelude hiding (traverse)
import Control.Lens hiding (traverse)

-- Rewrite all the different looping constructs into a do-while loop.
rewriteLoops :: Ast -> Ast
rewriteLoops ast = (fromRight . anotate) $ traverse funcRewrite $ traverse whileToDoWhile ast

-- Rewrite while to do while loops.
-- This done by simply wrapping the body of a while into a do-while
-- and place that inside an if-statement to guard the first execution.
whileToDoWhile :: Statement -> Statement
whileToDoWhile (WhileStatement c b loc) = IfStatement c [(DoWhileStatement c b loc)] Nothing loc
whileToDoWhile s = s

-- Rewriting for-loops is a bit more tricky.
-- First of all the implicitly introduced start, end, and increment variables
-- are hoisted to the top of the function.
-- Secondly to flatten the scopes of the for-loops we need to introduced unique
-- variables that have never been seen before in the function.
-- And to use these variables the old names must be rewritten.
funcRewrite :: FunctionBody -> FunctionBody
funcRewrite (FunctionBody vars funcs stmnts (Just table) loc) = ret
        where
            (tab', vars', stmnts') = specialConcatMap statementMap table stmnts
            vars'' = map (flip LocalDeclaration loc) vars'
            ret = FunctionBody (vars ++ vars'') funcs stmnts' (Just tab') loc
funcRewrite a = a

-- Map the various statements that could contain for-loops into statements
-- that no longer contain them.
statementMap :: SymbolTable -> Statement -> (SymbolTable, [VariableDefinition], [Statement])
-- If-statements might have for-loops in their various branches.
statementMap tab (IfStatement c ib eb loc) = ret
        where
            (tab', vars, ib') = specialConcatMap statementMap tab ib
            (tab'', vars', eb') = f eb tab'
            ret = (tab'', vars ++ vars', [IfStatement c ib' eb' loc])

            f :: Maybe [Statement] -> SymbolTable -> (SymbolTable, [VariableDefinition], Maybe [Statement])
            f Nothing t = (t, [], Nothing)
            f (Just st) t = ret
                    where
                        (t', vars, st') = specialConcatMap statementMap t st
                        ret = (t', vars, Just st')
-- The same goes for do-while loops.
statementMap tab (DoWhileStatement c b loc) = ret
        where
            (tab', vars, b') = specialConcatMap statementMap tab b
            ret = (tab', vars, [DoWhileStatement c b' loc])
-- This one we want to map.
statementMap tab (ForStatement cn s e i b loc)
            = (tab'''',
                [vdcn'] ++ vs ++ [vdne, vdni],
                [ AssignmentStatement cn' s (Just tab'''') (s^.location)
                , AssignmentStatement ne e (Just tab'''') (e^.location)
                , AssignmentStatement ni (h i) (Just tab'''') defaultLocation
                , whileToDoWhile loop
                ])
        where
            -- Generate a new variable name for the counter.
            cn' = genVarName tab
            -- It's definition to be added to the top of the function.
            vdcn' = VariableDefinition (Int loc) cn' Nothing Neither Nothing loc
            -- Add the newly generated variable to the symbol table.
            tab' = justAddVariable tab cn' vdcn'

            -- Use the new symbol table to remap the for-loops that might be inside this one.
            (tab'', vs, b') = specialConcatMap statementMap tab' b

            -- Rename all old uses of the loop-counter to the newly generated name.
            b'' = traverse (f cn cn') $ traverse (g cn cn') b'

            -- Generate a variable name for the end value of the loop.
            ne = genVarName tab''
            -- Make it's variable definition.
            vdne = VariableDefinition (Int $ e^.location) ne Nothing Neither Nothing (e^.location)
            -- Add the end variable's definition to the symbol table.
            tab''' = justAddVariable tab'' ne vdne
            -- Gen a name for the variable that should the value of the increment expression.
            ni = genVarName tab'''
            -- Generate it's variable definition.
            vdni = VariableDefinition (Int $ s^.location) ni Nothing Neither Nothing (e^.location)
            -- Also add that one to the symbol table.
            tab'''' = justAddVariable tab''' ni vdni
            -- Side note: We are ending up with a lot of versions of the symbol table.

            -- Add the incrementing of the counter to the end of the variable definition.
            wb = b'' ++ [AssignmentStatement cn' incExpr (Just tab'''') defaultLocation]

            -- The actual expression which increments the counter.
            incExpr = Expression (BinaryExpression (binId cn' tab'''') (binId ni tab'''') (Add d) d) d

            -- The condition needed for forward iteration.
            fc = BinaryExpression (binId cn' tab'''') (binId ne tab'''') (LessThan d) d 
            -- The condition needed for reverse iteration.
            bc = BinaryExpression (binId cn' tab'''') (binId ne tab'''') (GreaterThan d) d
            -- Determines the direction of the iteration
            direction = BinaryExpression (binId ni tab'''') (PassThrough (UnaryPassThrough (Literal (IntegerValue 0) d) d) d) (GreaterThan d) d

            incrementing = BinaryExpression direction fc (And d) d

            -- The end condition used in the loop looks as follows:
            -- (direction == forward && i < e) || i > e
            condition = BinaryExpression incrementing bc (Or d) d

            -- The resulting loop construct.
            loop = WhileStatement (Expression condition d) wb d

            -- A shorted name for annoying defaultLocation thing needed in the ast.
            d = defaultLocation

            -- Replaced assignment statement uses (technically illegal but we allow it (I think)).
            f :: String -> String -> Statement -> Statement
            f orig new (AssignmentStatement name e tab loc)
                    | name == orig = AssignmentStatement new e tab loc
                    | otherwise    = AssignmentStatement name e tab loc
            f _ _ s = s

            -- Replace identifier expression uses.
            g :: String -> String -> BasicExpression -> BasicExpression
            g orig new (IdentifierExpression name tab loc)
                    | name  == orig = IdentifierExpression new tab loc
                    | otherwise     = IdentifierExpression name tab loc
            g _ _ e = e

            -- Makes an increment expression if the loop has non.
            h :: Maybe Expression -> Expression
            h Nothing = Expression (PassThrough (UnaryPassThrough (Literal (IntegerValue 1) d) d) d) d
                    where
                        d = defaultLocation
            h (Just e) = e

            -- Generates a binary expression housing an identifier expression from the name of an identifier.
            binId :: String -> SymbolTable -> BinaryExpression
            binId name s = PassThrough (UnaryPassThrough (IdentifierExpression name (Just s) d) d) d
                    where
                        d = defaultLocation
statementMap tab s = (tab, [], [s])

-- Perform the same kind of mapping as in Compiler.Analyse.SemanticAnalyser
-- But we also collect all the variable definition we end up with.
specialConcatMap :: (SymbolTable -> Statement -> (SymbolTable, [VariableDefinition], [Statement])) -> SymbolTable -> [Statement] -> (SymbolTable, [VariableDefinition], [Statement])
specialConcatMap _ tab [] = (tab, [], [])
specialConcatMap f tab (s : rem) = ret
        where
            (tab', v', s') = f tab s
            (tab'', vs', rem') = specialConcatMap f tab' rem
            ret = (tab'', v' ++ vs', s' ++ rem')

-- Force the adding of a variable.
-- It should never fail otherwise the generation of unique variables isn't so unique.
justAddVariable :: SymbolTable -> String -> VariableDefinition -> SymbolTable
justAddVariable tab n v = fromJust $ addVariableInfo tab n v

-- Force an either into it's right element.
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "Still can't make a Right from a Left."
