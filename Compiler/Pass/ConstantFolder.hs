{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Compiler.Pass.ConstantFolder (
    foldConstants
) where

import qualified Compiler.Ast.Ast as Ast
import qualified Data.List.NonEmpty as Ne
import Data.Maybe

foldConstants :: Ast.Ast -> Ast.Ast
foldConstants = optimize . anotate


-- | Anotates the Ast with information regarding to whether or not
-- a variable is a constant and known at compile-time and
-- whether or not a function is a pure function.
-- A function is a pure function if it satisfies the following 3 criteria.
--   1. It only calls other pure functions.
--   2. It does not write to outside of it's own scope.
--   3. IT only reads variables outside of it's own scope that are considered constant.
--
-- Now there is a problem with this definition when it comes to local functions.
-- A function that has local functions is considered pure if it's local
-- do not violate the 3 rules when they are considered in the context of the enclosing function.
--
-- Per example this function is considered pure even though it's local function reach outside of it's scope.
-- ```
-- void f () {
--     int a = 2;
--     void g () { a = 4;}
--     g();
--     return a;
-- }
-- ```
-- Dispite the fact that the function 'g' writes to a variable outside of it's scope the function 'f' is still pure.
-- 
-- A variable is considered constant if it is only written to in it's initializer.
anotate :: Ast.Ast -> Ast.Ast
anotate = pureAnotate . constAnotate

-- | Figures out which variables are considered constants and which are not.
constAnotate :: Ast.Ast -> Ast.Ast
constAnotate ast = (Ast.Ast . Ast.Program . Ne.fromList) $ (map isConst $ zip declarations (repeat declarations)) ++ init
    where
        declarations = ((filter (not . isInitFunction)) . Ne.toList . Ast.declarations . Ast.program) ast
        init = ((filter isInitFunction) . Ne.toList . Ast.declarations . Ast.program) ast

        isConst :: (Ast.Declaration, [Ast.Declaration]) -> Ast.Declaration

        -- Exported variables are automatically non-const we don't know who might write to them.
        isConst (decl@Ast.GlobalDefinition { Ast.exported = True}, _)
                = decl { Ast.definition = def' }
            where
                def' = (Ast.definition decl) { Ast.isConst = Just False }

        -- If the variable is not exported it might be const if it has an initializer
        -- no function except the __init function writes to it.
        isConst (decl@Ast.GlobalDefinition {Ast.exported = False}, decls)
                = decl { Ast.definition = def' }
            where
                def = Ast.definition decl
                def' = def { Ast.isConst = Just isConst'}

                isConst' = figureOut def 

                figureOut :: Ast.VariableDefinition -> Bool
                figureOut (Ast.VariableDefinition {Ast.initializer = Just expr})
                        = isWrittenTo && isKnown expr
                figureOut (Ast.ArrayDefinition { Ast.arrayInitializer = Just expr})
                        = isWrittenTo && isKnown expr
                figureOut _ = False

                isWrittenTo = not $ writesTo def decls

        -- Now to anotate the local variables inside of functions.
        isConst (decl@Ast.FunctionDefinition {}, _) = decl { Ast.body = constAnotateBody $ Ast.body decl }

        -- And everything else is not a variable nor has one that needs anotation.
        isConst (decl, _) = decl
        
        -- Checks if the given declaration is the init function
        isInitFunction :: Ast.Declaration -> Bool
        isInitFunction (Ast.FunctionDefinition {Ast.header = (Ast.FunctionHeader { Ast.funcName = "__init"})}) = True
        isInitFunction _ = False

        -- Anotates the local variables of functions.
        constAnotateBody :: Ast.FunctionBody -> Ast.FunctionBody
        constAnotateBody body = body { Ast.variableDeclarations = vars'
                                     , Ast.functionDeclarations = funs'
                                     }
            where
                vars = Ast.variableDeclarations body
                funs = Ast.functionDeclarations body
                funs' = map figureOut funs
                vars' = map isConst' $ zip3 vars (repeat funs) (repeat $ Ast.statements body)

                figureOut :: Ast.LocalFunctionDeclaration -> Ast.LocalFunctionDeclaration
                figureOut decl = decl {Ast.localBody = body' }
                    where
                        body = Ast.localBody decl
                        body' = constAnotateBody body

        -- Anotates local variables with their constness.
        isConst' (decl@Ast.LocalDeclaration {}, funs, statements)
                = decl { Ast.declaration = var'}
            where
                var = Ast.declaration decl
                var' = var { Ast.isConst = Just actuallyConst }
                actuallyConst = not (writesTo var funs || writesTo var statements)
                
pureAnotate :: Ast.Ast -> Ast.Ast
pureAnotate = id

optimize :: Ast.Ast -> Ast.Ast
optimize = id

-- | Typeclass used to check if any part of the ast writes to a given variable.
class ConstCheckable a where
    writesTo :: Ast.VariableDefinition -> a -> Bool

instance  (ConstCheckable a, Foldable t) => ConstCheckable (t a) where
    writesTo var terms = any (writesTo var) terms

instance ConstCheckable Ast.Declaration where
    writesTo v (Ast.FunctionDefinition {Ast.body = b}) = writesTo v b
    writesTo _ _ = False

instance ConstCheckable Ast.FunctionBody where
    writesTo var body = writesTo var statements || writesTo var localFunctions
        where
            statements = Ast.statements body
            localFunctions = Ast.functionDeclarations body

instance ConstCheckable Ast.LocalFunctionDeclaration where
    writesTo var func = writesTo var (Ast.localBody func)

instance ConstCheckable Ast.Statement where
    writesTo var stmnt@(Ast.AssignmentStatement {}) = var == var'
        where
            var' = fromJust $ Ast.variableInfoOf st id
            st = (fromJust . Ast.statementTable) stmnt
            id = Ast.targetName stmnt
    writesTo var stmnt@(Ast.ElemAssignmentStatement {}) = var == var'
        where
            var' = fromJust $ Ast.variableInfoOf st id
            st = (fromJust . Ast.statementTable) stmnt
            id = Ast.elemTargetName stmnt
    writesTo var stmnt@(Ast.IfStatement {}) = writesTo var ifBranch || writesTo var elseBranch
        where
            ifBranch = Ast.ifBranch stmnt
            elseBranch = Ast.elseBranch stmnt
    writesTo var stmnt@(Ast.WhileStatement {Ast.loopBody = body}) = writesTo var body
    writesTo var stmnt@(Ast.DoWhileStatement {Ast.loopBody = body}) = writesTo var body
    writesTo var stmnt@(Ast.ForStatement {Ast.loopBody = body}) = writesTo var body
    writesTo _ _ = False

-- | Indicates a node in the Ast of which we know it's value at compile-time.
class IsKnown a where
    isKnown :: a -> Bool

instance (IsKnown a, Foldable t) => IsKnown (t a) where
    isKnown xs = all isKnown xs

instance IsKnown Ast.ArrayInitializer where
    isKnown (Ast.ArrayInitializer {Ast.initializers = inits}) = isKnown inits
    isKnown (Ast.ArrayInitializerExpression {Ast.initializerExpression = expr}) = isKnown expr

instance IsKnown Ast.Expression where
    isKnown = isKnown . Ast.binaryExpression

instance IsKnown Ast.BinaryExpression where
    isKnown (Ast.PassThrough {Ast.passExpression = expr }) = isKnown expr
    
    -- TODO: Fix this one as we might know some results of binary expressions
    -- even a part of the expression is not known.
    -- For instance the expression `0 * a` is still known no mather if 'a' is.
    -- However care must be taken when function calls are involved.
    isKnown expr = isKnown (Ast.left expr) && isKnown (Ast.right expr)

instance IsKnown Ast.UnaryExpression where
    isKnown (Ast.UnaryPassThrough {Ast.passBasicExpression = expr}) = isKnown expr
    isKnown expr = (isKnown . Ast.unaryValue) expr

instance IsKnown Ast.BasicExpression where
    isKnown (Ast.Literal {}) = True
    isKnown (Ast.ParenthesizedExpression {Ast.innerExpression = expr}) = isKnown expr

    -- TODO: Fix this as pure functions with known arguments are also known.
    isKnown (Ast.FunctionCallExpression {}) = False

    isKnown idExpr@(Ast.IdentifierExpression {}) = (isConst . Ast.isConst) var
        where
            identifier = Ast.identifierName idExpr
            table = fromJust $ Ast.identifierSymbolTable idExpr
            var = fromJust $ Ast.variableInfoOf table identifier
            isConst = any id

    isKnown arrExpr@(Ast.ArrayExpression {}) = (isConst . Ast.isConst) var
        where
            identifier = Ast.arrayId arrExpr
            table = fromJust $ Ast.arraySymbolTable arrExpr
            var = fromJust $ Ast.variableInfoOf table identifier
            isConst = any id
