module Compiler.Pass.Initialization (
    rewriteInitializers
) where

import Prelude hiding (traverse, reverse)
import qualified Prelude as Prelude
import Compiler.Ast.Ast
import Data.List.NonEmpty ((<|), reverse, toList, NonEmpty)
import qualified Data.List.NonEmpty as Ne
import Data.Maybe
import Compiler.Analyse.SemanticAnalyser
import Compiler.Pass.ArrayRewriter

rewriteInitializers :: Ast -> Ast
rewriteInitializers = fromRight . anotate . initializeGlobals . unAnotate

initializeGlobals :: Ast -> Ast
initializeGlobals ast = traverse (injectInit $ (collectInitializedGlobals . toList . declarations . program) ast) ast

collectInitializedGlobals :: [Declaration] -> [VariableDefinition]
collectInitializedGlobals d = map mig $ filter g d
    where
        mig :: Declaration -> VariableDefinition
        mig g = definition g

        g :: Declaration -> Bool
        g (GlobalDefinition { definition = (VariableDefinition { initializer = (Just _)})}) = True
        g (GlobalDefinition { definition = (ArrayDefinition {})}) = True
        g _ = False


toStatement :: VariableDefinition -> [Statement]
-- TODO: figure out the symbol table or steal it or something
-- For now we just cheat and re-anotate the entire ast...
-- Okay so this cheat doesn't work because we screw up the symbol table of the initializer.
-- Massively useless and cheating fix. Don't rewrite local initaliazers and have special code generation for them D:
toStatement (VariableDefinition _ id (Just ex) _ _ loc)
        = [AssignmentStatement id ex Nothing loc]
toStatement (ArrayDefinition _ id dims init _ _ loc)
        = [ArrayInitializationStatement id dims Nothing loc] ++ redoInit id (Ne.length dims) init

injectInit :: [VariableDefinition] -> Program -> Program
injectInit vd p@(Program { declarations = d}) = p {declarations = reverse $ a <| (reverse d)}
    where
        a :: Declaration
        a = FunctionDefinition h True (b vd) defaultLocation
            where
                h :: FunctionHeader
                h = FunctionHeader (Void defaultLocation) "__init" [] Nothing Exported (Just False) defaultLocation

                b :: [VariableDefinition] -> FunctionBody
                b vd = FunctionBody [] [] s Nothing defaultLocation
                    where
                        s = concatMap toStatement vd ++ [ReturnStatement Nothing (Just h) defaultLocation]

removeInit :: Declaration -> Declaration
removeInit d@(GlobalDefinition vd@(VariableDefinition {}) _ _) = d {definition = vd {initializer = Nothing}}
removeInit d@(GlobalDefinition vd@(ArrayDefinition {}) _ _) = d {definition = vd {arrayInitializer = Nothing}}
removeInit d = d

initializeLocals :: Ast -> Ast
initializeLocals ast = traverse localRewrite ast

localRewrite :: FunctionBody -> FunctionBody
localRewrite (FunctionBody vars funcs stmnts table loc) = FunctionBody vars' funcs stmnts' table loc
    where
        vars' = map removeInitializers vars
        stmnts' = concatMap toStatement vars ++ stmnts

        removeInitializers :: LocalDeclaration -> LocalDeclaration
        removeInitializers (LocalDeclaration vd@(VariableDefinition {}) loc)
                = LocalDeclaration (vd {initializer = Nothing}) loc
        removeInitializers (LocalDeclaration vd@(ArrayDefinition {}) loc)
                = LocalDeclaration (vd {arrayInitializer = Nothing}) loc
        removeInitializers d = d

        toStatement :: LocalDeclaration -> [Statement]
        toStatement (LocalDeclaration (VariableDefinition _ id (Just init) _ _ loc) _)
                = [AssignmentStatement id init Nothing loc]
        toStatement (LocalDeclaration (ArrayDefinition _ id dims init _ _ loc) _)
                = [ArrayInitializationStatement id dims Nothing loc] ++ redoInit id (Ne.length dims) init
        toStatement d = []


redoInit :: String -> Int -> Maybe ArrayInitializer -> [Statement]
redoInit _ _ Nothing = []
redoInit id dims (Just (ArrayInitializer inits _)) = concatMap (uncurry $ redoArrayInitializer id) elements
    where
        idxs = Prelude.map (\i -> [constExpr i]) $ Prelude.iterate ((+) 1) 0
        elements = Prelude.zip idxs $ Ne.toList inits
redoInit id dims (Just (ArrayInitializerExpression expr _)) = redoScalarInitializer id dims expr

redoScalarInitializer :: String -> Int -> Expression -> [Statement]
redoScalarInitializer id dims value = [first, remainder]
    where
        first = ElemAssignmentStatement id (Ne.fromList [zeroExpr]) value Nothing defaultLocation

        origin = Expression (PassThrough (UnaryPassThrough (ArrayExpression id (Ne.fromList [zeroExpr]) Nothing defaultLocation) defaultLocation) defaultLocation) defaultLocation

        assign = ElemAssignmentStatement id (Ne.fromList [Expression (idExpr (id ++ "#i")) defaultLocation]) origin Nothing defaultLocation

        remainder = ForStatement (id ++ "#i") zeroExpr endExpr Nothing [assign] defaultLocation

        endExpr = Expression (foldl1 mult ids) defaultLocation
            where
                mult x y = BinaryExpression x y (Multiply defaultLocation) defaultLocation

        ids = Prelude.take dims $ Prelude.map idExpr $ Prelude.map (\i -> id ++ "#" ++ show i) $ Prelude.iterate ((+) 1) 0

        idExpr id = PassThrough (UnaryPassThrough (IdentifierExpression id Nothing defaultLocation) defaultLocation) defaultLocation

redoArrayInitializer :: String -> [Expression] -> ArrayInitializer -> [Statement]
redoArrayInitializer id idxs (ArrayInitializerExpression expr loc)
        = [ElemAssignmentStatement id (flatten $ Ne.fromList idxs) expr Nothing loc]
    where
        flatten :: NonEmpty Expression -> NonEmpty Expression
        flatten idxs = idxs' Ne.:| []
            where
                idxs' = foldl collapse
                              (Ne.head idxs)
                              (Prelude.zip dimensionIds (Ne.tail idxs))
            
                dimensionIds = Prelude.map (\c -> id ++ "#" ++ show c) (Prelude.iterate ((+) 1) 1)

                collapse :: Expression -> (String, Expression) -> Expression
                collapse expr@(Expression _ loc) (id, Expression expr' _)
                        = Expression (binary (Add loc)
                                             (mulExpr (parens expr)
                                                      (id))
                                             (expr'))
                                     loc

                mulExpr :: BinaryExpression -> String -> BinaryExpression
                mulExpr l id = binary (Multiply loc) l $ passThrough id

                binary :: BinaryOperator -> BinaryExpression -> BinaryExpression -> BinaryExpression
                binary op l r = BinaryExpression l r op loc

                passThrough :: String -> BinaryExpression
                passThrough id = PassThrough (unary id) loc

                unary :: String -> UnaryExpression
                unary id = UnaryPassThrough (IdentifierExpression id Nothing loc) loc

                parens :: Expression -> BinaryExpression
                parens e = PassThrough (UnaryPassThrough (ParenthesizedExpression e loc)
                                                         loc)
                                       loc

                
redoArrayInitializer id idxs (ArrayInitializer inits _) = concatMap (uncurry $ redoArrayInitializer id) elements
    where
        idxs' = Prelude.map (\i -> idxs ++ [constExpr i]) $ Prelude.iterate ((+) 1) 0
        elements = Prelude.zip idxs' $ Ne.toList inits

zeroExpr :: Expression
zeroExpr = Expression (PassThrough unary defaultLocation) defaultLocation
    where
        unary = UnaryPassThrough (Literal (IntegerValue 0) defaultLocation) defaultLocation

constExpr :: Integer -> Expression
constExpr value = Expression (PassThrough unary defaultLocation) defaultLocation
    where
        unary = UnaryPassThrough (Literal (IntegerValue value) defaultLocation) defaultLocation

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "This really shouldn't happen. Incorrectly forcing Left to Right."
