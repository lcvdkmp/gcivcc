{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Compiler.CodeGen (generateCode, length) where

import qualified Compiler.Ast.Ast as Ast
import qualified Compiler.Type.HasType as HasType
import qualified Compiler.Type.Type as Type
import qualified Compiler.Position as Position

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Either
import Data.Word
import Data.Int
import Data.Foldable (toList, Foldable)
import Data.List hiding (length)
import qualified Data.List.NonEmpty as Ne
import Prelude hiding (length)
import qualified Prelude as Prelude
import Data.Function

-- Code generation is fairly simple.
-- First we extract all the constants from the program.
-- After that we simply walk the ast and generate code for each node in it.
-- After that we declare all globals.
-- Then we export them.
-- Following by the importing of variables.
-- Then the exporting of functions.
-- Followed by the importing of functions.
-- When all this is done we have a slight problem.
-- Since both the imported functions and variables start at index 0.
-- To fix this we simply bump the counter off all the imported functions
-- by the amount of imported variables we have.
-- When we then a list of all instructions that represent our program.
-- we perform some simple peep-hole optimizations.
generateCode :: Ast.Ast -> [Instruction]
generateCode ast =  optimize
                 $  instructions
                 ++ declareConstants constantTable
                 ++ (declareGlobals . Ast.declarations . Ast.program) ast
                 ++ (exportVariables . Ast.declarations . Ast.program) ast
                 ++ (importVariables . Ast.declarations . Ast.program) ast
                 ++ (exportFunctions . Ast.declarations . Ast.program) ast
                 ++ (importFunctions . Ast.declarations . Ast.program) ast
                 &  map (fixImports
                        $ fromIntegral
                        $ Prelude.length
                        $ (importVariables . Ast.declarations . Ast.program) ast)
    where
        constants = Set.toList $ extractConstants ast
        constantTable = Map.fromList $ zip constants $ iterate ((+) 1) 0

        lg = LabelGenerator 0

        (_, instructions) = encode constantTable lg ast

        declareConstants :: ConstantTable -> [Instruction]
        declareConstants c = map (ConstantDeclaration. fst) constants
            where
                constants = sortOn snd $ Map.toList c

        fixImports :: Word16 -> Instruction -> Instruction
        fixImports offset (Jsre index) = Jsre (offset + index)
        fixImports _ i = i

data Offset = RelativeOffset Int16
            | LabelOffset String
            deriving(Eq)

instance Show Offset where
    show (RelativeOffset offset) = show offset
    show (LabelOffset name) = name

data Instruction = IAdd
                 | FAdd
                 | ISub
                 | FSub
                 | IMul
                 | FMul
                 | IDiv
                 | FDiv
                 | IRem
                 | INeg
                 | FNeg
                 | BNot
                 | IInc
                    { location :: Word8
                    , constant :: Word16
                    }
                 | IInc1 { location :: Word8 }
                 | IDec
                    { location :: Word8
                    , constant :: Word16
                    }
                 | IDec1 { location :: Word8 }
                 | BAdd
                 | BMul
                 | INe
                 | IEq
                 | ILt
                 | ILe
                 | IGt
                 | IGe
                 | FNe
                 | FEq
                 | FLt
                 | FLe
                 | FGt
                 | FGe
                 | BNe
                 | BEq
                 | Isr
                 | Isrn { nestingDepth :: Word8 }
                 | Isrl
                 | Isrg
                 | Jsr
                    { argumentCount :: Word8
                    , offset :: Offset
                    }
                 | Jsre { importIndex :: Word16 }
                 | Esr { localVariableCount :: Word8 }
                 | IReturn
                 | FReturn
                 | BReturn
                 | Return
                 | Jump { offset :: Offset }
                 | BranchTrue { offset :: Offset }
                 | BranchFalse { offset :: Offset }
                 | ILoad { location :: Word8 }
                 | FLoad { location :: Word8 }
                 | BLoad { location :: Word8 }
                 | ALoad { location :: Word8 }
                 | ILoadN
                    { nestingDepth :: Word8
                    , location :: Word8
                    }
                 | FLoadN
                    { nestingDepth :: Word8
                    , location :: Word8
                    }
                 | BLoadN
                    { nestingDepth :: Word8
                    , location :: Word8
                    }
                 | ALoadN
                    { nestingDepth :: Word8
                    , location :: Word8
                    }
                 | ILoadG { globalIndex :: Word16 }
                 | FLoadG { globalIndex :: Word16 }
                 | BLoadG { globalIndex :: Word16 }
                 | ALoadG { globalIndex :: Word16 }
                 | ILoadE { importIndex :: Word16 }
                 | FLoadE { importIndex :: Word16 }
                 | BLoadE { importIndex :: Word16 }
                 | ALoadE { importIndex :: Word16 }
                 | ILoadC { constantIndex :: Word16 }
                 | FLoadC { constantIndex :: Word16 }
                 | BLoadC { constantIndex :: Word16 }
                 | ILoadZero
                 | FLoadZero
                 | BLoadTrue
                 | ILoadOne
                 | FLoadOne
                 | BLoadFalse
                 | ILoadMinusOne
                 | IStore { location :: Word8 }
                 | FStore { location :: Word8 }
                 | BStore { location :: Word8 }
                 | AStore { location :: Word8 }
                 | IStoreN { nestingDepth :: Word8, location :: Word8 }
                 | FStoreN { nestingDepth :: Word8, location :: Word8 }
                 | BStoreN { nestingDepth :: Word8, location :: Word8 }
                 | IStoreG { globalIndex :: Word16 }
                 | FStoreG { globalIndex :: Word16 }
                 | BStoreG { globalIndex :: Word16 }
                 | AStoreG { globalIndex :: Word16 }
                 | IStoreE { exportIndex :: Word16 }
                 | FStoreE { exportIndex :: Word16 }
                 | BStoreE { exportIndex :: Word16 }
                 | AStoreE { exportIndex :: Word16 }
                 | INewA
                 | FNewA
                 | BNewA
                 | ILoadA
                 | FLoadA
                 | BLoadA
                 | IStoreA
                 | FStoreA
                 | BStoreA
                 | I2F
                 | F2I
                 | IPop
                 | FPop
                 | BPop
                 | Label { name :: String }
                 | ConstantDeclaration { value :: Ast.LiteralValue }
                 | GlobalDeclaration { variableType :: Type.TypeElem }
                 | VariableExport { name :: String, globalIndex :: Word16 }
                 | VariableImport { name :: String, variableType :: Type.TypeElem }
                 | FunctionExport { function :: Ast.FunctionHeader }
                 | FunctionImport { function :: Ast.FunctionHeader }
                 | Commented { instruction :: Instruction, comment :: String }
                 deriving (Eq)

-- This prints every instruction in it's textual format.
instance Show Instruction where
    show (IAdd) = "iadd"
    show (FAdd) = "fadd"
    show (ISub) = "isub"
    show (FSub) = "fsub"
    show (IMul) = "imul"
    show (FMul) = "fmul"
    show (IDiv) = "idiv"
    show (FDiv) = "fdiv"
    show (IRem) = "irem"
    show (INeg) = "ineg"
    show (FNeg) = "fneg"
    show (BNot) = "bnot"
    show (IInc1 {location=l}) = "iinc_1 " ++ show l
    show (IInc {location=l, constant=c}) = "iinc " ++ show l ++ " " ++ show c
    show (IDec1 {location=l}) = "idec_1 " ++ show l
    show (IDec {location=l, constant=c}) = "idec " ++ show l ++ " " ++ show c
    show (BAdd) = "badd"
    show (BMul) = "bmul"
    show (INe) = "ine"
    show (IEq) = "ieq"
    show (ILt) = "ilt"
    show (ILe) = "ile"
    show (IGt) = "igt"
    show (IGe) = "ige"
    show (FNe) = "fne"
    show (FEq) = "feq"
    show (FLt) = "flt"
    show (FLe) = "fle"
    show (FGt) = "fgt"
    show (FGe) = "fge"
    show (BNe) = "bne"
    show (BEq) = "beq"
    show (Isr) = "isr"
    show (Isrn {nestingDepth=n}) = "isrn " ++ show n
    show (Isrl) = "isrl"
    show (Isrg) = "isrg"
    show (Jsr {argumentCount=a, offset=o}) = "jsr " ++ show a ++ " " ++ show o
    show (Jsre {importIndex=i}) = "jsre " ++ show i
    show (Esr {localVariableCount=l}) = "esr " ++ show l
    show (IReturn) = "ireturn"
    show (FReturn) = "freturn"
    show (BReturn) = "breturn"
    show (Return) = "return"
    show (Jump {offset=o}) = "jump " ++ show o
    show (BranchTrue {offset=o}) = "branch_t " ++ show o
    show (BranchFalse {offset=o}) = "branch_f " ++ show o
    show (ILoad {location=0}) = "iload_0"
    show (ILoad {location=1}) = "iload_1"
    show (ILoad {location=2}) = "iload_2"
    show (ILoad {location=3}) = "iload_3"
    show (ILoad {location=l}) = "iload " ++ show l
    show (FLoad {location=0}) = "fload_0"
    show (FLoad {location=1}) = "fload_1"
    show (FLoad {location=2}) = "fload_2"
    show (FLoad {location=3}) = "fload_3"
    show (FLoad {location=l}) = "fload " ++ show l
    show (BLoad {location=0}) = "bload_0"
    show (BLoad {location=1}) = "bload_1"
    show (BLoad {location=2}) = "bload_2"
    show (BLoad {location=3}) = "bload_3"
    show (BLoad {location=l}) = "bload " ++ show l
    show (ALoad {location=0}) = "aload_0"
    show (ALoad {location=1}) = "aload_1"
    show (ALoad {location=2}) = "aload_2"
    show (ALoad {location=3}) = "aload_3"
    show (ALoad {location=l}) = "aload " ++ show l
    show (ILoadN {nestingDepth=n, location=l}) = "iloadn " ++ show n ++ " " ++ show l
    show (FLoadN {nestingDepth=n, location=l}) = "floadn " ++ show n ++ " " ++ show l
    show (BLoadN {nestingDepth=n, location=l}) = "bloadn " ++ show n ++ " " ++ show l
    show (ALoadN {nestingDepth=n, location=l}) = "aloadn " ++ show n ++ " " ++ show l
    show (ILoadG {globalIndex=g}) = "iloadg " ++ show g
    show (FLoadG {globalIndex=g}) = "floadg " ++ show g
    show (BLoadG {globalIndex=g}) = "bloadg " ++ show g
    show (ALoadG {globalIndex=g}) = "aloadg " ++ show g
    show (ILoadE {importIndex=e}) = "iloade " ++ show e
    show (FLoadE {importIndex=e}) = "floade " ++ show e
    show (BLoadE {importIndex=e}) = "bloade " ++ show e
    show (ALoadE {importIndex=e}) = "aloade " ++ show e
    show (ILoadC {constantIndex=c}) = "iloadc " ++ show c
    show (FLoadC {constantIndex=c}) = "floadc " ++ show c
    show (BLoadC {constantIndex=c}) = "bloadc " ++ show c
    show (ILoadZero) = "iloadc_0"
    show (FLoadZero) = "floadc_0"
    show (BLoadTrue) = "bloadc_t"
    show (ILoadOne) = "iloadc_1"
    show (FLoadOne) = "floadc_1"
    show (BLoadFalse) = "bloadc_f"
    show (ILoadMinusOne) = "iloadc_m1"
    show (IStore {location=l}) = "istore " ++ show l
    show (FStore {location=l}) = "fstore " ++ show l
    show (BStore {location=l}) = "bstore " ++ show l
    show (AStore {location=l}) = "astore " ++ show l
    show (IStoreN {nestingDepth=n, location=l}) = "istoren " ++ show n ++ " " ++ show l
    show (FStoreN {nestingDepth=n, location=l}) = "fstoren " ++ show n ++ " " ++ show l
    show (BStoreN {nestingDepth=n, location=l}) = "bstoren " ++ show n ++ " " ++ show l
    show (IStoreG {globalIndex=g}) = "istoreg " ++ show g
    show (FStoreG {globalIndex=g}) = "fstoreg " ++ show g
    show (BStoreG {globalIndex=g}) = "bstoreg " ++ show g
    show (AStoreG {globalIndex=g}) = "astoreg " ++ show g
    show (IStoreE {exportIndex=e}) = "istoree " ++ show e
    show (FStoreE {exportIndex=e}) = "fstoree " ++ show e
    show (BStoreE {exportIndex=e}) = "bstoree " ++ show e
    show (AStoreE {exportIndex=e}) = "astoree " ++ show e
    show (INewA) = "inewa"
    show (FNewA) = "fnewa"
    show (BNewA) = "bnewa"
    show (ILoadA) = "iloada"
    show (FLoadA) = "floada"
    show (BLoadA) = "bloada"
    show (IStoreA) = "istorea"
    show (FStoreA) = "fstorea"
    show (BStoreA) = "bstorea"
    show (I2F) = "i2f"
    show (F2I) = "f2i"
    show (IPop) = "ipop"
    show (FPop) = "fpop"
    show (BPop) = "bpop"
    show (Label name) = name ++ ":"
    show (ConstantDeclaration v) = ".const " ++ mangle v ++ " " ++ show' v
        where
            mangle :: Ast.LiteralValue -> String
            mangle (Ast.FloatValue _)   = "float"
            mangle (Ast.IntegerValue _) = "int"

            show' :: Ast.LiteralValue -> String
            show' (Ast.FloatValue v)   = show v
            show' (Ast.IntegerValue v) = show v
    show (GlobalDeclaration t) = ".global " ++ mangle t
        where
            mangle :: Type.TypeElem -> String
            mangle (Type.Float 0) = "float"
            mangle (Type.Float _) = "float[]"
            mangle (Type.Bool 0)  = "bool"
            mangle (Type.Bool _)  = "bool[]"
            mangle (Type.Int 0)   = "int"
            mangle (Type.Int _)   = "int[]"
    show (VariableExport n i) = ".exportvar \"" ++ n ++ "\" " ++ show i
    show (VariableImport n t) = ".importvar \"" ++ n ++ "\" " ++ mangle t
        where
            mangle :: Type.TypeElem -> String
            mangle (Type.Float 0) = "float"
            mangle (Type.Float _) = "float[]"
            mangle (Type.Bool 0)  = "bool"
            mangle (Type.Bool _)  = "bool[]"
            mangle (Type.Int 0)   = "int"
            mangle (Type.Int _)   = "int[]"
    show (FunctionImport h) =  ".importfun \""
                            ++ (mangledNameOf h)
                            ++ "\" "
                            ++ (intercalate " " $ map mangle $ fromRight $ HasType.typeOf h)
        where
            mangle :: Type.TypeElem -> String
            mangle (Type.Float 0) = "float"
            mangle (Type.Float _) = "float[]"
            mangle (Type.Bool 0)  = "bool"
            mangle (Type.Bool _)  = "bool[]"
            mangle (Type.Int 0)   = "int"
            mangle (Type.Int _)   = "int[]"
            mangle (Type.Void)    = "void"
    show (FunctionExport h) =  ".exportfun \""
                            ++ (mangledNameOf h)
                            ++ "\" "
                            ++ (intercalate " " $ map mangle $ fromRight $ HasType.typeOf h)
                            ++ " "
                            ++ (mangledNameOf h)
        where
            mangle :: Type.TypeElem -> String
            mangle (Type.Float 0) = "float"
            mangle (Type.Float _) = "float[]"
            mangle (Type.Bool 0)  = "bool"
            mangle (Type.Bool _)  = "bool[]"
            mangle (Type.Int 0)   = "int"
            mangle (Type.Int _)   = "int[]"
            mangle (Type.Void)    = "void"
    show (Commented i c) = show i ++ " ; " ++ c

-- Sometimes needed for calculating the 
length :: Instruction -> Int16
length (IInc {}) = 4
length (IInc1 {}) = 2
length (IDec {}) = 4
length (IDec1 {}) = 2
length (Isrn {}) = 2
length (Jsr {}) = 4
length (Jsre {}) = 3
length (Esr {}) = 2
length (Jump {}) = 3
length (BranchTrue {}) = 3
length (BranchFalse {}) = 3
length (ILoad {location=loc})
    | loc > 3   = 2
    | otherwise = 1
length (FLoad {location=loc})
    | loc > 3   = 2
    | otherwise = 1
length (BLoad {location=loc})
    | loc > 3   = 2
    | otherwise = 1
length (ALoad {location=loc})
    | loc > 3   = 2
    | otherwise = 1
length (ILoadN {}) = 3
length (FLoadN {}) = 3
length (BLoadN {}) = 3
length (ALoadN {}) = 3
length (ILoadG {}) = 3
length (FLoadG {}) = 3
length (BLoadG {}) = 3
length (ALoadG {}) = 3
length (ILoadE {}) = 3
length (FLoadE {}) = 3
length (BLoadE {}) = 3
length (ALoadE {}) = 3
length (ILoadC {}) = 3
length (FLoadC {}) = 3
length (BLoadC {}) = 3
length (IStore {}) = 2
length (FStore {}) = 2
length (BStore {}) = 2
length (AStore {}) = 2
length (IStoreN {}) = 3
length (FStoreN {}) = 3
length (BStoreN {}) = 3
length (IStoreG {}) = 3
length (FStoreG {}) = 3
length (BStoreG {}) = 3
length (AStoreG {}) = 3
length (IStoreE {}) = 3
length (FStoreE {}) = 3
length (BStoreE {}) = 3
length (AStoreE {}) = 3
length (Label {}) = 0
length (ConstantDeclaration {}) = 0
length (GlobalDeclaration {}) = 0
length (VariableExport {}) = 0
length (VariableImport {}) = 0
length (FunctionExport {}) = 0
length (FunctionImport {}) = 0
length (Commented i _) = length i
length _ = 1

type ConstantTable = Map.Map Ast.LiteralValue Word16

-- Infix operators used in most (if not all) of the encode functions.
-- It's a pair of functions used to chain encode oerations

-- An operator which given a tuple of a label generator and a list of structions
-- and a function that given a label generator makes a new tuple of a label generator
-- and more instructions. Makes a new tuple.
-- This tuple contains the label generator returned by the last function
-- allong with the concatenated sequence of instructions.
infixl 8 >.>
(>.>) :: (b, [a]) -> (b -> (b, [a])) -> (b, [a])
(v, xs) >.> f = (fst $ f v, xs ++ (snd $ f v))

infixl 8 >++>
(>++>) :: (b, [a]) -> [a] -> (b, [a])
(v, xs) >++> ys = (v, xs ++ ys)

-- The class used to encode the entire Ast into assembly.
class Encodable a where
    encode :: ConstantTable -> LabelGenerator -> a -> (LabelGenerator, [Instruction])

instance (Encodable a, Foldable t) => Encodable (t a) where
    encode c lg xs = foldl f (lg, []) xs
        where
            f :: (LabelGenerator, [Instruction]) -> a -> (LabelGenerator, [Instruction])
            f x y = x >.> (flip (encode c) y)

instance Encodable Ast.Ast where
    encode c lg (Ast.Ast prog) = encode c lg prog

instance Encodable Ast.Program where
    encode c lg (Ast.Program decls) = encode c lg decls

instance Encodable Ast.Declaration where
    encode c lg (Ast.FunctionDefinition h _ b _) = (encode c lg h) >.> (flip (encode c) b)
    encode _ lg _ = (lg, [])

instance Encodable Ast.LocalFunctionDeclaration where
    encode c lg (Ast.LocalFunctionDeclaration h b _) = (encode c lg h) >.> (flip (encode c) b)

instance Encodable Ast.FunctionHeader where
    encode _ lg h = (lg, [Label $ mangledNameOf h])

instance Encodable Ast.FunctionBody where
    encode c lg (Ast.FunctionBody vars funcs body (Just st) _) =    (lg, [])
                                                               >++> enter
                                                               >.>  (flip (foldEncode c)
                                                                          (zip (map Ast.declaration vars)
                                                                               (repeat st)))
                                                               >.>  (flip (encode c) body)
                                                               >++> [Return]
                                                               >.>  (flip (encode c) funcs)
        where
            enter = [Esr $ f st]

            -- Figure out how many local variables the function has.
            -- If the function has an array that initialized with a single expression e.g int[2] = 7;
            -- Then we need three extra variables to hold the induction and stop variables used for the loop
            -- and the value itself. (Technically the last one isn't true as it can be loaded from the array after
            -- the first store but this should be better)
            -- This could be done without the extra local variables if there were stack copy and shuffle instructions.
            f :: Ast.SymbolTable -> Word8
            f (Ast.SymbolTable _ vars _) = if hasLoopBasedInit then (varCount + 3) else varCount
                where
                    varCount = fromIntegral $ Prelude.length vars

                    hasLoopBasedInit = any p $ map snd vars

                    p :: Ast.VariableDefinition -> Bool
                    p (Ast.ArrayDefinition {Ast.arrayInitializer=(Just (Ast.ArrayInitializerExpression _ _))}) = True
                    p _ = False

            foldEncode :: ConstantTable
                       -> LabelGenerator
                       -> [(Ast.VariableDefinition, Ast.SymbolTable)]
                       -> (LabelGenerator, [Instruction])
            foldEncode c lg xs = foldl f (lg, []) xs
                where
                    f :: (LabelGenerator, [Instruction])
                      -> (Ast.VariableDefinition, Ast.SymbolTable)
                      -> (LabelGenerator, [Instruction])
                    f x y = x >.> (flip (encode' c) y)

            encode' :: ConstantTable
                    -> LabelGenerator
                    -> (Ast.VariableDefinition, Ast.SymbolTable)
                    -> (LabelGenerator, [Instruction])
            encode' c lg (Ast.VariableDefinition t id (Just init) orig _ loc, st) = encode c lg init >++> handle loc' t
                where
                    loc' = locationOfVariable id st
                    
                    -- Handles storing the variable of different types in only current scopes.
                    handle :: Location -> Ast.BasicType -> [Instruction]        
                    handle (Nested 0 l) (Ast.Float _) = [Commented (FStore l) (show loc)]
                    handle (Nested 0 l) (Ast.Bool _) = [Commented (BStore l) (show loc)]
                    handle (Nested 0 l) (Ast.Int _) = [Commented (IStore l) (show loc)]
            
            -- First just plain old array allocation. No initializer.
            -- Just evaluate all the dimension sizes and multiply them together.
            -- And then store it in the appropriate location.
            encode' c lg (Ast.ArrayDefinition t id dims Nothing orig _ loc, st) =    encode c lg dims
                                                                                >++> collapse dims
                                                                                >++> handle loc' t
                where
                    loc' = locationOfVariable id st

                    -- Handles storage of said array.
                    handle :: Location -> Ast.BasicType -> [Instruction]
                    handle (Nested 0 l) (Ast.Float _) = [FNewA, Commented (AStore l) (show loc)]
                    handle (Nested 0 l) (Ast.Bool _) = [BNewA, Commented (AStore l) (show loc)]
                    handle (Nested 0 l) (Ast.Int _) = [INewA, Commented (AStore l) (show loc)]

                    -- Insert the appropriate amount of IMul instructions...
                    collapse :: (Ne.NonEmpty Ast.Expression) -> [Instruction]
                    collapse dims = Prelude.replicate ((Ne.length dims) - 1) IMul

            -- Now the hard one.
            -- Encode a local array with an array based initializer.
            -- First is the same as above just allocate a new array.
            -- However instead of after the length calcuation immediatly using the it for allocating
            -- the array we store it and load it.
            -- Remember we have two extra variables at the end for use during initalization.
            encode' c lg (Ast.ArrayDefinition t id dims (Just init) orig _ loc, st) =   length
                                                                                    >.> encodeInit init
                where
                    loc' = locationOfVariable id st

                    inductionLoc = Prelude.fromIntegral (Prelude.length (Ast.variables st))
                    lengthLoc = inductionLoc + 1
                    valueLoc = inductionLoc + 2

                    length = encode c lg dims >++> collapse dims

                    loadLength = [ILoad lengthLoc]
                    loadInduction = [ILoad inductionLoc]

                    loadVal = case t of (Ast.Float _) -> [FLoad valueLoc]
                                        (Ast.Bool _)  -> [BLoad valueLoc]
                                        (Ast.Int _)   -> [ILoad valueLoc]

                    loadArray = (\(Nested 0 l) -> [ALoad l]) loc'

                    arrayStore = case t of (Ast.Float _) -> [FStoreA]
                                           (Ast.Bool _)  -> [BStoreA]
                                           (Ast.Int _)   -> [IStoreA]

                    -- Handles storage of said array.
                    alloc :: Location -> Ast.BasicType -> [Instruction]
                    alloc (Nested 0 l) (Ast.Float _) = [FNewA, Commented (AStore l) (show loc)]
                    alloc (Nested 0 l) (Ast.Bool _) = [BNewA, Commented (AStore l) (show loc)]
                    alloc (Nested 0 l) (Ast.Int _) = [INewA, Commented (AStore l) (show loc)]

                    -- Insert the appropriate amount of IMul instructions...
                    collapse :: (Ne.NonEmpty Ast.Expression) -> [Instruction]
                    collapse dims = Prelude.replicate ((Ne.length dims) - 1) IMul

                    -- Copied from the global init pass.
                    -- However this leaves us with a multi-dimensional expression that needs to be collapsed
                    -- to properly work...
                    redoArrayInitializer :: String -> [Ast.Expression] -> Ast.ArrayInitializer -> [Ast.Statement]
                    redoArrayInitializer id idxs (Ast.ArrayInitializerExpression expr loc)
                            = [Ast.ElemAssignmentStatement id (Ne.fromList idxs) expr (Just st) loc]
                    redoArrayInitializer id idxs (Ast.ArrayInitializer inits _)
                            = concatMap (uncurry $ redoArrayInitializer id) elements
                        where
                            idxs' = Prelude.map (\i -> idxs ++ [constExpr i]) $ Prelude.iterate ((+) 1) 0
                            elements = Prelude.zip idxs' $ Ne.toList inits

                    constExpr :: Integer -> Ast.Expression
                    constExpr value = Ast.Expression (Ast.PassThrough unary loc) loc
                        where
                            unary = Ast.UnaryPassThrough (Ast.Literal (Ast.IntegerValue value) loc) loc

                    flatten :: Ast.Statement -> Ast.Statement
                    flatten (Ast.ElemAssignmentStatement  id exprs val (Just st) loc)
                            = Ast.ElemAssignmentStatement  id (exprs' Ne.:| []) val (Just st) loc
                        where
                            exprs' = foldl collapse
                                           (Ne.head exprs)
                                           (Prelude.zip (dimensionIds $ Ast.variableInfoOf st id)
                                                        (Ne.tail exprs))

                            dimensionIds :: Maybe Ast.VariableDefinition -> [String]
                            dimensionIds (Just (Ast.ArrayDefinition {})) = Prelude.map (\c -> id ++ "#" ++ show c)
                                                                                       (Prelude.iterate ((+) 1) 1)
                            dimensionIds (Just (Ast.ArrayDeclaration {Ast.dimensionIdentifiers=ids})) = Ne.tail ids

                            collapse :: Ast.Expression -> (String, Ast.Expression) -> Ast.Expression
                            collapse expr@(Ast.Expression _ loc) (id, (Ast.Expression expr' _))
                                    = Ast.Expression (binary (Ast.Add loc)
                                                             (mulExpr (parens expr)
                                                                      (id))
                                                             (expr'))
                                                     loc

                            mulExpr :: Ast.BinaryExpression -> String -> Ast.BinaryExpression
                            mulExpr l id = binary (Ast.Multiply loc) l $ passThrough id

                            binary :: Ast.BinaryOperator
                                   -> Ast.BinaryExpression
                                   -> Ast.BinaryExpression
                                   -> Ast.BinaryExpression
                            binary op l r = Ast.BinaryExpression l r op loc

                            passThrough :: String -> Ast.BinaryExpression
                            passThrough id = Ast.PassThrough (unary id) loc

                            unary :: String -> Ast.UnaryExpression
                            unary id = Ast.UnaryPassThrough (Ast.IdentifierExpression id (Just st) loc) loc

                            parens :: Ast.Expression -> Ast.BinaryExpression
                            parens e = Ast.PassThrough (Ast.UnaryPassThrough (Ast.ParenthesizedExpression e loc)
                                                                              loc)
                                                        loc

                    -- When we get here the length of the array has already been calculated what we need
                    -- to now is finish it up by either writing a loop for the initializer or encoding
                    -- the assigning of the individual elements if an array is used instead of a scalar expression.
                    encodeInit :: Ast.ArrayInitializer -> LabelGenerator -> (LabelGenerator, [Instruction])
                    -- Encode the array based initializer.
                    -- This is simply done by storing all the individual elements.
                    encodeInit (Ast.ArrayInitializer inits _) lg
                            =    (lg, [])
                            -- No neew to store it's length just allocate away.
                            >++> alloc loc' t
                            -- Now we just need to store every individual ellement in the initializer's list.
                            -- The simplest way to do this is just using the same initializer stuff from the global
                            -- array initialization rewriting pass.
                            >.> flip (encode c) (Prelude.map flatten initialization)
                        where
                            idxs = Prelude.map (\i -> [constExpr i]) $ Prelude.iterate ((+) 1) 0
                            elements = Prelude.zip idxs $ Ne.toList inits
                            initialization = concatMap (uncurry $ redoArrayInitializer id) elements

                    -- Encode the scalar initialization.
                    -- This is done using a loop.
                    -- First we store the length that is on the top of the stack for later...
                    encodeInit (Ast.ArrayInitializerExpression expr loc) lg
                            =    (lg'', [])
                            >++> [IStore lengthLoc]
                            >++> loadLength -- And load it again.
                            -- Allocate the array.
                            >++> alloc loc' t
                            -- Evaluate the expression.
                            >.>  flip (encode c) expr
                            -- And store it at the appropriate location.
                            >++> case t of (Ast.Float _) -> [FStore valueLoc]
                                           (Ast.Bool _)  -> [BStore valueLoc]
                                           (Ast.Int _)   -> [IStore valueLoc]
                            -- Load the value of zero into the induction variable.
                            >++> [ILoadZero, IStore inductionLoc]
                            -- Now we loop over the entire array and store the value.
                            -- First compare the induction variable to the array length.
                            >++> [Label lblStart]
                            >++> loadInduction >++> loadLength >++> [ILt]
                            -- If the induction-variable is not less than the length of the array
                            -- we are done initializing so we jump to the end.
                            >++> [BranchFalse $ LabelOffset lblEnd]
                            -- Load the value
                            >++> loadVal
                            -- Load the index (induction variable)
                            >++> loadInduction
                            -- Then the array
                            >++> loadArray
                            -- Then store all the stuff.
                            >++> arrayStore
                            -- Increment the induction variable and jump back.
                            >++> [IInc1 inductionLoc, Jump $ LabelOffset lblStart]
                            >++> [Label lblEnd]
                        where
                            (lblStart, lg') = generateLabel lg
                            (lblEnd, lg'') = generateLabel lg'
            encode' _ lg _ = (lg, [])

instance Encodable Ast.Statement where
    encode c lg (Ast.AssignmentStatement id expr (Just st) loc) = encode c lg expr >++> handle loc' t
        where
            loc' = locationOfVariable id st
            t = typeOf expr

            -- Handles storing the variable of different types in different scopes.
            handle :: Location -> Type.TypeElem -> [Instruction]
            handle (Global i) t
                | t == (Type.Float 0) = [Commented (FStoreG i) (show loc)]
                | t == (Type.Bool 0)  = [Commented (BStoreG i) (show loc)]
                | t == (Type.Int 0)   = [Commented (IStoreG i) (show loc)]
            handle (External i) t        
                | t == (Type.Float 0) = [Commented (FStoreE i) (show loc)]
                | t == (Type.Bool 0)  = [Commented (BStoreE i) (show loc)]
                | t == (Type.Int 0)   = [Commented (IStoreE i) (show loc)]
            handle (Nested 0 l) t        
                | t == (Type.Float 0) = [Commented (FStore l) (show loc)]
                | t == (Type.Bool 0)  = [Commented (BStore l) (show loc)]
                | t == (Type.Int 0)   = [Commented (IStore l) (show loc)]
            handle (Nested n l) t        
                | t == (Type.Float 0) = [Commented (FStoreN n l) (show loc)]
                | t == (Type.Bool 0)  = [Commented (BStoreN n l) (show loc)]
                | t == (Type.Int 0)   = [Commented (IStoreN n l) (show loc)]

    encode c lg (Ast.ElemAssignmentStatement id idxs value (Just st) _)
            = encode c lg value >.> (flip (encode c) idx) >++> load loc >++> store t
        where
            loc = locationOfVariable id st
            t = typeOf value
            idx = Ne.head idxs

            load :: Location -> [Instruction]
            load (Global i) = [ALoadG i]
            load (External i) = [ALoadE i]
            load (Nested 0 l) = [ALoad l]
            load (Nested n l) = [ALoadN n l]

            store :: Type.TypeElem -> [Instruction]
            store (Type.Float _) = [FStoreA]
            store (Type.Bool _)  = [BStoreA]
            store (Type.Int _)   = [IStoreA]

    encode c lg (Ast.ArrayInitializationStatement id idxs (Just table) _)
            =    encode c lg idxs
            >++> replicate ((Ne.length idxs) - 1) IMul
            >++> handle (typeOf $ fromJust $ Ast.variableInfoOf table id)
            >++> store (locationOfVariable id table)
        where
            handle :: Type.TypeElem -> [Instruction]
            handle (Type.Float _) = [FNewA]
            handle (Type.Bool _)  = [BNewA]
            handle (Type.Int _)   = [INewA]

            store :: Location -> [Instruction]
            store (Global i)   = [AStoreG i]
            store (Nested 0 l) = [AStore  l]

    encode c lg (Ast.FunctionCallStatement f _) = encode c lg f >++> handle (typeOf f)
        where
            handle :: Type.TypeElem -> [Instruction]
            handle (Type.Void)    = []
            handle (Type.Float _) = [FPop]
            handle (Type.Bool _)  = [BPop]
            handle (Type.Int _)   = [IPop]

    encode c lg (Ast.IfStatement condition ifB Nothing _) = condInstr >++> jumper >.> bodyInstr >++> [Label lbl]
        where
            (lbl, lg') = generateLabel lg
            condInstr = encode c lg' condition
            bodyInstr = (flip (encode c) ifB)
            jumper = [BranchFalse $ LabelOffset lbl]
    encode c lg (Ast.IfStatement condition ifB (Just elseB) _)
            =    condInstr
            >++> jumper
            >.>  bodyInstr
            >++> [Jump $ LabelOffset lbl2, Label lbl1]
            >.>  elseInstr
            >++> [Label lbl2]
        where
            (lbl1, lg') = generateLabel lg
            (lbl2, lg'') = generateLabel lg'
            condInstr = encode c lg'' condition
            elseInstr = flip (encode c) elseB
            bodyInstr = flip (encode c) ifB
            jumper    = [BranchFalse $ LabelOffset lbl1]

    encode c lg (Ast.DoWhileStatement condition body _) = (lg', [Label lbl]) >.> bodyInstr >.> condInstr >++> jumper
        where
            (lbl, lg') = generateLabel lg
            bodyInstr = flip (encode c) body
            condInstr = flip (encode c) condition
            jumper    = [BranchTrue $ LabelOffset lbl]
    
    encode c lg (Ast.ReturnStatement Nothing _ loc) = (lg, [Commented Return (show loc)])
    encode c lg (Ast.ReturnStatement (Just expr) _ loc) = encode c lg expr >++> handle (typeOf expr)
        where
            handle :: Type.TypeElem -> [Instruction]
            handle (Type.Float _) = [Commented FReturn (show loc)]
            handle (Type.Bool _)  = [Commented BReturn (show loc)]
            handle (Type.Int _)   = [Commented IReturn (show loc)]


instance Encodable Ast.FunctionCall where
    encode c lg (Ast.FunctionCall fn args (Just st) loc) = (lg, f loc') >.> flip (encode c) args >++> g loc'
        where
            loc' = locationOfFunction fn st

            f :: Location -> [Instruction]
            f (Global _)   = [Commented Isrg (show loc)]
            f (External _) = [Commented Isrg (show loc)]
            f (Nested 0 _) = [Commented Isrl (show loc)]
            f (Nested 1 _) = [Commented Isr (show loc)]
            f (Nested n _) = [Commented (Isrn (n - 1)) (show loc)]

            g :: Location -> [Instruction]
            g (External i) = [Commented (Jsre i) (show loc)]
            g _            = [Commented (Jsr (fromIntegral $ length args)
                                             (LabelOffset $ mangledNameOf $ fromJust $ Ast.functionInfoOf st fn))
                                        (show loc)]
                where
                    length :: [Ast.Expression] -> Int
                    length [] = 0
                    length (expr : exprs) = l' (typeOf expr) + length exprs
                        where
                            l' :: Type.TypeElem -> Int
                            l' (Type.Float i) = i + 1
                            l' (Type.Int i) = i + 1
                            l' (Type.Bool i) = i + 1

instance Encodable Ast.Expression where
    encode c lg (Ast.Expression e _) = encode c lg e

instance Encodable Ast.BinaryExpression where
    encode c lg (Ast.BinaryExpression l r (Ast.Multiply _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FMul]
        | typeOf l == (Type.Bool 0)  = encode c lg l >.> flip (encode c) r >++> [BMul]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [IMul]
    encode c lg (Ast.BinaryExpression l r (Ast.Add _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FAdd]
        | typeOf l == (Type.Bool 0)  = encode c lg l >.> flip (encode c) r >++> [BAdd]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [IAdd]
    encode c lg (Ast.BinaryExpression l r (Ast.Subtract _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FSub]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [ISub]
    encode c lg (Ast.BinaryExpression l r (Ast.Divide _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FDiv]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [IDiv]
    encode c lg (Ast.BinaryExpression l r (Ast.Modulus _) _)
                                     = encode c lg l >.> flip (encode c) r >++> [IRem]
    encode c lg (Ast.BinaryExpression l r (Ast.Equal _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FEq]
        | typeOf l == (Type.Bool 0)  = encode c lg l >.> flip (encode c) r >++> [BEq]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [IEq]
    encode c lg (Ast.BinaryExpression l r (Ast.NotEqual _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FNe]
        | typeOf l == (Type.Bool 0)  = encode c lg l >.> flip (encode c) r >++> [BNe]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [INe]
    encode c lg (Ast.BinaryExpression l r (Ast.LessThan _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FLt]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [ILt]
    encode c lg (Ast.BinaryExpression l r (Ast.LessEqual _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FLe]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [ILe]
    encode c lg (Ast.BinaryExpression l r (Ast.GreaterThan _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FGt]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [IGt]
    encode c lg (Ast.BinaryExpression l r (Ast.GreaterEqual _) _)
        | typeOf l == (Type.Float 0) = encode c lg l >.> flip (encode c) r >++> [FGe]
        | typeOf l == (Type.Int 0)   = encode c lg l >.> flip (encode c) r >++> [IGe]

    -- If the left-hand side if the expression is true the right hand side is ignored and true is returned.
    -- We accomplish by skipping over the instructions of the right hand side and loading the constant true.
    -- If the left hand side is false we evaluate the right hand side and leave on the top of the stack.
    encode c lg (Ast.BinaryExpression l r (Ast.Or _) _)
            =    el
            >++> [BranchTrue $ LabelOffset lbl2]
            >.>  er
            >++> [Jump $ LabelOffset lbl1]
            >++> [Label lbl2]
            >++> [BLoadTrue]
            >++> [Label lbl1]
        where
            (lbl1, lg') = generateLabel lg -- The label signifying the end of the binary operation.
            (lbl2, lg'') = generateLabel lg' -- The label which is jumped to when the left hand side is true.
            el = encode c lg'' l
            er = flip (encode c) r

    -- If the left-hand side of the expression is true it is the result of the right hand side.
    -- Otherwise it is false.
    -- We handle this by first evaluating the left hand side.
    -- If it is false we we load false onto the top of the stack and jump to the end of the expression.
    -- Otherwise we evaluate the right hand side and leave it at the top of the stack.
    encode c lg (Ast.BinaryExpression l r (Ast.And _) _)
            =    el                                 -- First evaluate the left hand side.
            >++> [BranchFalse $ LabelOffset lbl2]   -- Figure out if we also need to evaluate the right hand side.
            >.>  er                                 -- Evaluate the right hand side if the jump was not taken and the left hand side was true.
            >++> [Jump $ LabelOffset lbl1]          -- Jump to the end of the expression.
            >++> [Label lbl2]                       
            >++> [BLoadFalse]
            >++> [Label lbl1]
        where
            (lbl1, lg') = generateLabel lg -- Represents the jump to the end of the expression.
            (lbl2, lg'') = generateLabel lg' -- Represents the label we jump to for loading false and finishing the expression.
            el = encode c lg'' l
            er = flip (encode c) r
    encode c lg (Ast.PassThrough e _) = encode c lg e


instance Encodable Ast.UnaryExpression where
    encode c lg (Ast.UnaryExpression (Ast.Not _) e _) = encode c lg e >++> [BNot]
    encode c lg (Ast.UnaryExpression (Ast.Negate _) e _)
        | typeOf e == (Type.Float 0) = encode c lg e >++> [FNeg]
        | typeOf e == (Type.Int 0)   = encode c lg e >++> [INeg]
    encode c lg (Ast.CastExpression (Ast.Int _) e _)
        | typeOf e == (Type.Float 0) = encode c lg e >++> [F2I]
        | typeOf e == (Type.Bool 0)  = encode c lg e >.> f
        | typeOf e == (Type.Int 0)   = encode c lg e
            where
                f lg = (lg'', code)
                    where
                        (lbl, lg') = generateLabel lg
                        (lbl', lg'') = generateLabel lg'
                        code = [ BranchFalse $ LabelOffset lbl
                               , ILoadOne
                               , Jump $ LabelOffset lbl'
                               , Label lbl
                               , ILoadZero
                               , Label lbl'
                               ]
    encode c lg (Ast.CastExpression (Ast.Float _) e _)
        | typeOf e == (Type.Float 0) = encode c lg e
        | typeOf e == (Type.Bool 0)  = encode c lg e >.> f
        | typeOf e == (Type.Int 0)   = encode c lg e >++> [I2F]
            where
                f lg = (lg'', code)
                    where
                        (lbl, lg') = generateLabel lg
                        (lbl', lg'') = generateLabel lg'
                        code = [ BranchFalse $ LabelOffset lbl
                               , FLoadOne
                               , Jump $ LabelOffset lbl'
                               , Label lbl
                               , FLoadZero
                               , Label lbl'
                               ]
    encode c lg (Ast.CastExpression (Ast.Bool _) e _)
        | typeOf e == (Type.Float 0) = encode c lg e >++> [FLoadZero, FNe]
        | typeOf e == (Type.Bool 0)  = encode c lg e
        | typeOf e == (Type.Int 0)   = encode c lg e >++> [ILoadZero, INe]
    encode c lg (Ast.UnaryPassThrough e _) = encode c lg e

instance Encodable Ast.BasicExpression where
    encode _ lg (Ast.Literal {Ast.value = Ast.IntegerValue 1})    = (lg, [ILoadOne])
    encode _ lg (Ast.Literal {Ast.value = Ast.IntegerValue 0})    = (lg, [ILoadZero])
    encode _ lg (Ast.Literal {Ast.value = Ast.IntegerValue (-1)}) = (lg, [ILoadMinusOne])
    encode c lg (Ast.Literal {Ast.value = Ast.IntegerValue v})    = (lg, [ILoadC $ c Map.! (Ast.IntegerValue v)])
    encode c lg (Ast.Literal {Ast.value = Ast.FloatValue v})
        | v == 0.0  = (lg, [FLoadZero])
        | v == 1.0  = (lg, [FLoadOne])
        | otherwise = (lg, [FLoadC $ c Map.! (Ast.FloatValue v)])
    encode _ lg (Ast.Literal {Ast.value = Ast.BooleanValue True})  = (lg, [BLoadTrue])
    encode _ lg (Ast.Literal {Ast.value = Ast.BooleanValue False}) = (lg, [BLoadFalse])
    encode c lg (Ast.ParenthesizedExpression {Ast.innerExpression=e}) = encode c lg e
    encode _ lg e@(Ast.IdentifierExpression id (Just st) _) = (lg, g loc $ typeOf e)
        where
            loc = locationOfVariable id st
            -- TODO: Also load arrays...
            g :: Location -> Type.TypeElem -> [Instruction]
            g     (Nested 0 l) (Type.Float 0) = [FLoad l]
            g     (Nested 0 l) (Type.Bool 0)  = [BLoad l]
            g     (Nested 0 l) (Type.Int 0)   = [ILoad l]
            g     (Nested n l) (Type.Float 0) = [FLoadN n l]
            g     (Nested n l) (Type.Bool 0)  = [BLoadN n l]
            g     (Nested n l) (Type.Int 0)   = [ILoadN n l]
            g     (Global i)   (Type.Float 0) = [FLoadG i]
            g     (Global i)   (Type.Bool 0)  = [BLoadG i]
            g     (Global i)   (Type.Int 0)   = [ILoadG i]
            g     (External i) (Type.Float 0) = [FLoadE i]
            g     (External i) (Type.Bool 0)  = [BLoadE i]
            g     (External i) (Type.Int 0)   = [ILoadE i]
            g loc@(Nested 0 l) (Type.Float d) = loadDimensions loc d ++ [ALoad l]
            g loc@(Nested 0 l) (Type.Bool d)  = loadDimensions loc d ++ [ALoad l]
            g loc@(Nested 0 l) (Type.Int d)   = loadDimensions loc d ++ [ALoad l]
            g loc@(Nested n l) (Type.Float d) = loadDimensions loc d ++ [ALoadN n l]
            g loc@(Nested n l) (Type.Bool d)  = loadDimensions loc d ++ [ALoadN n l]
            g loc@(Nested n l) (Type.Int d)   = loadDimensions loc d ++ [ALoadN n l]
            g loc@(Global i)   (Type.Float d) = loadDimensions loc d ++ [ALoadG i]
            g loc@(Global i)   (Type.Bool d)  = loadDimensions loc d ++ [ALoadG i]
            g loc@(Global i)   (Type.Int d)   = loadDimensions loc d ++ [ALoadG i]
            g loc@(External i) (Type.Float d) = loadDimensions loc d ++ [ALoadE i]
            g loc@(External i) (Type.Bool d)  = loadDimensions loc d ++ [ALoadE i]
            g loc@(External i) (Type.Int d)   = loadDimensions loc d ++ [ALoadE i]


            loadDimensions :: Location -> Int -> [Instruction]
            loadDimensions     (Nested 0 l) 1 = [ILoad (l - 1)]
            loadDimensions loc@(Nested 0 l) a = (ILoad (l - fromIntegral a)) : loadDimensions loc (a - 1)
            loadDimensions     (Nested n l) 1 = [ILoadN n (l - 1)]
            loadDimensions loc@(Nested n l) a = (ILoadN n (l - fromIntegral a)) : loadDimensions loc (a - 1)
            loadDimensions     (Global i)   1 = [ILoadG (i - 1)]
            loadDimensions loc@(Global i)   a = (ILoadG (i - fromIntegral a)) : loadDimensions loc (a - 1)
            loadDimensions     (External i) 1 = [ILoadE (i - 1)]
            loadDimensions loc@(External i) a = (ILoadE (i - fromIntegral a)) : loadDimensions loc (a - 1)
    encode c lg e@(Ast.ArrayExpression id expr (Just st) _) = encode c lg idx >++> f loc >++> g (typeOf e)
        where
            loc = locationOfVariable id st
            idx = Ne.head expr

            f :: Location -> [Instruction]
            f (Nested 0 l) = [ALoad l]
            f (Nested n l) = [ALoadN n l]
            f (Global i)   = [ALoadG i]
            f (External i) = [ALoadE i]

            g :: Type.TypeElem -> [Instruction]
            g (Type.Float 0) = [FLoadA]
            g (Type.Bool 0)  = [BLoadA]
            g (Type.Int 0)   = [ILoadA]
    encode c lg (Ast.FunctionCallExpression callee _) = encode c lg callee

typeOf :: HasType.HasType ht => ht -> Type.TypeElem
typeOf typed = Type.t' $ fromRight (HasType.typeOf typed)

-- Utility function to force an Either to it's right value
-- throwing an error when encountering a left.
fromRight :: Show a => Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error ("You can't make a Right from a Left (Compiler/CodeGen.hs : fromRight) " ++ show a)

-- Figures out the total number of bytes needed for a sequence of instruction.
lengthOf :: [Instruction] -> Int16
lengthOf instructions = foldl (flip $ (+) . length) 0 instructions

data Location = Global { index :: Word16 }
              | External { index :: Word16 }
              | Nested { depth :: Word8, frameOffset :: Word8 }
              deriving (Show, Eq)
              
locationOfVariable :: String -> Ast.SymbolTable -> Location
locationOfVariable s (Ast.SymbolTable Nothing vars _) = indexOf s vars (Global 0) (External 0)
    where
        indexOf :: String -> [(String, Ast.VariableDefinition)] -> Location -> Location -> Location
        indexOf s ((n, v) : vs) (Global g) (External e)
            | s == n && Ast.origin v == Ast.Imported = (External e)
            | s == n                                 = (Global g)
            | Ast.origin v == Ast.Imported           = indexOf s vs (Global g)       (External (e + 1))
            | otherwise                              = indexOf s vs (Global (g + 1)) (External e)
locationOfVariable s (Ast.SymbolTable parent vars _)
    | member s vars = Nested 0 $ indexOf s vars
    | otherwise     = g $ locationOfVariable s $ fromJust parent
    where
        member :: String -> [(String, Ast.VariableDefinition)] -> Bool
        member _ [] = False
        member s ((x, _) : xs)
            | s == x    = True
            | otherwise = member s xs
        
        g :: Location -> Location
        g (Nested n i) = Nested (n + 1) i
        g l = l

        indexOf :: String -> [(String, Ast.VariableDefinition)] -> Word8
        indexOf s v = f s v 0
            where
                f s ((n, v) : vs) i
                    | s == n    = i
                    | otherwise = f s vs (i + 1)

locationOfFunction :: String -> Ast.SymbolTable -> Location
locationOfFunction s (Ast.SymbolTable Nothing _ funcs) = indexOf s funcs (Global 0) (External 0)
    where
        indexOf :: String -> [(String, Ast.FunctionHeader)] -> Location -> Location -> Location
        indexOf s ((n, v) : vs) (Global 0) (External e)
            | s == n && Ast.functionOrigin v == Ast.Imported = (External e)
            | s == n                                         = (Global 0)
            | Ast.functionOrigin v == Ast.Imported           = indexOf s vs (Global 0) (External (e + 1))
            | otherwise                                      = indexOf s vs (Global 0) (External e)
locationOfFunction s (Ast.SymbolTable parent _ funcs)
    | member s funcs = Nested 0 0
    | otherwise      = g $ locationOfFunction s $ fromJust parent
    where
        member :: String -> [(String, Ast.FunctionHeader)] -> Bool
        member _ [] = False
        member s ((x, _) : xs)
            | s == x    = True
            | otherwise = member s xs

        g :: Location -> Location
        g (Nested n i) = Nested (n + 1) i
        g l = l

mangledNameOf :: Ast.FunctionHeader -> String
mangledNameOf (Ast.FunctionHeader _ id _ Nothing _ _ _) = id
mangledNameOf (Ast.FunctionHeader _ id _ (Just parent) _ _ _) = mangledNameOf parent ++ "#" ++ id

-- Class to extract all the constants from a program.
class Extractable a where
    extractConstants :: a -> Set.Set Ast.LiteralValue

instance (Extractable a, Foldable t) => Extractable (t a) where
    extractConstants xs = foldl (flip $ Set.union . extractConstants) Set.empty xs

instance Extractable Ast.Ast where
    extractConstants (Ast.Ast prog) = extractConstants prog

instance Extractable Ast.Program where
    extractConstants (Ast.Program decls) = extractConstants decls

instance Extractable Ast.Declaration where
    extractConstants (Ast.FunctionDefinition _ _ body _) = extractConstants body
    extractConstants _ = Set.empty

instance Extractable Ast.LocalFunctionDeclaration where
    extractConstants (Ast.LocalFunctionDeclaration _ body _) = extractConstants body

instance Extractable Ast.FunctionBody where
    extractConstants (Ast.FunctionBody vars funcs stmnts _ _) = Set.unions [f', s', v']
        where
            v' = extractConstants vars
            f' = extractConstants funcs
            s' = extractConstants stmnts

instance Extractable Ast.LocalDeclaration where
    extractConstants = extractConstants . Ast.declaration

instance Extractable Ast.VariableDefinition where
    extractConstants (Ast.VariableDefinition _ _ (Just init) _ _ _) = extractConstants init
    extractConstants (Ast.ArrayDefinition _ _ dims init _ _ _) = Set.union d' i'
        where
            d' = extractConstants dims
            i' = extractConstants init
    extractConstants _ = Set.empty

instance Extractable Ast.ArrayInitializer where
    extractConstants (Ast.ArrayInitializer inits _) = extractConstants inits
    extractConstants (Ast.ArrayInitializerExpression expr _) = extractConstants expr

instance Extractable Ast.Statement where
    extractConstants (Ast.AssignmentStatement _ expr _ _) = extractConstants expr
    extractConstants (Ast.ElemAssignmentStatement _ idxs expr _ _) = Set.union c c'
        where
            c = extractConstants expr
            c' = extractConstants idxs
    extractConstants (Ast.ArrayInitializationStatement _ idxs _ _) = extractConstants idxs
    extractConstants (Ast.FunctionCallStatement call _) = extractConstants call
    extractConstants (Ast.IfStatement con ifB elseB _) = Set.unions [c, c', c'']
        where
            c = extractConstants con
            c' = extractConstants ifB
            c'' = extractConstants elseB
    extractConstants (Ast.DoWhileStatement con body _) = Set.union c c'
        where
            c = extractConstants con
            c' = extractConstants body
    extractConstants (Ast.ReturnStatement expr _ _) = extractConstants expr

instance Extractable Ast.FunctionCall where
    extractConstants (Ast.FunctionCall _ args _ _) = extractConstants args

instance Extractable Ast.Expression where
    extractConstants (Ast.Expression expr _) = extractConstants expr

instance Extractable Ast.BinaryExpression where
    extractConstants (Ast.BinaryExpression l r _ _) = Set.union c c'
        where
            c = extractConstants l
            c' = extractConstants r
    extractConstants (Ast.PassThrough expr _) = extractConstants expr

instance Extractable Ast.UnaryExpression where
    extractConstants (Ast.UnaryExpression _ expr _) = extractConstants expr
    extractConstants (Ast.CastExpression _ expr _) = extractConstants expr
    extractConstants (Ast.UnaryPassThrough expr _) = extractConstants expr

instance Extractable Ast.BasicExpression where
    extractConstants (Ast.Literal (Ast.IntegerValue v) _)
        | v == 0    = Set.empty
        | v == 1    = Set.empty
        | v == -1   = Set.empty
        | otherwise = Set.singleton (Ast.IntegerValue v)
    extractConstants (Ast.Literal (Ast.FloatValue v) _)
        | v == 0.0  = Set.empty
        | v == 1.0  = Set.empty
        | otherwise = Set.singleton (Ast.FloatValue v)
    extractConstants (Ast.Literal { Ast.value = Ast.BooleanValue {}}) = Set.empty
    extractConstants (Ast.ParenthesizedExpression e _) = extractConstants e
    extractConstants (Ast.FunctionCallExpression callee _) = extractConstants callee
    extractConstants (Ast.IdentifierExpression {}) = Set.empty
    extractConstants (Ast.ArrayExpression _ idxs _ _) = extractConstants idxs

data LabelGenerator = LabelGenerator { counter :: Int }

generateLabel :: LabelGenerator -> (String, LabelGenerator)
generateLabel (LabelGenerator c) = ("L#" ++ show c, LabelGenerator (c + 1))


declareGlobals :: Foldable t => t Ast.Declaration -> [Instruction]
declareGlobals decls = foldl g [] decls
    where
        g :: [Instruction] -> Ast.Declaration -> [Instruction]
        g instr (Ast.GlobalDefinition def _ _) = instr ++ [GlobalDeclaration $ typeOf def]
        g instr _ = instr

exportVariables :: Foldable t => t Ast.Declaration -> [Instruction]
exportVariables decls = snd $ foldl g (0, []) decls
    where
        g :: (Word16, [Instruction]) -> Ast.Declaration -> (Word16, [Instruction])
        g (c, instructions) (Ast.GlobalDefinition def True _)
                = (c + 1, instructions ++ [VariableExport (Ast.variableName def) c])
        g (c, instructions) (Ast.GlobalDefinition _ False _)
                = (c + 1, instructions)
        g v _ = v

importVariables :: Foldable t => t Ast.Declaration -> [Instruction]
importVariables decls = foldl g [] decls
    where
        g :: [Instruction] -> Ast.Declaration -> [Instruction]
        g instr (Ast.GlobalDeclaration def@(Ast.ArrayDeclaration {}) _)
                =  instr
                ++ (Prelude.map (\n -> VariableImport n (Type.Int 0))
                                (Prelude.take dimensions $ Prelude.map (\c -> id ++ "#" ++ show c)
                                                                       (Prelude.iterate ((+) 1) 0)))
                ++ [VariableImport (Ast.variableName def) (typeOf def)]
            where
                id = Ast.variableName def
                dimensions = Ne.length $ Ast.dimensionIdentifiers def
        g instr (Ast.GlobalDeclaration def _) = instr ++ [VariableImport (Ast.variableName def) (typeOf def)]
        g instr _ = instr

exportFunctions :: Foldable t => t Ast.Declaration -> [Instruction]
exportFunctions decls = foldl g [] decls
    where
        g :: [Instruction] -> Ast.Declaration -> [Instruction]
        g instr (Ast.FunctionDefinition h True _ _) = instr ++ [FunctionExport h]
        g instr _ = instr

importFunctions :: Foldable t => t Ast.Declaration -> [Instruction]
importFunctions decls = foldl g [] decls
    where
        g :: [Instruction] -> Ast.Declaration -> [Instruction]
        g instr (Ast.FunctionDeclaration h _) = instr ++ [FunctionImport h]
        g instr _ = instr

-- Applies peep-hole optimizations to the generated Assembly code.
-- The optimizer takes a list of functions and the amount of instructions
-- they process at a time.
-- And then runs the assembly through these functions in series.
-- TODO: Find a way around the comment problem.
--       Currently comments screw everything up.
--       We can either strip them defeating their purpose.
--       Or strip them and reaply them. Which is impossible when we get different instructions back...
optimize :: [Instruction] -> [Instruction]
optimize instructions = runOptimizer optimizations instructions
    where
        runOptimizer :: [(Int, ([Instruction] -> [Instruction]))] -> [Instruction] -> [Instruction]
        runOptimizer [] instructions = instructions
        runOptimizer ((d, f) : fs) xs = runOptimizer fs $ process d f xs

        optimizations = [ (2, doubleOp)
                        , (4, incrConstant)
                        , (2, branchInversion)
                        , (2, doubleReturn)
                        ]
        
        process :: Int -> ([Instruction] -> [Instruction]) -> [Instruction] -> [Instruction]
        process _ _ [] = []
        process d f xs = next (seq == xs') (xs' ++ (drop d xs))
            where
                seq = take d xs
                xs' = f seq

                -- Either retry the optimization with the current instructions
                -- if it gave as different instructions.
                -- Otherwise move on further down the list of instructions
                next :: Bool -> [Instruction] -> [Instruction]
                next False instructions = process d f instructions
                next True (x : xs) = x : (process d f xs)

-- Remove occurences of doubled operations i.e. -(-x)
doubleOp :: [Instruction] -> [Instruction]
doubleOp (INeg : INeg : []) = []
doubleOp (FNeg : FNeg : []) = []
doubleOp (BNot : BNot : []) = []
doubleOp xs = xs

-- Change the sequence x = x (+/-) c into a single instructions
-- Where c is a constant
incrConstant :: [Instruction] -> [Instruction]
incrConstant instr@(ILoad loc : ILoadC idx : IAdd : IStore loc2 : [])
    | loc == loc2 = [IInc loc idx]
    | otherwise   = instr
incrConstant instr@(ILoadC idx : ILoad loc : IAdd : IStore loc2 : [])
    | loc == loc2 = [IInc loc idx]
    | otherwise   = instr
incrConstant instr@(ILoad loc : ILoadOne : IAdd : IStore loc2 : [])
    | loc == loc2 = [IInc1 loc]
    | otherwise   = instr
incrConstant instr@(ILoadOne : ILoad loc : IAdd : IStore loc2 : [])
    | loc == loc2 = [IInc1 loc]
    | otherwise   = instr
incrConstant instr@(ILoad loc : ILoadMinusOne : IAdd : IStore loc2 : [])
    | loc == loc2 = [IDec1 loc]
    | otherwise   = instr
incrConstant instr@(ILoad loc : ILoadC idx : ISub : IStore loc2 : [])
    | loc == loc2 = [IDec loc idx]
    | otherwise   = instr
incrConstant instr@(ILoad loc : ILoadOne : ISub : IStore loc2 : [])
    | loc == loc2 = [IDec1 loc]
    | otherwise   = instr
incrConstant instructions = instructions

-- Perform branch inversion
branchInversion :: [Instruction] -> [Instruction]
branchInversion (BNot : BranchFalse lbl : []) = [BranchTrue lbl]
branchInversion (BNot : BranchTrue lbl : [])  = [BranchFalse lbl]
branchInversion instructions = instructions

-- Remove occurences of double return statements.
-- The encoder generously sprinkles return instruction add the end
-- of function bodies sometimes even when they are not needed.
doubleReturn (IReturn : Return : []) = [IReturn]
doubleReturn (FReturn : Return : []) = [FReturn]
doubleReturn (BReturn : Return : []) = [BReturn]
doubleReturn (Return : Return : []) = [Return]
doubleReturn instructions = instructions
