module Compiler.Type.Type (
    Type(..),
    TypeElem(..),
    numberClass,
    TypeCombiner(..),
    changeDimension,
    changeElemDimension,
    sameDimension,
    dimension,
    t',
    errorType
) where
import Compiler.Ast.Ast (BinaryOperator)
import Data.Sequence (adjust, fromList )
import Data.Foldable (toList)
import Data.List (intersect)
import Compiler.Ast.Print 



-- A type is a list of types that map from one to the other.
--
-- A function f with parameters p and return type r will have the type
-- t(f) = t(p) -> r 
--
-- A parameter list p = p_1 .. p_n has the type of
-- t(p) = p_1 -> ... -> p_n
--
-- Thus a function like ``int foo (bool a, float b)''
-- Has a type: (Bool 0 -> Float 0) -> Int 0 = Bool 0 -> Float 0 -> Int 0
--
-- A variable is a singleton type.
-- That is, instead of a mapping from one
-- typeElem  to another type, the variable just is a typeElem
-- A variable like ``bool a'' has the type ``Bool 0''
--
--
-- A assignment could be seen as a mapping from one type to the other.
-- An assignment of b to a has a type 
-- t(b) -> t(a) 
--
-- Thus an assignment of the form
--    a = b;
-- has the type
-- t(b) -> t(a)
-- Note here that t(a) must be a singleton type.
-- We know this because assignment
-- to a non-singleton type is disallowed in the source language and doesn't
-- really make sence either.
-- t(b) however could be a non-singleton type.
--
-- A binary expression with left operand l and right operand r and operator
-- op has the type
-- t_l -> t_r -> tOp(op, t_l, t_r)
--
-- Note the function tOp here. This is a simple hack to allow type
-- inference of operators without type variables. 
-- Basically, we would like an operator to have a type like 
-- NumberClass a => a -> a -> a
-- This allows us to to bind an instance of a TypeClass to _multiple_
-- TypeElems in a single Type. Implementing this is a bit of a hastle
-- though and would make our typesystem a lot more complicated. (CiviC
-- only requires a very weak typesystem (as in not powerfull))
--
-- The hack, which works around this problem, is that for operators like `+` ect, tOp returns numberClass
-- when the left and right operands are of different Types. When they are
-- of the same (non-typeclass) type, that Type is returned instead.
-- This hack is also used in unary expressions.
-- The type of `-` for example should be
-- NumberClass a => a -> a 
-- But since we can't do this we return
-- a -> tOp(a) instead.
-- here tOp(a) is defined to return a in case of a \in NumberClass and
-- NumberClass otherwise.
--
-- The logic behind returning NumberClass when the types don't match (both
-- in binops and uops) is that the type returned is unknown but we are
-- certain binops like `*` and `+` and the uop `-` always return a NumberClass.
-- When typechecking, we can simply check for type equality, allowing us to
-- report an error in the case of "bool a = 1.0 * 1" for example since we
-- know `*` will never return a bool. In the case of "int a = 1.0 * 1" the
-- only error produced will be the mismatched types of 1.0 and 1 (and not
-- the type mismatch of a and (1.0 * 1))
--
-- Examples:
-- ((1 * 2.0) + 3) * (8 + 1.0)
-- ((Int 0 -> Float 0 -> numberClass 0) -> Int -> numberClass 0) -> (Int -> Int -> NumberClass 0) -> NumberClass 0
--
-- ((1 * 2) * 3)
-- ((Int 0 -> Int 0) -> Int 0) -> Int 0
--
-- The nesting of TypeElems (denoted with the parenthesis) is realized using the NestedType

-- TODO: this allows nested typeclasses. Maybe it shouldn't?
-- TODO: We should specify TypeClasses using sets
data TypeElem = Bool Int | Int Int | Float Int | Void | TypeClass [TypeElem] | NestedType [TypeElem]
            deriving (Show)


instance Eq TypeElem where
        (NestedType t1) == (NestedType t2)  = t1 == t2
        (NestedType t1) == t2               = t1 == [t2]
        t1              == (NestedType t2)  = [t1] == t2
        (TypeClass tl)  == (TypeClass tl2)  = (length $ intersect tl tl2) > 0
        (TypeClass tl)  == t2               = or $ map (== t2) tl

        t1              == t2@(TypeClass _) = t2 == t1

        (Bool d1)       == (Bool d2)        = d1 == d2
        (Int d1)        == (Int d2)         = d1 == d2
        (Float d1)      == (Float d2)       = d1 == d2
        (Void)          == (Void)           = True
        _               == _                = False


numberClass :: Int -> TypeElem
numberClass i = TypeClass [Int i, Float i]


-- | The type that doesn't equal any type (not even errorType)
-- Useful to represent error types
errorType :: Type
errorType = [TypeClass []]

type Type = [TypeElem]

data TypeCombiner = BinaryCombiner BinaryOperator

instance Show TypeCombiner where
        show (BinaryCombiner op) = "operator " ++ programPrint op


-- We will store the types in reverse order in a list.
-- This way, getting the final type is O(1)
t' :: Type -> TypeElem
t' = head

changeElemDimension :: Int -> TypeElem -> TypeElem
changeElemDimension i (Bool _)  = Bool i
changeElemDimension i (Int _)   = Int i  
changeElemDimension i (Float _) = Float i
changeElemDimension _ (Void )   = Void

changeDimension :: Int -> Type -> Type
changeDimension _ [] = []
changeDimension i t = toList $ adjust (changeElemDimension i) ((length t)-1) (fromList t)

dimension :: TypeElem -> Maybe Int
dimension (Bool i)  = Just i
dimension (Int i)   = Just i
dimension (Float i) = Just i
dimension (_) = Nothing

sameDimension :: TypeElem -> TypeElem -> Bool
sameDimension t1 t2 = d1 == d2 && d1 /= Nothing
                      where
                          d1 = dimension t1
                          d2 = dimension t2
