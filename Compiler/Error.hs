module Compiler.Error (
    CompilerError(..),
    displayMarker,
    ErrorMonad,
    throwError
) where

import Prelude hiding (getLine, error)
import System.IO hiding (getLine)
import System.Environment
import Text.Printf
import Compiler.Position
import Control.Monad.Except
import Compiler.Type.Type
import Control.Lens
import Data.List (intercalate)

import Compiler.Ast.Ast (ArrayInitializer)

import qualified Compiler.Ast.Ast as Ast

data CompilerError = LexError 
                { char     :: Char
                , location :: Location 
                }

              | CommentNotClosedError
                    { location :: Location }

              | StringNotClosedError 
                    { location :: Location }

                -- | The most basic type error.
                -- We expected some type expectedType, but instead we got
                -- realType. 
                --
                -- Occurance:
                --      - Function parameter
                --      - Array access parameter
                --      - Assignment
                --      - Function return
              | UnexpectedType
                    { location     :: Location
                    , expectedType :: Type
                    , actualType   :: Type
                    }

              -- | UnexpectedType with multiple expected types.
              | UnexpectedTypeOneOf
                    { location      :: Location
                    , expectedTypes :: [Type]
                    , actualType    :: Type
                    }

                -- | Type error.
                -- The combination of types 'type1' and 'type2', combined using 'combiner', is not allowed.
                -- Occurance:
                --      - Binary expression
              | DisallowedTypeCombination
                    { location :: Location
                    , types    :: [(Type, Location)]
                    , combiner :: TypeCombiner
                    }

              | DisallowedCast
                    { location :: Location
                    , fromType :: Type
                    , toType   :: Type
                    }

              | UnknownType
                    { location :: Location }

              -- Should never occur in CiviC. Assigning something to
              -- something with a non-singleton type (like a function for
              -- example)
              | NonSingletonAssignee
                    { location       :: Location
                    , assigneeType   :: Type
                    , assignmentType :: Type
                    }

              | MismatchedParameterTypes
                    { location     :: Location
                    , expectedType :: Type
                    , actualType   :: Type
                    }

              -- | The parent function of a return is not known for
              -- whatever reason. Should never occur.
              | OrphanReturn
                    { location :: Location }

              | MismatchedArrayDimension
                    { arrayInitializer :: ArrayInitializer
                    , assumedType      :: Type
                    , mismatchedTypes  :: [(Type, Location)]
                    }

              | AmbiguousArrayType
                    { location         :: Location
                    , assumedType      :: Type
                    , encounteredTypes :: [(Type, Location)]
                    }

              -- A non-dimensional type (like Void) is encountered in an
              -- array.
              | NonDimensionalArrayType
                    { arrayInitializer   :: ArrayInitializer
                    , nonDimensionalType :: Type
                    }

              -- | Incorrect indice count for arrays
              | IncorrectIndiceCount
                    { location      :: Location
                    , expectedCount :: Int
                    , actualCount   :: Int
                    }

              -- | For debugging purposes. Prints str
              | DebugError { str :: String}

              | UndefinedVariable 
                    { location   :: Location
                    , identifier :: String
                    }

              | UndefinedFunction
                    { location   :: Location
                    , identifier :: String
                    }

             -- | Multiple compiler errors that should all be printed
             | Aggregate
                    { errors :: [CompilerError] }

             | VariableAlreadyDefined
                { original  :: Ast.VariableDefinition
                , duplicate :: Ast.VariableDefinition
                }

            | FunctionAlreadyDefined
                { originalFunction  :: Ast.FunctionHeader
                , duplicateFunction :: Ast.FunctionHeader
                }

            -- | the main function is not exported
            | UnexportedMain
                { location :: Location }

            | MissingReturn
                { location         :: Location -- ^The location where a return was expected
                , functionName     :: String
                , functionLocation :: Ast.Location -- ^The location of the function that should have a return
                }

            -- | An integer is out of range
            | IntegerOutOfRange
                { location :: Location
                , value    :: Integer
                , bound    :: Integer
                }
            | Warning
                { error :: CompilerError
                , comment :: String
                }

type ErrorMonad = Either CompilerError

instance Show CompilerError where
    show (Aggregate {errors = e}) = intercalate "\n" $ map show e
    show (LexError {char = c, location = loc})
        = "Lexical error " ++ pos ++ l
                where 
                    pos = (show loc)
                    l = "\n" ++ positionHint (getLine loc) (position loc)

    show (CommentNotClosedError {location = loc})
        = "Comment block not closed before end of file!" ++ posNote ++ l
                where 
                    posNote = "\nNote: Comment block opened " ++ (show p) ++ " but never closed!"
                    l       = "\n" ++ positionHint (getLine loc) p
                    p       = position loc

    show (StringNotClosedError {location = loc})
        = "String not closed before end of file!" ++ posNote ++ l
                where 
                    posNote = "\nNote: String opened " ++ (show p) ++ " but never closed!"
                    l       = "\n" ++ positionHint (getLine loc) p
                    p       = position loc

    show (UnexpectedType
                    { location     = l
                    , expectedType = et
                    , actualType   = at
                    })
        = (printf "Unexpected type %s. Expected type %s but actual type is %s." 
                    (show l) 
                    (displayT et)
                    (displayT at)
        ) ++ lh
                where 
                    lh = "\n" ++ positionHint (getLine l) (position l)

    show (UnexpectedTypeOneOf
                    { location      = l
                    , expectedTypes = et
                    , actualType    = at
                    })
        = (printf "Unexpected type %s. Expected one of types: %s but actual type is %s." 
                    (show l)
                    (ets et)
                    (displayT at)
        ) ++ lh
                where 
                    lh     = "\n" ++ positionHint (getLine l) (position l)
                    ets et = intercalate ", " $ map displayT et


    show (DisallowedCast
                    { location  = l
                    , fromType  = ft
                    , toType    = tt
                    })
        = (printf "Disallowed cast %s. Casting from type %s to type %s, which is not allowed." 
                    (show l)
                    (displayT ft)
                    (displayT tt)
        ) ++ lh
                where 
                    lh = "\n" ++ positionHint (getLine l) (position l)

    show (UnknownType { location = l})
        = (printf "Typesystem encountered an unknown type %s." (show l)) ++ lh
                where 
                    lh = "\n" ++ positionHint (getLine l) (position l)

    show (NonSingletonAssignee
                    { location       = l
                    , assigneeType   = at
                    , assignmentType = bt
                    })
        = (printf "Disallowed type assignment %s. Assinging (%s) to non-singleton type (%s) is disallowed!" 
                    (show l)
                    (displayT at)
                    (displayT  bt)
        ) ++ lh
                where 
                    lh = "\n" ++ positionHint (getLine l) (position l)

    show (MismatchedParameterTypes
                    { location     = l
                    , expectedType = et
                    , actualType   = at
                    })
        = (printf "Mismatched parameter types %s. Calling a function with parametertypes %s but the function takes %s" 
                    (show l)
                    (displayParam at)
                    (displayParam et)
        ) ++ lh
                where 
                    lh = "\n" ++ positionHint (getLine l) (position l)

    show (DisallowedTypeCombination
                    { location = l
                    , types    = t
                    , combiner = c
                    })
        = (printf "Cannot combine types %s using %s %s" 
                    (tl t)
                    (show c)
                    (show l)
        ) 
        ++ lh
        ++ "\n"
        ++ nt t
                where 
                    nt :: [(Type, Location)] -> String
                    -- nt l = "Note: " ++ intercalate ", " $ map displayT $ map fst l
                    -- TODO
                    nt l      = "Note:\n" ++ (intercalate "\n" $ map dt l)
                    dt (t, l) = "Component " ++ show l ++ " has type " ++ displayT t ++ lhf l

                    tl :: [(Type, Location)] -> String
                    tl l   = intercalate ", " $ map displayT $ map fst l
                    lh     = "\n" ++ positionHint (getLine l) (position l)
                    lhf l' = "\n" ++ positionHint (getLine l') (position l')

    show (OrphanReturn { location = l})
        = printf "Oprhaned return %s" (show l) ++ lh
            where
                lh = "\n" ++ positionHint (getLine l) (position l)

    show (MismatchedArrayDimension
                        { arrayInitializer = i
                        , assumedType      = at
                        , mismatchedTypes  = t
                        })
        = (printf "Mismatched array dimensionality encountered in array initializer %s." (show l)) 
        ++ lh
        ++ nt t
            where 
                lh        = "\n" ++ positionHint (getLine l) (position $ i ^. Ast.location)
                l         = i ^. Ast.location
                nt tl     = "\nNote:\n" ++ (intercalate "\n" $ map dt tl)
                dt (t, l) = "Initializer " 
                            ++ show l 
                            ++ " has type " 
                            ++ displayT t 
                            ++ " but assumed type to be the same as the first initializer, which has the type "
                            ++ displayT at
                            ++ lhf l

                lhf l'    = "\n"
                            ++ positionHint (getLine l') (position l')

    show (AmbiguousArrayType
                    { location         = l
                    , assumedType      = t
                    , encounteredTypes = et
                    }) 
        = (printf "The type of the array %s is ambiguous. Assumed the type of the array was %s but also encountered types that differ in the initializer." 
                    (show l)
                    (displayT t)
        )
        ++ lh
        ++ nt et
            where 
                lh         = "\n" 
                            ++ positionHint (getLine l) (position $ l)

                nt tl      = "\nNote:\n" 
                            ++ (intercalate "\n" $ map dt tl)

                dt (t', l) = "Initializer " 
                            ++ show l 
                            ++ " has type " 
                            ++ displayT t'  
                            ++ " but assumed type to be the same as the first initializer, which has the type " 
                            ++ displayT t 
                            ++ lhf l

                lhf l'     = "\n" 
                            ++ positionHint (getLine l') (position l')

    show (NonDimensionalArrayType
                    { arrayInitializer   = i
                    , nonDimensionalType = t
                    }) 
        = (printf "Non-dimensional type encountered in array %s. An array is not allowed to consist of types without a dimensionality. Note: the type %s is non-dimensional." 
                    (show l)
                    (displayT t)
        )
        ++ lh
            where 
                lh = "\n" 
                    ++ positionHint (getLine l) (position $ i ^. Ast.location)
                l  = i ^. Ast.location

    show (IncorrectIndiceCount
                    { location      = l
                    , expectedCount = e
                    , actualCount   = a
                    }) 
        = (printf "Incorrect number of array indices %s. Expected %i indices but only %i are supplied!" 
                    (show l)
                    e
                    a
        ) ++ lh
            where 
                lh = "\n" ++ positionHint (getLine l) (position l)


    show (DebugError {str = s}) = s

    show (UndefinedVariable
                    { location   = l
                    , identifier = i
                    })
        = (printf "Undefined variable \"%s\" %s" 
                    i
                    (show l)
        ) ++ lh
            where 
                lh = "\n" ++ positionHint (getLine l) (position l)

    show (UndefinedFunction
                    { location   = l
                    , identifier = i
                    })
        = (printf "Undefined function \"%s\" %s" 
                    i
                    (show l)
        ) ++ lh
            where 
                lh = "\n" ++ positionHint (getLine l) (position l)

    show (VariableAlreadyDefined
                    { original  = o
                    , duplicate = d
                    })
        = (printf "Variable %s (\"%s\") already defined!" 
                    (show l1)
                    (dv o)
        )
        ++ lh l1
        ++ nt
            where 
                lh l = "\n" 
                    ++ positionHint (getLine l) (position l)

                lh2  = "\n"
                    ++ positionHint (getLine l2) (position l2)

                l1   = o ^. Ast.location

                l2   = d ^. Ast.location

                nt   = printf "\nNote: Already defined %s: %s" 
                                    (show l2)
                                    (lh l2)

                dv v = Ast.variableName v

    show (FunctionAlreadyDefined
                    { originalFunction = o
                    , duplicateFunction = d
                    })
        = (printf "Function %s (\"%s\") already defined!" (show l1) (dv o)) ++ lh l1 ++ nt
            where 
                lh l = "\n" 
                    ++ positionHint (getLine l) (position l)

                lh2  = "\n"
                    ++ positionHint (getLine l2) (position l2)
                l1   = o ^. Ast.location
                l2   = d ^. Ast.location
                nt   = printf "\nNote: Already defined %s: %s" 
                                    (show l2)
                                    (lh l2)
                dv v = Ast.funcName v

    show (UnexportedMain { location = l })
        = (printf "main (%s) is not exported!" (show l)) ++ lh l
            where 
                lh l = "\n" 
                    ++ positionHint (getLine l) (position l)

    show (MissingReturn { location = l, functionLocation = fl, functionName = n})
        = (printf "The last statement of '%s' %s must be a return statement!" 
          n (show fl)) ++ lh l
            where 
                lh l = "\n" 
                    ++ positionHint (getLine l) (position l)

                fname :: Ast.Declaration -> String
                fname f = Ast.funcName $ Ast.header f
    show (IntegerOutOfRange { location = l , value = v , bound = b })
        = (printf "Integer with value %i out of range! The bound is %i." v b) 
          ++ lh l
          where
                lh l = "\n" 
                    ++ positionHint (getLine l) (position l)
            
    show (Warning {error = e, comment = c})
        = show e ++ "\n" ++ c

displayMarker :: Int -> String
displayMarker c = (replicate (c) ' ') ++ "^"

displayMarker' :: Int -> String
displayMarker' c = (replicate (c) '~') ++ "^"

displayMarkers :: [Int] -> String
displayMarkers = displayMarkers' "" 0

displayMarkerRange :: (Int, Int) -> String
displayMarkerRange (a, b) 
            | a > b     = displayMarkerRange (b, a)
            | b > a     = displayMarker a ++ displayMarker' (b - a - 1)
            | otherwise = ""

displayMarkers' :: String -> Int -> [Int] -> String
displayMarkers' p l (m : []) = p ++ displayMarker (m - l)
displayMarkers' p l (m : t) 
                    | m == l    = displayMarkers' (p ++ "^") (l + 1) t
                    | otherwise = displayMarkers' (p ++ " ") (l + 1) (m : t)



positionHint :: (Int -> String) -> Position ->  String
positionHint getLine (At l c) = 
           (getLine l) ++ "\n" ++ displayMarker (c - 1)

positionHint getLine (Before l c) = positionHint getLine $ At l c
positionHint getLine (Near l c)   = positionHint getLine $ At l c

positionHint getLine (Between l1 c1 l2 c2)
                        | l1 == l2  = (getLine l1) ++ "\n" ++ displayMarkerRange (c1 - 1, c2 - 1) 
                        | otherwise = (positionHint getLine $ At l1 c1) ++ "\n" ++  (positionHint getLine $ At l2 c2)


displayParam :: Type -> String
displayParam l = "(" ++ intercalate ", " (map displayTE $ reverse l) ++ ")"


displayT :: Type -> String
displayT t = intercalate " -> " (map displayTE t)

displayTE :: TypeElem -> String
displayTE (Int n)        = "int" ++ displayDimension n
displayTE (Float n)      = "float" ++ displayDimension n
displayTE (Bool n)       = "bool" ++ displayDimension n
displayTE (Void)         = "void"
displayTE (TypeClass []) = "ErrorType"
displayTE (TypeClass e)  = "Typeclass of [" ++ intercalate ", " (map displayTE e) ++ "]"
displayTE (NestedType e) = "(" ++ intercalate " -> " (map displayTE $ reverse e) ++ ")"

displayDimension :: Int -> String
displayDimension 0 = ""
displayDimension n = ":" ++ show n
