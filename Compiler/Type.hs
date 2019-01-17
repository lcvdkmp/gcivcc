module Compiler.Type (
    typeOf,
    uOpTypeOf,
    binOpTypeOf,
    Type(..),
    TypeElem(..),
    HasType,
    TypeCombiner(..),
    changeDimension,
    changeElemDimension,
    t',
    dimension
) where

import Compiler.Type.HasType
import Compiler.Type.Type
