module Compiler.Utils (
    foldrMr,
) where

import Data.Foldable (foldrM)

-- | The same as foldrM but with the 't a' and 'b' reversed
foldrMr :: (Foldable t, Monad m) => (a -> b -> m b) -> t a -> b -> m b
foldrMr f z x  = foldrM f x z
