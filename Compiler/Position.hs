{-# LANGUAGE DeriveDataTypeable #-}
module Compiler.Position (
    Position(..),
    Location(..),
    defaultLocation
) where
import Data.Generics
import Text.Printf

data Position 
        -- | At an exact position
        = At
            { line   :: Int
            , column :: Int
            }
        -- | Somewhere near a position but not sure where
        | Near
            { line    :: Int
            , column  :: Int
            }
        -- | Before a certain position
        | Before
            { line    :: Int
            , column  :: Int
            }
        -- | Between two positions
        | Between { lline   :: Int
                  , lcolumn :: Int
                  , rline   :: Int
                  , rcolumn :: Int
                  }
        deriving (Eq, Data, Typeable)

instance Show Position where
    show (At l c)              = printf "at %i:%i" l c
    show (Before l c)          = printf "before %i:%i" l c
    show (Near l c)            = printf "near %i:%i" l c
    show (Between l1 c1 l2 c2) = printf "between %i:%i and %i:%i" l1 c1 l2 c2


-- | A location is a position in a file combined with the filename. Also
-- provides a getLine to easily print lines from the file the location
-- points to (very handy when reporting errors)
data Location = Location
                 { position :: Position
                 , file     :: String
                 , getLine  :: (Int -> String)
                 } deriving (Data, Typeable)

instance Eq Location where
        a1 == a2 = position a1 == position a2 && file a1 == file a2

instance Show Location where
        show (Location { position = p, file = f}) = show p ++ " (in: " ++ f ++ ")"

defaultLocation :: Location
defaultLocation = Location (At (-1) (-1)) "" (\_ -> "")
