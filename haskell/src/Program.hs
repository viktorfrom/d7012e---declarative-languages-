-- Code to Haskell lab assignment 3 in the course D7012E by Viktor From

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving (Show) -- Import type T (7 constructors)
instance Parse T where
  -- Iterate through the program stmts
  parse = (iter Statement.parse) >-> Program
  toString = toStringFunc

-- Convert program stmts into strings
toStringFunc :: T -> String
toStringFunc (Program (x:xs)) = Statement.toString x ++ toString (Program xs)

-- Takes a program containing stmt xs, int xs and evaluates the input
exec (Program xs) int_xs = Statement.exec xs Dictionary.empty int_xs
