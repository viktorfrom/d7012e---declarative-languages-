module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement

-- Type T should have 7 constructors
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Skip |
    Begin [Statement] |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

-- Remove "if", "then", "else". Save (expr) # (stmt) # (stmt) within if-stmt
if_stmt = (accept "if" -# Expr.parse #- require "then") # (parse #- require "else") # parse >-> buildIf
buildIf ((expr, stmt1), stmt2) = If expr stmt1 stmt2


skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> Begin

-- Remove "while" and "do". Save/parse expr and stmt.
while = (accept "while" -# Expr.parse #- require "do") # parse >-> buildWhile
buildWhile (expr, stmt) = While expr stmt


read = accept "read" -# word #- require ";" >-> Read
write = accept "write" -# Expr.parse #- require ";" >-> Write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
