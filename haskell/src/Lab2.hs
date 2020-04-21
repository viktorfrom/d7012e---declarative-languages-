-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

module Lab2
    ( result,
      --mkfun
    ) where

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App func e1) = func ++ "(" ++ unparse e1 ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" expr) env = sin (eval expr env) 
eval (App "cos" expr) env = cos (eval expr env) 
eval (App "log" expr) env = log (eval expr env) 
eval (App "exp" expr) env = exp (eval expr env) 

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "sin" e1) = Op "*" (diff v e1) (App "cos" (e1))
diff v (App "cos" e1) = Op "*" (diff v e1) (App "sin" (e1))
diff v (App "exp" e1) = Op "*" (diff v e1) (App "exp" (e1))
diff v (App "log" e1) = Op "/" (diff v e1) (e1)
diff _ _ = error "can not compute the derivative"



simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App fn e1) = App fn e1
   
-- result :: EXPR
-- result = (parse "sin(2*x)") --test

-- result :: Float
-- result = eval (parse "sin(10+5)") [("", 0)] --test
-- result = eval (parse "cos(10+5)") [("", 0)] --test
-- result = eval (parse "sin(x)")   [("x", 0)] --test
-- result = eval (parse "10+5") [("", 0)] --test
-- result = eval (parse "log(10)") [("", 0)] --test

-- part 2
-- result :: String
-- result = unparse(simplify (diff (Var "x") (parse "exp(sin(2*x))"))) 

-- part 3
-- mkfun :: (EXPR, EXPR) -> (Float -> Float)
-- mkfun (expr, var) x = eval expr [((unparse var), x)]

-- result :: Float
-- result = mkfun (parse "x*x+2", Var "x") 3 

-- part 4
findzero ::  String -> String -> Float -> Float
findzero s1 s2 x0 = x0 - eval (parse (s2 ++ "/" ++ s2_prim s1 s2)) [((s1), x0)]

s2_prim :: String -> String -> String 
s2_prim var expr = unparse(simplify (diff (Var var) (parse expr))) 

result :: Float
-- result = ("x*x*x+x-1" ++ "/" ++ fprim "x" "x*x*x+x-1")
result = findzero "x" "x*x*x+x-1" 1.0 --0.68232775
-- result = findzero "y" "cos(y)*sin(y)" 2.0 --1.5707964