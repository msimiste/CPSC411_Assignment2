module Main where
import MLexer

data Expr =  EAdd Expr Expr 
           | EMul Expr Expr
           | ENum Int 
           deriving (Eq,Show,Read)

{-     
Grammar                            Haskell Code
=======                            ============
exp -> term more_exp               exp ts = more_exp (term ts)

more_exp -> + term more_exp        more_exp (ADD:ts) = more_exp (term ts)
          |.                       more_exp ts = ts

term -> factor more_term           term ts = more_term (factor ts)

more_term -> * factor more_term    more_term (MUL:ts) = more_term (factor ts)
           |.                      more_term ts = ts

factor -> NUM.                     factor ((NUM n):ts) = ts
-}
exp1:: [Tokens] -> Either String ([Tokens],Expr)
exp1  ts = do
    (rem,e) <- term ts  
    more_exp e rem 

---------------------------------------------

more_exp :: Expr -> [Tokens] ->  Either String ([Tokens],Expr)
more_exp e (ADD:ts)  = do 
        (rem,e') <- term ts
        more_exp (EAdd e e') rem 
more_exp e ts        = Right (ts,e) 
----------------------------------------------

term :: [Tokens] -> Either String ([Tokens],Expr)
term  ts           = do
       (rem,e) <- factor ts
       more_term e rem 

-----------------------------------------------

more_term :: Expr -> [Tokens] -> Either String ([Tokens],Expr)
more_term e (MUL:ts) = do 
    (rem,e') <- factor ts
    more_term (EMul e e') rem 
more_term e ts       = Right (ts,e)

-----------------------------------------------

factor :: [Tokens] ->  Either String ([Tokens],Expr)
factor ((NUM n):ts)  = Right (ts,ENum n)
factor toks          = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 


main = do
  tokList <- mlex
  let parseRes = exp1 tokList
  case parseRes of
    Left str -> putStrLn str 
    Right ([],ast) -> do
       putStrLn "Parse Successful.\n"
       putStrLn $ "AST is :\n" ++ show ast 

     
    
