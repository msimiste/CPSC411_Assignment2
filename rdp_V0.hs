module RDP (main)  where
import MLexer

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


prog -> stmt.
stmt -> IF expr thenpart elsepart
     | WHILE expr dopart
     | INPUT ID
     | ID ASSIGN expr
     | WRITE expr
     | BEGIN stmtlist endpart.
   
thenpart -> THEN stmt.
elsepart -> ELSE stmt.
dopart ->   DO stmt.
endpart -> END.

     
stmtlist -> stmtlist'.
stmtlist' -> IF expr thenpart elsepart semipart 
	 | WHILE expr dopart semipart 
     | INPUT ID semipart 
     | ID ASSIGN expr semipart 
     | WRITE expr semipart 
     | BEGIN stmtlist endpart semipart 
     |.
     
semipart -> SEMICOLON stmtlist'.

expr  ->  term expr'.

expr' -> addop term expr'
     |.
     
addop -> ADD
     |   SUB.
     
term  ->  factor term'.

term' -> mulop factor term'
     |.
     
mulop -> MUL
     |   DIV.
     
factor -> LPAR expr rightbrac
     |    ID 
     |    NUM
     |    SUB NUM.

rightbrac -> RPAR.

-}
exp1::[Tokens] -> [Tokens]
exp1 ts = more_exp (term ts)

more_exp ::[Tokens] -> [Tokens]
more_exp (ADD:ts) = more_exp (term ts)
more_exp ts = ts

term :: [Tokens] -> [Tokens]
term ts = more_term (factor ts)

more_term :: [Tokens] -> [Tokens]
more_term (MUL:ts) = more_term (factor ts)
more_term ts = ts

factor :: [Tokens] -> [Tokens]
factor ((NUM n):ts) = ts

main = do
  l <- mlex
  let parse      = exp1 l
      succParse  = if null parse then True else False
  if succParse then do
     putStrLn  "Parsing Successful"
   else do
     putStrLn "Parsing Failed"
     
     
    
