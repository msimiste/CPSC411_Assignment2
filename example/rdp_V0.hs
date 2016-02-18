module Main  where
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

factor -> NUM.                     factor ((NUM n):ts) = 


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
--exp1::[Lexeme] -> Either String [Lexeme]
--exp1 ts = more_exp (term ts)

--prog -> stmt.
{-stmt -> IF expr thenpart elsepart
     | WHILE expr dopart
     | INPUT ID
     | ID ASSIGN expr
     | WRITE expr
     | BEGIN stmtlist endpart.-}

stmt :: [Lexeme] -> Either String [Lexeme]
stmt ((IF n):ts) = elsepart(thenpart (expr ts))
stmt (WHILE:ts) = dopart(expr ts)
stmt (INPUT:ts) = stmt ts
stmt (ID:ASSIGN:ts) = stmt ts
stmt (WRITE:ts) = expr ts
stmt (BEGIN:ts) = endpart(stmtlist ts)

stmtlist :: [Lexeme] -> Either String [Lexeme]
stmtlist ts = stmtlist' ts

stmtlist' :: [Lexeme] -> Either String [Lexeme]
stmtlist' ((IF n0:ts) = semipart(elsepart(thenpart(expr ts)))
stmtlist' (WHILE:ts) = semipart(dopart ts)
stmtlist' (ID:ASSIGN:ts) = semipart(expr ts)
stmtlist' (WRITE:ts) =  semipart(expr ts)
stmtlist' (BEGIN:ts) = semipart(endpart(stmtlist ts))
stmtlist' ts = ts

semipart :: [Lexeme] -> Either String [Lexeme] 
semipart ((SEMICOLON _):ts) = stmtlist' ts

expr :: [Lexeme] -> Either String [Lexeme]
expr ts = term (expr' ts)

expr' :: [Lexeme] -> Either String [Lexeme]
expr' ((ADD _):ts) = term (expr' ts)
expr' ((SUB n):ts) = term (expr' ts)
expr' ts = term (expr' ts)


thenpart :: [Lexeme] -> Either String [Lexeme]
thenpart ((THEN n):ts) = stmt ts

elsepart :: [Lexeme] -> Either String [Lexeme]
elsepart (ELSE:ts) = stmt ts


dopart :: [Lexeme] -> Either String [Lexeme]
dopart ((DO n):ts) = stmt ts


endpart ::[Lexeme] -> Either String [Lexeme]
endpart (END:ts) = []

term :: [Lexeme] -> Either String [Lexeme]
term ts = term'(factor ts)

term' :: [Lexeme] -> Either String [Lexeme]
term' ((MUL n):ts) = term' (factor ts)
term' ((DIV n):ts) = term' (factor ts)
term' ts = term'(factor ts)

factor :: [Lexeme] -> Either String [Lexeme]
factor (LPAR:ts) = rightbrac (expr ts)
factor (ID:ts) = ts
factor ((NUM n):ts) = ts
factor ((SUB n):(NUM m):ts) = ts



rightbrac :: [Lexeme] -> Either String [Lexeme]
rightbrac (RPAR:ts) = ts




{-
more_exp ::[Lexeme] -> Either String [Lexeme]
more_exp (ADD:ts) = more_exp (term ts)
more_exp ts = ts

term :: [Lexeme] -> Either String [Lexeme]
term ts = more_term (factor ts)

more_term :: [Lexeme] -> Either String [Lexeme]
more_term (MUL:ts) = more_term (factor ts)
more_term ts = ts

factor :: [Lexeme] -> Either String [Lexeme]
factor ((NUM n):ts) = ts
-}

main = do
  l <- mlex
  let parse      = stmt l
      succParse  = if null parse then True else False
  if succParse then do
     putStrLn  "Parsing Successful"
   else do
     putStrLn "Parsing Failed"
     
     
    
