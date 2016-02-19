module Main  where
import MLexer
import Data.Either

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
   


     
stmtlist -> stmtlist'.
stmtlist' -> IF expr thenpart elsepart semipart 
	 | WHILE expr dopart semipart 
     | INPUT ID semipart 
     | ID ASSIGN expr semipart 
     | WRITE expr semipart 
     | BEGIN stmtlist endpart semipart 
     |.
     
thenpart -> THEN stmt.
elsepart -> ELSE stmt.
dopart ->   DO stmt.
endpart -> END.

semipart -> SEMICOLON stmtlist'.

expr  ->  term expr'.

expr' -> ADD term expr'
     | SUB term expr'     
     |.

     
term  ->  factor term'.

term' -> MUL factor term'
	 | DIV factor term'	
     |.


     
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
--stmt ((IF n):ts) = elsepart(thenpart (expr ts)) 
stmt ((IF n):ts) = do
                    val1 <- expr ts
                    val2 <- thenpart val1
                    elsepart val2
--stmt ((WHILE n):ts) = dopart (expr ts0
stmt ((WHILE n):ts) = do 
                      val1 <- expr ts
                      dopart val1
                       
stmt ((INPUT i):(ID m n):ts) = Right ts
stmt ((ID m n):(ASSIGN p):ts) = expr ts
stmt ((WRITE m):ts) = expr ts

--stmt ((BEGIN m):ts) = endpart(stmtlist ts)
stmt ((BEGIN m):ts) =  do
                       val1 <- stmtlist ts
                       endpart val1
stmt ts = errors ts "stmt"


stmtlist :: [Lexeme] -> Either String [Lexeme]
stmtlist ts = stmtlist' ts

stmtlist' :: [Lexeme] -> Either String [Lexeme]
--stmtlist' ((IF n):ts) = semipart(elsepart(thenpart(expr ts)))
stmtlist' ((IF n):ts) = do
                        val1 <- expr ts
                        val2 <- thenpart val1
                        val3 <- elsepart val2
                        semipart val3
--stmtlist' ((WHILE n):ts) = semipart(dopart ts)
stmtlist' ((WHILE n):ts) = do
                           val1 <- expr ts
                           val2 <- dopart val1
                           semipart val2
                           
stmtlist' ((INPUT i):(ID m n):ts) = semipart ts
	 
                                    
--stmtlist' ((ID m n):(ASSIGN o p):ts) = semipart(expr ts)
stmtlist' ((ID m n):(ASSIGN p):ts) = do
                                       val1 <- expr ts
                                       semipart val1
--stmtlist' ((WRITE m):ts) =  semipart(expr ts)
stmtlist' ((WRITE m):ts) = do
                           val1 <- expr ts
                           semipart val1
--stmtlist' ((BEGIN m):ts) = semipart(endpart(stmtlist ts))
stmtlist' ((BEGIN m):ts) = do
                           val1 <- stmtlist ts
                           val2 <- endpart val1
                           semipart val2
stmtlist' ts = Right ts

semipart :: [Lexeme] -> Either String [Lexeme] 
semipart ((SEMICOLON n):ts) = stmtlist' ts

expr :: [Lexeme] -> Either String [Lexeme]
--expr ts = expr' (term ts)
expr ts = do 
          val1 <- term ts
          expr' val1
          
expr' :: [Lexeme] -> Either String [Lexeme]
--expr' ((ADD m n):ts) = expr' (term ts)
expr' ((ADD n):ts) = do 
                       val1 <- term ts
                       expr' val1
                       
--expr' ((SUB m n):ts) = expr' (term ts)
expr' ((SUB n):ts) = do 
                       val1 <- term ts
                       expr' val1
--expr' ts = expr' (term ts)
expr' ts = Right ts

thenpart :: [Lexeme] -> Either String [Lexeme]
thenpart ((THEN n):ts) = stmt ts

elsepart :: [Lexeme] -> Either String [Lexeme]
elsepart ((ELSE m):ts) = stmt ts


dopart :: [Lexeme] -> Either String [Lexeme]
dopart ((DO n):ts) = stmt ts


endpart ::[Lexeme] -> Either String [Lexeme]
endpart ((END m):ts) = Right ts
endpart ts = errors ts "endpart"

term :: [Lexeme] -> Either String [Lexeme]
--term ts = term'(factor ts)
term ts = do 
          val1 <- factor ts
          term' val1
term' :: [Lexeme] -> Either String [Lexeme]
--term' ((MUL m n):ts) = term' (factor ts)
term' ((MUL n):ts) = do 
                       val1 <- factor ts
                       term' val1
--term' ((DIV m n):ts) = term' (factor ts)
term' ((DIV n):ts) = do 
                       val1 <- factor ts
                       term' val1
                       
--term' ts = term'(factor ts)
term' ts = Right ts
          
factor :: [Lexeme] -> Either String [Lexeme]
--factor ((LPAR m n):ts) = rightbrac (expr ts)
factor ((LPAR n):ts) = do
                       val1 <- expr ts
                       rightbrac val1                         
factor ((ID m n):ts) = Right ts
factor ((NUM _ m):ts) = Right ts
factor ((SUB n):(NUM o p):ts) = Right ts




rightbrac :: [Lexeme] -> Either String [Lexeme]
rightbrac ((RPAR n):ts) = Right ts


errors :: [Lexeme]-> String -> Either String [Lexeme]
errors toks y = Left $ "Error: " ++ y ++" : Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 


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


{-main = do
  l <- mlex
  case l of 
    Right ts -> do
      let parse      = stmt ts
      let succParse  = if null parse then True else False
      if succParse then do
        putStrLn  "Parsing Successful"
      else do
        putStrLn "Parsing Failed"
    Left tsp -> do
     putStrLn  tsp 
-}

main = do
  l <- mlex
  case l of
    Right ts -> do
      let parse = stmt ts
      case parse of 
         Right [] -> putStrLn "Parsing Successful" 
         Left str -> do putStrLn str

