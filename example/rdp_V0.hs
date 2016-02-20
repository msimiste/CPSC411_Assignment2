module Main  where
import MLexer
import Data.Either

                                
data Stmt = If Exp Stmt Stmt                        
			| While Exp Stmt 
			| Assign String Exp
			| Block[Stmt]
			| Write Exp
			| Input Exp
			deriving (Eq, Show, Read)
			
data Exp = Add Exp Exp   
		   | Mul Exp Exp
		   | Div Exp Exp 
		   | Sub Exp
		   | Id String
		   | Num Int
		   deriving (Eq, Show, Read)
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
     
prog :: [Lexeme] -> Either String [Lexeme]
prog ts = stmt ts

stmt :: [Lexeme] -> Either String ([Lexeme], Stmt)
--stmt ((IF n):ts) = elsepart(thenpart (expr ts)) 
stmt ((IF n):ts) = do
                    (L, E)<- expr ts
                    (L1, S1) <- thenpart L
                    (L2, S2) <- elsepart L1
                    Right (L2, If E S1 S2)
                    
--stmt ((WHILE n):ts) = dopart (expr ts0
stmt ((WHILE n):ts) = do 
                      (L, E) <- expr ts
                      (L1, S) <- dopart L
                      Right (L1, While E S)
                       
stmt ((INPUT i):(ID m n):ts) = Right (ts, Input (Id m))
stmt ((ID m n):(ASSIGN p):ts) = do
                                (L, E) <- expr ts
                                Right(L, Assign m E)
stmt ((WRITE m):ts) = do
					(L, E) <- expr ts
					Right(L, Write E)

--stmt ((BEGIN m):ts) = endpart(stmtlist ts)
stmt ((BEGIN m):ts) =  do
                       (L, S) <- stmtlist ts
                       L1 <- endpart L
                       Right (L1, Block S)
stmt ts = errors ts "stmt"


stmtlist :: [Lexeme] -> Either String ([Lexeme], [Stmt])
stmtlist ts = do
				(L, S) <- stmtlist' [] ts
				Right (L, S)

stmtlist' :: [Stmt] ->[Lexeme] -> Either String ([Lexeme], [Stmt])
--stmtlist' ((IF n):ts) = semipart(elsepart(thenpart(expr ts)))
stmtlist' S ((IF n):ts) = do
                        (L, E)<- expr ts
						(L1, S1) <- thenpart L
						(L2, S2) <- elsepart L1
                        L3 <- semipart L2
                        Right (L3, [If E S1 S2]++S)
                         
--stmtlist' ((WHILE n):ts) = semipart(dopart ts)
stmtlist' S ((WHILE n):ts) = do
                           (L, E) <- expr ts
                           (L1, S1) <- dopart L
                           L2 <- semipart L1
                           Right (L2, [While E S1]++S)
                           
stmtlist' S ((INPUT i):(ID m n):ts) = do
									L <- semipart ts
									Right (L, [Input (Id m)]++S)
	 
                                    
--stmtlist' ((ID m n):(ASSIGN o p):ts) = semipart(expr ts)
stmtlist' S ((ID m n):(ASSIGN p):ts) = do
                                       (L, E) <- expr ts
                                       L1 <- semipart L
                                       Right (L1, [Assign m E]++S)
--stmtlist' ((WRITE m):ts) =  semipart(expr ts)
stmtlist' S ((WRITE m):ts) = do
                           (L, E) <- expr ts
                           L1 <-  semipart L
                           Right (L1, [Write E]++S)
                           
--stmtlist' ((BEGIN m):ts) = semipart(endpart(stmtlist ts))
stmtlist' S ((BEGIN m):ts) = do
                           (L, S1) <- stmtlist ts
                           L1 <- endpart L
                           L2 <- semipart L1
                           Right (L2, [Block S1]++S)
                           
stmtlist' S ts = Right (ts, S)

semipart :: [Lexeme] -> Either String ([Lexeme], [Stmt]) 
semipart ((SEMICOLON n):ts) = stmtlist' ts
semipart ts = errors ts "semipart"

expr ::  [Lexeme] -> Either String ([Lexeme], Exp)
--expr ts = expr' (term ts)
expr ts = do 
          (L, E) <- term ts
           expr' E L
          
          
expr' :: Exp -> [Lexeme] -> Either String ([Lexeme], Exp)
--expr' ((ADD m n):ts) = expr' (term ts)
expr' e ((ADD n):ts) = do 
                       (L, E) <- term ts
                       (L1, E1) <- expr' E L
                       Right (L1, Add e E1)
                       
--expr' ((SUB m n):ts) = expr' (term ts)
expr' e ((SUB n):ts) = do 
                       (L, E) <- term ts
                       (L1, E1) <- expr' E L
                        Right (L1, Add e (Sub E1))
--expr' ts = expr' (term ts)
--Should this go to errors ie, expr' ts = errors ts "espr'"
expr' ts = Right (ts, e)

thenpart :: [Lexeme] -> Either String ([Lexeme], Stmt)
thenpart ((THEN n):ts) =  stmt ts



elsepart :: [Lexeme] -> Either String ([Lexeme], Stmt)
elsepart ((ELSE m):ts) = stmt ts



dopart :: [Lexeme] -> Either String ([Lexeme], Stmt)
dopart ((DO n):ts) =  stmt ts



endpart ::[Lexeme] -> Either String [Lexeme]
endpart ((END m):ts) = Right ts
endpart ts = errors ts "endpart"

term :: [Lexeme] -> Either String ([Lexeme], Exp)
--term ts = term'(factor ts)
term ts = do 
          (L, E) <- factor ts
          term' E L
          
term' :: Exp -> [Lexeme] -> Either String ([Lexeme], Exp)
--term' ((MUL m n):ts) = term' (factor ts)
term' e ((MUL n):ts) = do 
                       (L, E) <- factor ts
                       (L1, E1) <- term' E L
                       Right (L1, Mul e E1)
--term' ((DIV m n):ts) = term' (factor ts)
term' e ((DIV n):ts) = do 
                       (L, E) <- factor ts
                       (L1, E1) <- term' E L
                       Right (L1, Div e E1)
                       
--term' ts = term'(factor ts)
term' e ts = Right (ts, e)
          
factor :: [Lexeme] -> Either String ([Lexeme], Exp)
--factor ((LPAR m n):ts) = rightbrac (expr ts)
factor ((LPAR n):ts) = do
                       (L, E) <- expr ts
                        T <- rightbrac L 
                        Right (T, E)                       
factor ((ID m pos):ts) = Right (ts, Id m)
factor ((NUM m pos):ts) = Right (ts, Num m)
factor ((SUB n):(NUM o pos):ts) = Right (ts, Sub (Num o))
factor ts = errors ts "factor"




rightbrac :: [Lexeme] -> Either String ([Lexeme])
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
      let parse = prog ts
      case parse of 
         Right ([], AST) -> putStrLn "Parsing Successful" 
         Left str -> do putStrLn str 
    Left tsp -> do putStrLn tsp

