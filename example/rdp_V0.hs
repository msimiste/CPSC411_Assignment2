module Main  where
import MLexer
import Data.Either
import Data.List
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

                                
data Stmt a = If (Exp a) (Stmt a) (Stmt a)                        
			| While (Exp a) (Stmt a) 
			| Assign String (Exp a)
			| Block [Stmt a]
			| Write (Exp a)
			| Input (Exp a)
			deriving (Eq, Show, Read)
			
data Exp a = Add (Exp a)  (Exp a)   
		   | Mul (Exp a) (Exp a)
		   | Div (Exp a) (Exp a)
		   | Sub (Exp a)
		   | Id String
		   | Num Int
		   deriving (Eq, Show, Read)
{-     
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
     
prog :: [Lexeme] -> Either String ([Lexeme], Stmt String)
prog ts = stmt ts

stmt :: [Lexeme] -> Either String ([Lexeme], Stmt String)
--stmt ((IF n):ts) = elsepart(thenpart (expr ts)) 
stmt ((IF n):ts) = do
                    (l, e)<- expr ts
                    (l1, s1) <- thenpart l
                    (l2, s2) <- elsepart l1
                    Right (l2, If e s1 s2)
                    
--stmt ((WHILE n):ts) = dopart (expr ts0
stmt ((WHILE n):ts) = do 
                      (l, e) <- expr ts
                      (l1, s) <- dopart l
                      Right (l1, While e s)
                       
stmt ((INPUT i):(ID m n):ts) = Right (ts, Input (Id m))
stmt ((ID m n):(ASSIGN p):ts) = do
                                (l, e) <- expr ts
                                Right(l, Assign m e)
stmt ((WRITE m):ts) = do
					(l, e) <- expr ts
					Right(l, Write e)

--stmt ((BEGIN m):ts) = endpart(stmtlist ts)
stmt ((BEGIN m):ts) =  do
                       (l, s) <- stmtlist ts
                       l1 <- endpart l
                       Right (l1, Block s)
stmt ts = errors1 ts "stmt"


stmtlist :: [Lexeme] -> Either String ([Lexeme], [Stmt String])
stmtlist ts = do
				(l, s) <- stmtlist' [] ts
				Right (l, s)

stmtlist' :: [Stmt String] ->[Lexeme] -> Either String ([Lexeme], [Stmt String])
--stmtlist' ((IF n):ts) = semipart(elsepart(thenpart(expr ts)))
stmtlist' s ((IF n):ts) = do
                        (l, e) <- expr ts
                        (l1, s1) <- thenpart l
                        (l2, s2) <- elsepart l1
                        (l3, s3)  <- semipart l2 s
                        Right (l3, [If e s1 s2]++s3)
                         
--stmtlist' ((WHILE n):ts) = semipart(dopart ts)
stmtlist' s ((WHILE n):ts) = do
                           (l, e) <- expr ts
                           (l1, s1) <- dopart l
                           (l2, s2) <- semipart l1 s
                           Right (l2, [While e s1]++s2)
                           
stmtlist' s ((INPUT i):(ID m n):ts) = do
									(l, s1) <- semipart ts s
									Right (l, [Input (Id m)]++s1)
	 
                                    
--stmtlist' ((ID m n):(ASSIGN o p):ts) = semipart(expr ts)
stmtlist' s ((ID m n):(ASSIGN p):ts) = do
                                       (l, e) <- expr ts
                                       (l1, s1) <- semipart l s
                                       Right (l1, [Assign m e]++s1)
--stmtlist' ((WRITE m):ts) =  semipart(expr ts)
stmtlist' s ((WRITE m):ts) = do
                           (l, e) <- expr ts
                           (l1, s1) <- semipart l s
                           Right (l1, [Write e]++s1)
                           
--stmtlist' ((BEGIN m):ts) = semipart(endpart(stmtlist ts))
stmtlist' s ((BEGIN m):ts) = do
                           (l, s1) <- stmtlist ts
                           l1 <- endpart l
                           (l2, s2) <- semipart l1 s1
                           Right (l2, [Block s1]++s2)
                           
stmtlist' s ts = Right (ts, s)

semipart :: [Lexeme] -> [Stmt String] -> Either String ([Lexeme], [Stmt String])
semipart ((SEMICOLON n):ts) s  = stmtlist' s ts
semipart ts s  = errors ts "semipart"

expr ::  [Lexeme] -> Either String ([Lexeme], Exp String)
--expr ts = expr' (term ts)
expr ts = do
          (l, e) <- term ts
          expr' e l
          
          
expr' :: (Exp String) -> [Lexeme] -> Either String ([Lexeme], Exp String)
--expr' ((ADD m n):ts) = expr' (term ts)
expr' e ((ADD n):ts) = do 
                       (l, e1) <- term ts
                       (l1, e2) <- expr' e1 l
                       Right (l1, Add e e2)
                       
--expr' ((SUB m n):ts) = expr' (term ts)
expr' e ((SUB n):ts) = do 
                       (l, e1) <- term ts
                       (l1, e2) <- expr' e1 l
                       Right (l1, Add e (Sub e2))
--expr' ts = expr' (term ts)
--Should this go to errors ie, expr' ts = errors ts "espr'"
expr' e ts = Right (ts, e)

thenpart :: [Lexeme] -> Either String ([Lexeme], Stmt String)
thenpart ((THEN n):ts) =  stmt ts



elsepart :: [Lexeme] -> Either String ([Lexeme], Stmt String)
elsepart ((ELSE m):ts) = stmt ts



dopart :: [Lexeme] -> Either String ([Lexeme], Stmt String)
dopart ((DO n):ts) =  stmt ts



endpart ::[Lexeme] -> Either String [Lexeme]
endpart ((END m):ts) = Right ts
endpart ts = Left $ "Error: " ++ "endpart" ++" : Couldn't parse\n" ++ show ts
                              ++ "\nExpecting a number got " ++ show (head ts) 


term :: [Lexeme] -> Either String ([Lexeme], Exp String)
--term ts = term'(factor ts)
term ts = do 
          (l, e) <- factor ts
          term' e l
          
term' :: (Exp String) -> [Lexeme] -> Either String ([Lexeme], Exp String)
--term' ((MUL m n):ts) = term' (factor ts)
term' e ((MUL n):ts) = do 
                       (l, e1) <- factor ts
                       (l1, e2) <- term' e1 l
                       Right (l1, Mul e e2)
--term' ((DIV m n):ts) = term' (factor ts)
term' e ((DIV n):ts) = do 
                       (l, e1) <- factor ts
                       (l1, e2) <- term' e1 l
                       Right (l1, Div e e2)
                       
--term' ts = term'(factor ts)
term' e ts = Right (ts, e)
          
factor :: [Lexeme] -> Either String ([Lexeme], Exp String)
--factor ((LPAR m n):ts) = rightbrac (expr ts)
factor ((LPAR n):ts) = do
                       (l, e) <- expr ts
                       t <- rightbrac l
                       Right (t, e)                       
factor ((ID m pos):ts) = Right (ts, Id m)
factor ((NUM m pos):ts) = Right (ts, Num m)
factor ((SUB n):(NUM o pos):ts) = Right (ts, Sub (Num o))
factor ts = Left $ "Error: " ++ "factor" ++" : Couldn't parse\n" ++ show ts
                              ++ "\nExp Stringecting a number got " ++ show (head ts) 




rightbrac :: [Lexeme] -> Either String [Lexeme]
rightbrac ((RPAR n):ts) = Right ts


errors :: [Lexeme]-> String -> Either String ([Lexeme], [Stmt String])
errors toks y = Left $ "Error: " ++ y ++" : Couldn't parse\n" ++ show toks
                              ++ "\nExp Stringecting a number got " ++ show (head toks) 
                              
errors1 :: [Lexeme]-> String -> Either String ([Lexeme], Stmt String)
errors1 toks y = Left $ "Error: " ++ y ++" : Couldn't parse\n" ++ show toks
                              ++ "\nExp Stringecting a number got " ++ show (head toks) 

instance (Out a) => Out (Stmt a) where                       
		
  doc (If a b c) =  parens $ text "If" $$ nest 1 (doc a)
                                                     $$ nest 1(doc b) 
                                                     $$ nest 1(doc c) 
  doc (While a b) = parens $ text "While" $$ nest 1 (doc a) $$ nest 2 (doc b)
  doc (Assign a b) = parens $ text "Assign" $$ nest 1 (doc a) 
                                                           $$ nest 2 (doc b)
  doc (Block a ) = parens $ text "Block" $$ nest 1 (doc a)
  doc (Write a ) = parens $ text "Write" $$ nest 1 (doc a)
  doc (Input a ) = parens $ text "Input" $$ nest 1 (doc a)
  
  docPrec _ = doc

instance (Out a) => Out (Exp a) where                       
	
	doc (Add a b) =  parens $ text "Add" $$ nest 1 (doc a) $$ nest 2 (doc b)
	doc (Mul a b) = parens $ text "Mul" $$ nest 1 (doc a) $$ nest 2 (doc b)
	doc (Div a b) = parens $ text "Div" $$ nest 1 (doc a) $$ nest 2 (doc b)
	doc (Sub a ) = parens $ text "Sub" $$ nest 1 (doc a)
	doc (Id a ) = parens $ text "Id" $$ nest 1 (doc a)
	doc (Num a ) = parens $ text "Num" $$ nest 1 (doc a)

	docPrec _ = doc
    
stackStmt :: Int -> Stmt String -> (Int, String)
stackStmt n (If e s1 s2) = (m,  (stackExp e) ++"cJump L"++(show n)++"\n"++code1++"JUMP L"++(show(n+1))++"\n"
                                      ++"L"++(show n)++":\n"++code2++"L"++(show(n+1)++":\n")) where
                                      (n', code1) = stackStmt(n+2) s1
                                      (m, code2) = stackStmt n' s2

stackStmt n (Assign s e) = (n, (stackExp e)++"LOAD "++s++"\n")
stackStmt n (While e s1) = (m, "L"++show(n)++":\n"++(stackExp e)++"cJump L"++(show (n+1))++"\n"++code1++"JUMP L"
                                      ++(show(n))++"\n"++"L"++show(n+1)++":\n") where
                                      (m, code1) = stackStmt (n+1) s1


stackStmt n (Block []) = (n, "")
stackStmt n (Block (x:xs)) = (n2, x1++x2) where
                             (n1, x1) = stackStmt (n+1) x
                             (n2, x2) = stackStmt n1 (Block xs)                            
stackStmt n (Write e) = (n, (stackExp e)++"PRINT\n")
stackStmt n (Input (Id s)) = (n, "READ "++s++"\n")                          


stackExp :: Exp String -> String
stackExp (Add e1 e2) = (stackExp e1) ++ (stackExp e2) ++"OP2 +"++"\n"
stackExp (Mul e1 e2) = (stackExp e1) ++ (stackExp e2) ++"OP2 +"++"\n"
stackExp (Div e1 e2) = (stackExp e1) ++ (stackExp e2) ++"OP2 +"++"\n"
stackExp (Sub e1) = (stackExp e1)++"OP1 -"++"\n"
stackExp (Id s) = ("rPush "++ s ++"\n")
stackExp (Num s) = ("cPush "++ (show s) ++"\n")


main = do
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./eng_lang inputfile > got < ./eng_lang >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            conts <- readFile fname
            let etok = tokens conts 
            case etok of
               Right tok -> do
                   putStrLn "\n**************************************\n"
                   putStrLn "The List of tokens are as follows.\n"
                   mapM_ (putStrLn.show) tok 
               Left msg -> do  
                  putStrLn msg -} 
  l <- mlex
  case l of
    Right ts -> do
      let parse = prog ts
      case parse of 
         Right ([], ast) -> do
             putStrLn "Parsing Successful"
             putStrLn $ "AST is :\n" ++ show ast
             pp ast
             let(count, output) = stackStmt 1 ast             
             putStrLn output 
             writeFile "outputTest.txt" output
         Left str -> do putStrLn str 
    Left tsp -> do putStrLn tsp

