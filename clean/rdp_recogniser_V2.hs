module Main where
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
-}
exp1:: [Tokens] -> Either String [Tokens]
exp1  ts = case eth_str_tok of
               Right toks       -> more_exp toks 
               Left  errormsg   -> Left errormsg 
             where
                eth_str_tok = term ts



---------------------------------------------

more_exp :: [Tokens] ->  Either String [Tokens]
more_exp (ADD:ts)  = case eth_str_tok of
                        Right toks     -> more_exp toks 
                        Left errormsg  -> Left errormsg
                     where
                         eth_str_tok = term ts
    
more_exp ts        = Right ts 
----------------------------------------------

term :: [Tokens] -> Either String [Tokens]
term  ts  = case eth_str_tok of 
                Right toks    -> more_term toks 
                Left errormsg -> Left errormsg    
              where 
                eth_str_tok = factor ts 
-----------------------------------------------

more_term :: [Tokens] -> Either String [Tokens]
more_term (MUL:ts) = case eth_str_tok of
                        Right toks    -> more_term toks 
                        Left errormsg -> Left errormsg
                    where 
                        eth_str_tok = factor ts 
                        
more_term ts       = Right ts

-----------------------------------------------

factor :: [Tokens] ->  Either String [Tokens]
factor ((NUM n):ts)  = Right ts
factor toks          = Left $ "Error:In factor: Couldn't parse\n" ++ show toks
                              ++ "\nExpecting a number got " ++ show (head toks) 


main = do
  tokList <- mlex
  let parseRes = exp1 tokList
  case parseRes of
    Left str -> putStrLn str 
    Right [] -> putStrLn $ "Parse Successful.\n" 

     
    
