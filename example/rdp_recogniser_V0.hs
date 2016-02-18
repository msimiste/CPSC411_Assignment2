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

factor -> NUM.                     factor ((NUM n):ts) = ts


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
     
     
    
