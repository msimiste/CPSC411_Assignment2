{
module MLexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :- 
       $white+				  ;
       if                                 {\s -> IF}
       then                               {\s -> THEN}
       while                              {\s -> WHILE}
       do                                 {\s -> DO}
       input                              {\s -> INPUT}
       else                               {\s -> ELSE}
       begin                              {\s -> BEGIN}
       end                                {\s -> END}
       write                              {\s -> WRITE}
       \+                                 {\s -> ADD}
       \:=                                {\s -> ASSIGN}
       \-                                 {\s -> SUB}
       \*                                 {\s  -> MUL}
       \/                                 {\s  -> DIV}
       \(                                 {\s  -> LPAR} 
       \)                                 {\s  -> RPAR}
       $digit+			          {\s -> NUM (read s) }
       $alpha [$alpha $digit \_ \`]*	  {\s -> ID s }
       
      
{
-- Each action has type :: String -> Token

-- The token type:
data Tokens = IF
            |THEN
            |WHILE
            |DO
            |INPUT
            |ELSE
            |BEGIN
            |END
            |WRITE
            |ADD
            |ASSIGN
            |SUB
            |MUL
            |DIV
            |LPAR
            |RPAR
            |SEMICOLON
            |NUM Int
            |ID String
             deriving (Eq,Show)

mlex :: IO [Tokens]
mlex = do
  putStrLn "Enter the input"
  s <- getLine
  return (alexScanTokens s)
}

