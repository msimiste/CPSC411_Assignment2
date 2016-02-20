{
module MLexer where


import Data.Char (chr)
import System.Environment


}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$newLine = \n

-- the line below will only be used if special chars are allowed to be included in ID values
$other = [\!\@\#\$\^\&\'\"\<\>\.\,\?\-\|\/\_\:]  



tokens :-

$white+                     				{ skip }
"%".*$newLine                  					;
if                           				{ if' }
then                         				{ then' }
while                        				{ while }
do                           				{ do' }
input                        				{ input }
else                         				{ else' }
begin                        				{ begin' }
end                          				{ end }
write                        				{ write }
$digit+                      				{ num } 
$alpha[$alpha$digit]*        				{ word }
":="                         				{ assign }
"+"                          				{ add }
"-"                          				{ sub }
"*"                          				{ mul }
"/"                          				{ div' }
"("                          				{ lpar }
")"                          				{ rpar }
";"                          				{ semicolon }
"/*"                         				{ nested_comment }

-- the line below will only be used if special chars are allowed to be included in ID values

--$alpha[$other$alpha$digit]*              	{ word } 

{



{-

type AlexInput =  (AlexPosn, Char, [Bytes], String)

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],   -- rest of the bytes for the current char
        alex_scd :: !Int        -- the current startcode
    }

newtype Alex a = Alex { unAlex :: AlexState
                               -> Either String (AlexState, a) }

runAlex          :: String -> Alex a -> Either String a

alexGetInput     :: Alex AlexInput
alexSetInput     :: AlexInput -> Alex ()

alexError        :: String -> Alex a

alexGetStartCode :: Alex Int
alexSetStartCode :: Int -> Alex ()

alexMonadScan :: Alex result

The token actions should have the following type:

type AlexAction result = AlexInput -> Int -> Alex result
{ ... }  :: AlexAction result


 -}

   
data Lexeme = ID String AlexPosn
             | IF AlexPosn
             | THEN AlexPosn
             | WHILE AlexPosn
             | DO AlexPosn
             | INPUT AlexPosn
             | ELSE AlexPosn
             | BEGIN AlexPosn
             | END AlexPosn
             | WRITE AlexPosn
             | ASSIGN AlexPosn
             | ADD AlexPosn             
             | SUB AlexPosn
             | MUL AlexPosn
             | DIV AlexPosn
             | NUM Int AlexPosn
             | LPAR AlexPosn
             | RPAR AlexPosn
             | SEMICOLON AlexPosn
             | LEOF  
  deriving (Show,Eq)

word :: AlexInput -> Int -> Alex Lexeme
word  (posn,c,_,inp) len =  return $ ID (take len inp) posn

if' :: AlexInput -> int -> Alex Lexeme
if' (posn,c,_,inp) len =  return $ IF posn

then' :: AlexInput -> int -> Alex Lexeme
then' (posn,c,_,inp) len =  return $ THEN posn

while :: AlexInput -> int -> Alex Lexeme
while (posn,c,_,inp) len =  return $ WHILE posn

do' :: AlexInput -> int -> Alex Lexeme
do' (posn,c,_,inp) len =  return $ DO posn

input :: AlexInput -> int -> Alex Lexeme
input (posn,c,_,inp) len =  return $ INPUT posn

else' :: AlexInput -> int -> Alex Lexeme
else' (posn,c,_,inp) len =  return $ ELSE posn

begin' :: AlexInput -> int -> Alex Lexeme
begin' (posn,c,_,inp) len =  return $ BEGIN posn

end :: AlexInput -> int -> Alex Lexeme
end (posn,c,_,inp) len =  return $ END posn

write :: AlexInput -> int -> Alex Lexeme
write (posn,c,_,inp) len =  return $ WRITE posn

assign :: AlexInput -> Int -> Alex Lexeme
assign (posn,c,_,inp) len =  return $ ASSIGN posn

add :: AlexInput -> Int -> Alex Lexeme
add (posn,c,_,inp) len =  return $ ADD  posn

num :: AlexInput -> Int -> Alex Lexeme
num (posn,c,_,inp) len =  return $ NUM (read (take len inp)) posn

semicolon :: AlexInput -> Int -> Alex Lexeme
semicolon (posn,c,_,inp) len =  return $ SEMICOLON  posn

sub :: AlexInput -> Int -> Alex Lexeme
sub (posn,c,_,inp) len =  return $ SUB posn

mul :: AlexInput -> Int -> Alex Lexeme
mul (posn,c,_,inp) len =  return $ MUL posn

div' :: AlexInput -> Int -> Alex Lexeme
div' (posn,c,_,inp) len =  return $ DIV posn

lpar :: AlexInput -> Int -> Alex Lexeme
lpar (posn,c,_,inp) len =  return $ LPAR posn

rpar :: AlexInput -> Int -> Alex Lexeme
rpar (posn,c,_,inp) len =  return $ RPAR posn

alexEOF = return LEOF


tokens str = runAlex str $ do
               let loop = do tok <- alexMonadScan
                             if tok == LEOF
                               then return []
                               else do toks <- loop
                                       return $ tok : toks
               loop
          

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment input _ = do
  input <- alexGetInput 
  go 1 input
  where      
      go 0 input = do 
         alexSetInput input
         alexMonadScan
      go n input = do
              case alexGetByte input of
                 Nothing  -> err input
                 Just (c,input) -> do
                      case chr (fromIntegral c) of
                          '*' -> do
                              case alexGetByte input of
                                Nothing  -> err input 
                                Just(42,(a,b,f,d)) -> go n (a,b,f,'\42':d)                                                    
                                Just (47,input) -> go (n-1) input                                
                                Just (c,input)   -> go n input
                          '\47' -> do
                              case alexGetByte input of
                                Nothing  -> err input
                                Just (c,input) | c == fromIntegral (ord '*') -> go (n+1) input
                                Just (c,input)   -> go n input
                          '\37' -> do
                               case alexGetByte input of
                                Nothing  -> err input                                                           
                                Just (c,input)   -> go keepN keepInput where
                                  retVal = singleComment n input
                                  keepN = fst retVal
                                  keepInput = snd retVal                        
                          c -> go n input
      err input = do 
        alexSetInput input;
        lexError $ "error in nested comment"

singleComment :: Int -> AlexInput -> (Int, AlexInput)
singleComment x (a,b,c,(d:e:g))
    | d == '\n' =  (x, (a,b,c,e:g))    
    | otherwise = singleComment x (a,b,c,(e:g))
singleComment x s = (x,s)
    
    
showPosn (AlexPn _ line col) = show line ++ ':': show col


lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++ 
       (if (not (null input))
         then " before " ++ show (head input)
         else " at end of file"))
 

test = do 
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
                  putStrLn msg  
                   




mlex :: IO (Either String [Lexeme])
mlex = do
  putStrLn "Enter the input"
  s <- getLine
  --putStrLn "\n**************************************\n"
  abc <-return (tokens s)
  --putStrLn ("Brian does not believe!!!" ++ (show abc))
  
  return abc 
  --
}

