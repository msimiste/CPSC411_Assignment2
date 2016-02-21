#Compiles the alex file as well as the haskell files 
# and runs the tests for Assignment 1 CPSC411

CC=ghc

.SILENT:

all: first second 


second: A2Recognizer.hs
	ghc --make A2Recognizer.hs
		
first: A2Lexer.x
	alex A2Lexer.x
	
clean: 
	@rm -f A2Lexer.hs *.exe *.hi *.o
	

