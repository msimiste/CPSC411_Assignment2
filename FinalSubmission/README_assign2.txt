Mike Simister
10095107
CPSC411 Assignment 2 W2016

-----------------------------------------------
1. List of included Files
-----------------------------------------------

    A2Lexer.x
    A2Recognizer.hs
    MTest1.txt
    MTest2.txt
    MTest3.txt
    MTest4.txt
    MTest5txt
    Makefile
    README_assign2.txt
    A2Grammar.txt


------------------------------------------------
2. Initial Exeution:

    Pretty Printing is done with GenericPretty
    Detailed installation and operation instructions can be found
    here:
    http://hackage.haskell.org/package/GenericPretty

    otherwise, install on *nix:

     $ cabal update
     $ cabal install GenericPretty
------------------------------------------------

    On *nix systems to compile:

            $ make

        To clean all generated files:

            $ make clean
        
        To run individual test and output to a default filename (outputDefault.txt)

            $./A2Recognizer <input file>

        To run individual test and output to a specific filename

            $./A2Recognizer <input file> <output file>
        
        If user forgets or chooses not to enter <input file> the 
        program will start and prompt user for an <input file> a 
        non-existent <input file> will result in an error. An 
        existing input file will result in output to the default
        <output file> in the working directory.


    On windows systems:

        To compile:

            C:user/path> alex A2Lexer.x
            C:user/path> ghc --make A2Recognizer.hs

            To run:

            C:user/path> A2Recognizer.exe <input file>
            or:
            C:user/path> A2Recognizer.exe <input file> <output file>
            or:
            C:user/path> A2Recognizer.exe 

            *** see description in *nix section for behaviour w.r.t. various
                type of input methods.

 
-----------------------------------------------
3. Grammar Transformation
-----------------------------------------------
 
Original Grammar:

                        prog -> stmt. 
                        stmt -> IF expr THEN stmt ELSE stmt
                                    | WHILE expr DO stmt
                                    | INPUT ID
                                    | ID ASSIGN expr
                                    | WRITE expr
                                    | BEGIN stmtlist END. 
                        stmtlist -> stmtlist stmt SEMICOLON
                                    |. 
                        expr -> expr addop term 
                                    | term. 
                        addop -> ADD
                                    | SUB. 
                        term -> term mulop factor 
                                    | factor. 
                        mulop -> MUL
                                    | DIV. 
                        factor -> LPAR expr RPAR
                                    | ID
                                    | NUM
                                    | SUB NUM.
            
Transformed Recursive Descent Grammar:

                        prog -> stmt.
                        stmt -> IF expr thenpart elsepart.
                             | WHILE expr dopart.
                             | INPUT ID.
                             | ID ASSIGN expr.
                             | WRITE expr.
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

                        expr' -> ADD term expr'.
                             | SUB term expr'.  
                             |.
                             
                        term  ->  factor term'.

                        term' -> MUL factor term'.
                             | DIV factor term'.    
                             |.
                             
                        factor -> LPAR expr rightbrac.
                             |    ID. 
                             |    NUM.
                             |    SUB NUM.

                        rightbrac -> RPAR.
