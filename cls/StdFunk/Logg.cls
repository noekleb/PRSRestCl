 
 /*------------------------------------------------------------------------
    File        : Logg
    Purpose     : 
    Syntax      : 
    Description : Skriver melding til loggfil
    Author(s)   : tny
    Created     : Thu mar 27 10:30:07 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/* BLOCK-LEVEL ON ERROR UNDO, THROW. */ 

CLASS Logg: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cLogg  AS CHARACTER NO-UNDO.

    DEFINE STREAM Inn.
    DEFINE STREAM Ut.
		
	CONSTRUCTOR PUBLIC Logg (  ):
		SUPER ().

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose: Skriver tekstlinje til loggfil.
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PUBLIC VOID SkrivTilLogg(INPUT cLoggfil AS CHARACTER,
	                                INPUT cTekst   AS CHARACTER ):

        DEFINE VARIABLE cDatoTid AS CHAR NO-UNDO.
        DEFINE VARIABLE cFilNavn AS CHAR NO-UNDO.
        DEFINE VARIABLE cKatalog AS CHAR NO-UNDO.

        cKatalog = '.\LOG'.
        IF cKatalog <> '' THEN 
        DO:
            /* Sikrer at katalog finnes. */
            OS-CREATE-DIR VALUE(RIGHT-TRIM(cKatalog,'\')).    
            cKatalog = RIGHT-TRIM(cKatalog,'\') + '\'.
        END.

        ASSIGN
            cFilNavn = cKatalog + cLoggfil + '.log'
            cDatoTid = STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' '
            .

        OUTPUT STREAM Ut TO VALUE(cFilNavn) APPEND UNBUFFERED NO-ECHO.

        PUT STREAM Ut UNFORMATTED
            cDatoTid
            cTekst SKIP.

        OUTPUT STREAM Ut CLOSE.
		
		RETURN.

	END METHOD. /* SkrivTilLogg*/

	DESTRUCTOR PUBLIC Logg ( ):

	END DESTRUCTOR.

END CLASS.