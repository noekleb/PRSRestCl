DEF INPUT PARAMETER cURL AS CHAR NO-UNDO.

DEFINE VARIABLE cProgramName  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPageAddress  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iReturnResult AS INTEGER    NO-UNDO.

ASSIGN
    cProgramName = "C:\~"Program Files~"\~"Internet Explorer~"\iexplore.exe"
    cPageAddress = cURL.

OS-COMMAND SILENT ( START VALUE(cProgramName  + CHR(32) + cPageAddress)).

/*                                                                                          */
/*RUN WinExec (INPUT cProgramName + CHR(32) + cPageAddress , INPUT 1, OUTPUT iReturnResult).*/
/*                                                                                          */
/*PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":                                                */
/*    DEFINE INPUT  PARAMETER ProgramName AS CHARACTER.                                     */
/*    DEFINE INPUT  PARAMETER VisualStyle AS LONG.                                          */
/*    DEFINE RETURN PARAMETER StatusCode  AS LONG.                                          */
/*END PROCEDURE.                                                                            */
