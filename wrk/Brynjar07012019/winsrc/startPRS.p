
/*------------------------------------------------------------------------
    File        : startMypage.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Jun 21 14:32:27 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF VAR hServer AS HANDLE NO-UNDO.
/*
CREATE SERVER hServer.

hServer:CONNECT("-H QA-SXD6E-013 -S 3050 -AppService bafoas -DirectConnect").
*/
hServer = SESSION.

RUN InitPRS.p (os-getenv("username"),hServer).
WAIT-FOR System.Windows.Forms.Application:Run().
