/* Update table-to-table link
   Parameters:  <parentid>|<childid-list>

   Created: 07/01/19 by brynj
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix          AS INT    NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.
DEF VAR hAsLib 	    AS HANDLE NO-UNDO.

hAsLib = DYNAMIC-FUNCTION("startASlib" IN SOURCE-PROCEDURE).

/* To make the lib available to a sub-procedure: */
FUNCTION startASlib RETURNS HANDLE():
  RETURN hAsLib.
END FUNCTION.

cUserId = ENTRY(1,icParam,"|").

/*
DO TRANSACTION:
  FOR EACH JBoxCompanyUser EXCLUSIVE-LOCK
      WHERE JBoxCompanyUser.cJBoxUserId = cUserId:
    IF LOOKUP(STRING(JBoxCompanyUser.iJBoxCompanyId),icParam,"|") = 0 THEN
      DELETE JBoxCompanyUser.
  END.
  DO ix = 2 TO NUM-ENTRIES(icParam,"|"):
    FIND JBoxCompanyUser
         WHERE JBoxCompanyUser.cJBoxUserId = cUserId
           AND JBoxCompanyUser.iJBoxCompanyId  = INT(ENTRY(ix,icParam,"|"))
         NO-LOCK NO-ERROR.
    IF NOT AVAIL JBoxCompanyUser THEN DO:
      CREATE JBoxCompanyUser.
      ASSIGN JBoxCompanyUser.cJBoxUserId    = cUserId
             JBoxCompanyUser.iJBoxCompanyId = INT(ENTRY(ix,icParam,"|"))
             .
    END.
  END.
END.
*/

FINALLY:
  obOk = ocReturn = "".
END FINALLY.
