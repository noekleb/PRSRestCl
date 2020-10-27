&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.ObjectModelParser.

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin*/

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoopY AS INTEGER NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE cJSonString AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cJSonFileName AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusCode AS INTEGER NO-UNDO.
DEFINE VARIABLE cStatusReason AS CHARACTER NO-UNDO.
DEFINE VARIABLE cToken AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBearer AS CHARACTER NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE cErorTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParametre AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst     AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cTekst2    AS CHARACTER         NO-UNDO.

DEFINE VARIABLE oJsonObject AS JsonObject NO-UNDO.
DEFINE VARIABLE oJsonObject2 AS JsonObject NO-UNDO.
DEFINE VARIABLE oParser      AS ObjectModelParser NO-UNDO.

{cls\DinTero\clientDinTero.i}
{cls\DinTero\ttOrder.i}
{cls\DinTero\dsOrder.i}

DEFINE VARIABLE rClientDinTero AS cls.DinTero.ClientDinTero NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectCreateSession rectGetSessionDetails ~
btnGetToken edToken_Entity btnListSessions fiFilListSessions ~
btnCreateSession btnListTransactions fiFilListTransactions fiCustomer_id ~
btnListProfiles fiFilListProfiles fiCustomer_eMail fiCustomer_phone_number ~
btnVoidTransaction fiFilVoidTransaction tgSms btnRefundTransaction ~
fiFilRefundTransaction fiKOrdre_Id fiSesjonsId btnGetSessionDetails ~
fiFilGetSessioonDetails fiTransactionId btnGetTransaction ~
fiFilGetTransaction 
&Scoped-Define DISPLAYED-OBJECTS edToken_Entity fiGetTokenStatusCode ~
fiExpires_in fitoken_type fiAccess_Token fiToken_StatusReason fiToken_Read ~
fiFilListSessions fiFilCreateSessions fiFilListTransactions fiCustomer_id ~
fiFilListProfiles fiCustomer_eMail fiCustomer_phone_number ~
fiFilVoidTransaction tgSms fiFilRefundTransaction fiKOrdre_Id fiSesjonsId ~
fiFilGetSessioonDetails fiTransactionId fiFilGetTransaction 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCreateSession 
     LABEL "CreateSession" 
     SIZE 25 BY 1.14 TOOLTIP "Opprettes en sesjon,".

DEFINE BUTTON btnGetSessionDetails 
     LABEL "GetSessionDetails" 
     SIZE 25 BY 1.14.

DEFINE BUTTON btnGetToken 
     LABEL "GetToken" 
     SIZE 15 BY 1.14 TOOLTIP "Henter token fra DinTero".

DEFINE BUTTON btnGetTransaction 
     LABEL "GetTransaction" 
     SIZE 25 BY 1.14 TOOLTIP "Henter detaljer om en transaksjon.".

DEFINE BUTTON btnListProfiles 
     LABEL "ListProfiles" 
     SIZE 25 BY 1.14 TOOLTIP "Lister alle betalingsprofiler.".

DEFINE BUTTON btnListSessions 
     LABEL "ListSessions" 
     SIZE 25 BY 1.14 TOOLTIP "Henter liste med sesjoner og legger dem i en JSon fil.".

DEFINE BUTTON btnListTransactions 
     LABEL "ListTransactions" 
     SIZE 25 BY 1.14 TOOLTIP "Lister registrerte transaksjoner".

DEFINE BUTTON btnRefundTransaction 
     LABEL "refundTransaction" 
     SIZE 25 BY 1.14 TOOLTIP "Retur av vare.".

DEFINE BUTTON btnVoidTransaction 
     LABEL "VoidTransaction" 
     SIZE 25 BY 1.14 TOOLTIP "Void transaction".

DEFINE VARIABLE edToken_Entity AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 136.2 BY 4.29 TOOLTIP "Inneholder token respons JSon melding." NO-UNDO.

DEFINE VARIABLE fiAccess_Token AS CHARACTER FORMAT "X(256)":U 
     LABEL "Access Token" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 152 BY 1.1 TOOLTIP "Access Token" NO-UNDO.

DEFINE VARIABLE fiCustomer_eMail AS CHARACTER FORMAT "X(256)":U INITIAL "tomn@nsoft.no" 
     LABEL "eMail" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Kundens eMail" NO-UNDO.

DEFINE VARIABLE fiCustomer_id AS CHARACTER FORMAT "X(256)":U INITIAL "Tom Nøkleby" 
     LABEL "Id/Navn" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Id eller navn på kunde" NO-UNDO.

DEFINE VARIABLE fiCustomer_phone_number AS CHARACTER FORMAT "X(256)":U INITIAL "41365436" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Kundens mobilnr" NO-UNDO.

DEFINE VARIABLE fiExpires_in AS CHARACTER FORMAT "X(256)":U 
     LABEL "Expires_in" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY 1.1 TOOLTIP "Expires_in" NO-UNDO.

DEFINE VARIABLE fiFilCreateSessions AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiFilGetSessioonDetails AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiFilGetTransaction AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Detaljer om en transaksjon," NO-UNDO.

DEFINE VARIABLE fiFilListProfiles AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Fil som inneholder oversikt over alle betalingsprofiler." NO-UNDO.

DEFINE VARIABLE fiFilListSessions AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Filnav" NO-UNDO.

DEFINE VARIABLE fiFilListTransactions AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Fil med liste over transaksjoner." NO-UNDO.

DEFINE VARIABLE fiFilRefundTransaction AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiFilVoidTransaction AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiGetTokenStatusCode AS CHARACTER FORMAT "X(256)":U 
     LABEL "TokenStatusCode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY 1.1 TOOLTIP "GetTokenStatusCode"
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiKOrdre_Id AS CHARACTER FORMAT "X(256)":U INITIAL "1200000011" 
     LABEL "Kundeordre id" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Kundeordrens id." NO-UNDO.

DEFINE VARIABLE fiSesjonsId AS CHARACTER FORMAT "X(256)":U 
     LABEL "SesjonsId" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiToken_Read AS DATETIME FORMAT "99/99/99 HH:MM:SS":U 
     LABEL "Last Read" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 30 BY 1.1 TOOLTIP "Siste gang token var hentet" NO-UNDO.

DEFINE VARIABLE fiToken_StatusReason AS CHARACTER FORMAT "X(256)":U 
     LABEL "StatusReason" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 152 BY 1.1 TOOLTIP "Token StatusReason" NO-UNDO.

DEFINE VARIABLE fitoken_type AS CHARACTER FORMAT "X(256)":U 
     LABEL "Token_type" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 15 BY 1.1 TOOLTIP "token_type" NO-UNDO.

DEFINE VARIABLE fiTransactionId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transaksjonsid" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE rectCreateSession
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.4 BY 8.24.

DEFINE RECTANGLE rectGetSessionDetails
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69.4 BY 4.05.

DEFINE VARIABLE tgSms AS LOGICAL INITIAL YES 
     LABEL "betale via SMS" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 TOOLTIP "Marker for at kunde skal få link sendt via SMS til mobil." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnGetToken AT ROW 1.71 COL 20 WIDGET-ID 2
     edToken_Entity AT ROW 1.76 COL 36 NO-LABEL WIDGET-ID 4
     fiGetTokenStatusCode AT ROW 2.86 COL 18.2 COLON-ALIGNED
     fiExpires_in AT ROW 4 COL 18.2 COLON-ALIGNED
     fitoken_type AT ROW 5.14 COL 18.2 COLON-ALIGNED
     fiAccess_Token AT ROW 6.29 COL 18.2 COLON-ALIGNED
     fiToken_StatusReason AT ROW 7.43 COL 18.2 COLON-ALIGNED
     fiToken_Read AT ROW 8.57 COL 18.2 COLON-ALIGNED
     btnListSessions AT ROW 9.81 COL 112 WIDGET-ID 6
     fiFilListSessions AT ROW 9.91 COL 138.6 COLON-ALIGNED
     fiFilCreateSessions AT ROW 10.43 COL 28.2 COLON-ALIGNED
     btnCreateSession AT ROW 10.48 COL 1.6 WIDGET-ID 12
     btnListTransactions AT ROW 11 COL 112 WIDGET-ID 14
     fiFilListTransactions AT ROW 11.1 COL 138.6 COLON-ALIGNED
     fiCustomer_id AT ROW 11.62 COL 28.2 COLON-ALIGNED
     btnListProfiles AT ROW 12.19 COL 112 WIDGET-ID 8
     fiFilListProfiles AT ROW 12.24 COL 138.6 COLON-ALIGNED
     fiCustomer_eMail AT ROW 12.62 COL 28.2 COLON-ALIGNED
     fiCustomer_phone_number AT ROW 13.62 COL 28.2 COLON-ALIGNED
     btnVoidTransaction AT ROW 13.86 COL 112 WIDGET-ID 18
     fiFilVoidTransaction AT ROW 13.91 COL 138.6 COLON-ALIGNED
     tgSms AT ROW 14.86 COL 30.4 WIDGET-ID 24
     btnRefundTransaction AT ROW 15.1 COL 112 WIDGET-ID 20
     fiFilRefundTransaction AT ROW 15.19 COL 138.6 COLON-ALIGNED
     fiKOrdre_Id AT ROW 15.95 COL 28.2 COLON-ALIGNED
     fiSesjonsId AT ROW 17.1 COL 28.2 COLON-ALIGNED
     btnGetSessionDetails AT ROW 19.57 COL 2 WIDGET-ID 36
     fiFilGetSessioonDetails AT ROW 19.57 COL 28.8 COLON-ALIGNED
     fiTransactionId AT ROW 20.81 COL 28.8 COLON-ALIGNED
     btnGetTransaction AT ROW 21.95 COL 2 WIDGET-ID 16
     fiFilGetTransaction AT ROW 22.05 COL 28.6 COLON-ALIGNED
     "Henter sesjonsstatus og transaksjonsid" VIEW-AS TEXT
          SIZE 49.8 BY .62 AT ROW 18.76 COL 3.2 WIDGET-ID 34
          FONT 6
     "Opprettelse/registrering av ny sesjon (Salg)" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 9.71 COL 2.4 WIDGET-ID 30
          FONT 6
     rectCreateSession AT ROW 10.29 COL 1 WIDGET-ID 22
     rectGetSessionDetails AT ROW 19.33 COL 1 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 173.4 BY 26.62 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Testrutine for DinTero API'et"
         HEIGHT             = 26.62
         WIDTH              = 173.4
         MAX-HEIGHT         = 26.62
         MAX-WIDTH          = 173.4
         VIRTUAL-HEIGHT     = 26.62
         VIRTUAL-WIDTH      = 173.4
         RESIZE             = YES
         SCROLL-BARS        = NO
         STATUS-AREA        = NO
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = YES
         THREE-D            = YES
         MESSAGE-AREA       = NO
         SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */
&IF "{&WINDOW-SYSTEM}" EQ "TTY" &THEN
  IF C-Win = ? THEN
    CREATE FRAME C-Win
           ASSIGN HIDDEN = YES.
&ENDIF

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 26.62
       FRAME DEFAULT-FRAME:WIDTH            = 173.4.

/* SETTINGS FOR FILL-IN fiAccess_Token IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiCustomer_eMail:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "tomn@nsoft.no".

ASSIGN 
       fiCustomer_id:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Tom Nøkleby".

ASSIGN 
       fiCustomer_phone_number:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "41365436".

/* SETTINGS FOR FILL-IN fiExpires_in IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiFilCreateSessions IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiFilCreateSessions:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiGetTokenStatusCode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiGetTokenStatusCode:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiKOrdre_Id:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "1200000011".

/* SETTINGS FOR FILL-IN fiToken_Read IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiToken_StatusReason IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fitoken_type IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tgSms:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "yes".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Testrutine for DinTero API'et */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Testrutine for DinTero API'et */
DO:
  EMPTY TEMP-TABLE ttoJSon. 
  EMPTY TEMP-TABLE ttUrl.
  EMPTY TEMP-TABLE ttOrder.
  EMPTY TEMP-TABLE ttItems.
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCreateSession
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCreateSession C-Win
ON CHOOSE OF btnCreateSession IN FRAME DEFAULT-FRAME /* CreateSession */
DO:
  RUN createSession.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGetSessionDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGetSessionDetails C-Win
ON CHOOSE OF btnGetSessionDetails IN FRAME DEFAULT-FRAME /* GetSessionDetails */
DO:
  RUN getGetSessionDetails.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGetToken
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGetToken C-Win
ON CHOOSE OF btnGetToken IN FRAME DEFAULT-FRAME /* GetToken */
DO:
  
  RUN getTokenProc.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGetTransaction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGetTransaction C-Win
ON CHOOSE OF btnGetTransaction IN FRAME DEFAULT-FRAME /* GetTransaction */
DO:
  RUN getTransaction.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnListProfiles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnListProfiles C-Win
ON CHOOSE OF btnListProfiles IN FRAME DEFAULT-FRAME /* ListProfiles */
DO:
  RUN ListProfilesProc.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnListSessions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnListSessions C-Win
ON CHOOSE OF btnListSessions IN FRAME DEFAULT-FRAME /* ListSessions */
DO:
  RUN listSessionsProc.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnListTransactions
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnListTransactions C-Win
ON CHOOSE OF btnListTransactions IN FRAME DEFAULT-FRAME /* ListTransactions */
DO:
  RUN listTransactionsProc.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefundTransaction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefundTransaction C-Win
ON CHOOSE OF btnRefundTransaction IN FRAME DEFAULT-FRAME /* refundTransaction */
DO:
  RUN refundTransaction.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVoidTransaction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVoidTransaction C-Win
ON CHOOSE OF btnVoidTransaction IN FRAME DEFAULT-FRAME /* VoidTransaction */
DO:
  RUN voidTransaction.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustomer_phone_number
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustomer_phone_number C-Win
ON VALUE-CHANGED OF fiCustomer_phone_number IN FRAME DEFAULT-FRAME /* Mobil */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      tgSms:SCREEN-VALUE = (IF fiCustomer_phone_number:SCREEN-VALUE <> '' THEN 'yes' ELSE 'no')
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
&ENDIF

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
{incl/wintrigg.i}
/*{incl/conttrigg.i oBrw<>:BROWSE-HANDLE} */
&ENDIF

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &IF DEFINED(UIB_is_Running) = 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    SESSION:SET-WAIT-STATE("").
    IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
      RUN WaitForForm NO-ERROR.
    ELSE 
    &ENDIF
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createSession C-Win 
PROCEDURE createSession :
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 
   
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE ttCreateSessionRespons.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    IF fiCustomer_id:SCREEN-VALUE = '' OR 
       fiCustomer_eMail:SCREEN-VALUE = '' OR 
       fiCustomer_phone_number:SCREEN-VALUE = '' THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Alle tre kundedata må være angitt.").
      RETURN.
    END.
    
    ASSIGN
      cParametre = fiCustomer_id:SCREEN-VALUE + '|' +
                   fiCustomer_eMail:SCREEN-VALUE + '|' +
                   fiCustomer_phone_number:SCREEN-VALUE + '|' + 
                   fiKOrdre_Id:SCREEN-VALUE + '|' + 
                   tgSms:SCREEN-VALUE 
      .
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'createSession|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved henting av token: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.
    IF iStatusCode = 200 THEN
    DO: 
      ASSIGN 
        cStatusReason = ENTRY(2,pcReturnValue,'|')
        cType         = ENTRY(3,pcReturnValue,'|')
        cJSonFileName = ENTRY(4,pcReturnValue,'|')
        cJSonString   = ENTRY(5,pcReturnValue,'|')
        .
      TEMP-TABLE ttCreateSessionRespons:READ-JSON('longchar', cJSonString, "EMPTY").
      FIND FIRST ttCreateSessionRespons NO-ERROR.
      IF AVAILABLE ttCreateSessionRespons THEN
      DO:
        fiSesjonsId:SCREEN-VALUE = ttCreateSessionRespons.cId.
      END.
      ELSE 
       fiSesjonsId:SCREEN-VALUE = ''.
        
    END.
    ELSE DO:  
      ASSIGN 
        cStatusReason = ENTRY(2,pcReturnValue,'|')
        cType         = ''
        cJSonFileName = ''
        cJSonString   = ''
        fiSesjonsId:SCREEN-VALUE = ''.
        .
    END.    

    ASSIGN 
      fiFilCreateSessions:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeCreateSession:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY edToken_Entity fiGetTokenStatusCode fiExpires_in fitoken_type 
          fiAccess_Token fiToken_StatusReason fiToken_Read fiFilListSessions 
          fiFilCreateSessions fiFilListTransactions fiCustomer_id 
          fiFilListProfiles fiCustomer_eMail fiCustomer_phone_number 
          fiFilVoidTransaction tgSms fiFilRefundTransaction fiKOrdre_Id 
          fiSesjonsId fiFilGetSessioonDetails fiTransactionId 
          fiFilGetTransaction 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectCreateSession rectGetSessionDetails btnGetToken edToken_Entity 
         btnListSessions fiFilListSessions btnCreateSession btnListTransactions 
         fiFilListTransactions fiCustomer_id btnListProfiles fiFilListProfiles 
         fiCustomer_eMail fiCustomer_phone_number btnVoidTransaction 
         fiFilVoidTransaction tgSms btnRefundTransaction fiFilRefundTransaction 
         fiKOrdre_Id fiSesjonsId btnGetSessionDetails fiFilGetSessioonDetails 
         fiTransactionId btnGetTransaction fiFilGetTransaction 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getGetSessionDetails C-Win
PROCEDURE getGetSessionDetails:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE aArray1       AS CHARACTER EXTENT NO-UNDO.
  DEFINE VARIABLE aArray2       AS CHARACTER EXTENT NO-UNDO.
  
  EMPTY TEMP-TABLE ttSession.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
      cParametre = 'id=' + fiSesjonsId:SCREEN-VALUE 
      .
    IF cParametre = '' THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Sesjonsid er ikke angitt. Forespørsel avbrutt.").
      RETURN.
    END.
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'getGetSessionDetails|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved henting av sesjons detlajer: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ENTRY(3,pcReturnValue,'|')
      cJSonFileName = ENTRY(4,pcReturnValue,'|')
      cJSonString   = ENTRY(5,pcReturnValue,'|')
      .      

    /* Henter ut transaction_id fra respons meldingen. */
    /* Legger resultatet i ttSession.                  */
    IF iStatusCode   = 200 THEN
    GetTRANSACTION_ID: 
    DO:
      fiTransactionId:SCREEN-VALUE = ''.
      oParser = NEW ObjectModelParser().
      /* Endrer ytre object slik at det blir et object, istedenfor en array. */
      cJSonString = '~{"session": ' + cJSonString + '~}'.
      oJsonObject = CAST(oParser:Parse(cJSonString), JsonObject).
      aArray1 = oJsonObject:GetNames(). /* Denne har 1 ekstent i denne meldingen. */
      YTRELOOP:
      DO iLoopY = 1 TO EXTENT(aArray1):
        cTekst = oJsonObject:getJsonText(aArray1[iLoopY]).
        oJsonObject2 = CAST(oParser:Parse(cTekst), JsonObject) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        ARRAYBLOKK:
        DO:
          /* Gjør ingenting. */
        END. /* ARRAYBLOKK */
        ELSE
        OBJECTBLOKK: 
        DO:
          aArray2 = oJsonObject2:GetNames().
          DO iLoop = 1 TO EXTENT(aArray2):
            cTekst = oJsonObject2:getJsonText(aArray2[iLoop]).
            CASE aArray2[iLoop]:
              WHEN 'transaction_id' THEN fiTransactionId:SCREEN-VALUE = cTekst.              
            END CASE.
          END.
        END. /* OBJECTBLOKK */         
      END. /* YTRELOOP */
      DELETE OBJECT oParser NO-ERROR.      
    END. /* GetTRANSACTION_ID */
  END.
  ELSE DO:  
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ''
      cJSonFileName = ''
      cJSonString   = ''
      fiTransactionId:SCREEN-VALUE = ''
      .
  END.    

    ASSIGN 
      fiFilGetSessioonDetails:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeGetSessionDetails:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.


END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTokenProc C-Win 
PROCEDURE getTokenProc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
    
  ASSIGN 
    edToken_Entity:SCREEN-VALUE       = ''
    fiGetTokenStatusCode:SCREEN-VALUE = ''
    .
  
  IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                         'getToken'                                         
                                         ) THEN 
  DO: 
      JBoxSession:Instance:ViewMessage("Feil ved henting av token: " + JBoxServerAPI:Instance:getCallMessage()).
      RETURN.
  END.
  ELSE DO:
    ASSIGN 
      pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
      iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
      .
  END.
  
  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      fiGetTokenStatusCode:SCREEN-VALUE = ENTRY(1,pcReturnValue,'|')
      fiToken_StatusReason:SCREEN-VALUE = ENTRY(2,pcReturnValue,'|')
      fiAccess_Token:SCREEN-VALUE       = ENTRY(3,pcReturnValue,'|')
      fiExpires_in:SCREEN-VALUE         = ENTRY(4,pcReturnValue,'|')
      fitoken_type:SCREEN-VALUE         = ENTRY(5,pcReturnValue,'|')
      fiToken_Read:SCREEN-VALUE         = ENTRY(6,pcReturnValue,'|')
      edToken_Entity:SCREEN-VALUE       = ENTRY(7,pcReturnValue,'|')
      cToken                            = ENTRY(3,pcReturnValue,'|')
      cBearer                           = ENTRY(5,pcReturnValue,'|')
      .
  END.
  ELSE DO:  
    ASSIGN 
      fiGetTokenStatusCode:SCREEN-VALUE = ENTRY(1,pcReturnValue,'|')
      fiToken_StatusReason:SCREEN-VALUE = ENTRY(2,pcReturnValue,'|')
      fiAccess_Token:SCREEN-VALUE       = ''
      fiExpires_in:SCREEN-VALUE         = ''
      fitoken_type:SCREEN-VALUE         = ''
      fiToken_Read:SCREEN-VALUE         = ''
      edToken_Entity:SCREEN-VALUE       = ''
      cToken                            = ''
      cBearer                           = ''
      .
  END.    
    
  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTransaction C-Win 
PROCEDURE getTransaction :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
      cParametre = fiTransactionId:SCREEN-VALUE 
      .
    IF TRIM(fiTransactionId:SCREEN-VALUE) = '' THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Transaksjonsid mangler. Sesjon av brutt.").
      RETURN.
    END.
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'getTransaction|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved henting av transaksjon: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ENTRY(3,pcReturnValue,'|')
      cJSonFileName = ENTRY(4,pcReturnValue,'|')
      cJSonString   = ENTRY(5,pcReturnValue,'|')
      .
  END.
  ELSE DO:  
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ''
      cJSonFileName = ''
      cJSonString   = ''
      .
  END.    

    ASSIGN 
      fiFilGetTransaction:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeGetTransaction:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

DO WITH FRAME {&FRAME-NAME}:
  FIX-CODEPAGE(cJSonString) = 'UTF-8'.
  rClientDinTero = NEW cls.DinTero.ClientDinTero().

  /* Loggfil navn. */
  ASSIGN 
      rClientDinTero:cLogg       = 'ClientGetToken' + REPLACE(STRING(TODAY),'/','')
      rClientDinTero:cProtocoll  = 'https'    
      rClientDinTero:cUserDomain = 'tomcat'
      .
  
  oContainer:setNoResizeY("edToken_Entity,rectCreateSession,rectGetSessionDetails").
  oContainer:setNoResizeX("rectCreateSession,rectGetSessionDetails").       
END.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  oContainer:initResize().
&ELSE
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|ServerQuery.log," +   
                    "TransLogFile|ServerTrans.log").
&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE listProfilesProc C-Win 
PROCEDURE listProfilesProc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN 
      cParametre = 'limit=1&starting_after=T11112542.4XopX7PCwvdZzWxu5anRmd'
      cParametre = ''
      . 

    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'listProfiles|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved henting av token: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

    IF iStatusCode = 200 THEN
    DO: 
      ASSIGN 
        cStatusReason = ENTRY(2,pcReturnValue,'|')
        cType         = ENTRY(3,pcReturnValue,'|')
        cJSonFileName = ENTRY(4,pcReturnValue,'|')
        cJSonString   = ENTRY(5,pcReturnValue,'|')
        .
    END.
    ELSE DO:  
      ASSIGN 
        cStatusReason = ENTRY(2,pcReturnValue,'|')
        cType         = ''
        cJSonFileName = ''
        cJSonString   = ''
        .
    END.    

    ASSIGN 
      fiFilListProfiles:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeListProfiles:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE listSessionsProc C-Win 
PROCEDURE listSessionsProc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
      
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
      cParametre = 'limit=2&starting_after=T11112542.4XCQ9MC8L2pKYfpD1bUTMv' /* Lister 2 sesjon. */
      cParametre = 'limit=1&starting_after=T11112542.4XCURixn2G7vduzpJ8sSLL' /* Lister 1 sesjon. */
      cParametre = '' /* Lister alle sesjoner */
      cParametre = 'id=T11112542.4XCV3ZAJ5NaDjWX6Yj91EH'
      .
    
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'listSessions|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved henting av token: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ENTRY(3,pcReturnValue,'|')
      cJSonFileName = ENTRY(4,pcReturnValue,'|')
      cJSonString   = ENTRY(5,pcReturnValue,'|')
      .
  END.
  ELSE DO:  
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ''
      cJSonFileName = ''
      cJSonString   = ''
      .
  END.    

    ASSIGN 
      fiFilListSessions:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeListSessions:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE listTransactionsProc C-Win 
PROCEDURE listTransactionsProc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
      cParametre = '' /* Lister alle sesjoner */
      cParametre = 'id=T11112542.4XCUWUfU8f2xfzwxrHswKH'
      .
    
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'listTransactions|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved henting av transaksjonsliste: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ENTRY(3,pcReturnValue,'|')
      cJSonFileName = ENTRY(4,pcReturnValue,'|')
      cJSonString   = ENTRY(5,pcReturnValue,'|')
      .
  END.
  ELSE DO:  
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ''
      cJSonFileName = ''
      cJSonString   = ''
      .
  END.    

    ASSIGN 
      fiFilListTransactions:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeListTransactions:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"advGuiWin") = "YES" THEN
  RUN ShowForm("").
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refundTransaction C-Win 
PROCEDURE refundTransaction :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
      cParametre = ''
      cParametre = 'id=T11112542.4XCXv4JPTyKff182nRCFaD'
      .
    
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'refundTransaction|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved retur av transaksjon: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ENTRY(3,pcReturnValue,'|')
      cJSonFileName = ENTRY(4,pcReturnValue,'|')
      cJSonString   = ENTRY(5,pcReturnValue,'|')
      .
  END.
  ELSE DO:  
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ''
      cJSonFileName = ''
      cJSonString   = ''
      .
  END.    

    ASSIGN 
      fiFilVoidTransaction:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeReturTransaction:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE voidTransaction C-Win 
PROCEDURE voidTransaction :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcReturnValue AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN 
      cParametre = ''
      cParametre = 'id=T11112542.4XCUWUfU8f2xfzwxrHswKH'
      .
    
    IF NOT JBoxServerApi:Instance:CallServerProc("cls\DinTero\asDinTero.p",  
                                           'voidTransaction|' + cToken + '|' + cBearer + '|' + cParametre                                         
                                           ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved voiding av transaksjon: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    ELSE DO:
      ASSIGN 
        pcReturnValue = JBoxServerApi:Instance:getCallReturnParam().
        iStatusCode   = INT(ENTRY(1,pcReturnValue,'|'))
        .
    END.

  IF iStatusCode = 200 THEN
  DO: 
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ENTRY(3,pcReturnValue,'|')
      cJSonFileName = ENTRY(4,pcReturnValue,'|')
      cJSonString   = ENTRY(5,pcReturnValue,'|')
      .
  END.
  ELSE DO:  
    ASSIGN 
      cStatusReason = ENTRY(2,pcReturnValue,'|')
      cType         = ''
      cJSonFileName = ''
      cJSonString   = ''
      .
  END.    

    ASSIGN 
      fiFilVoidTransaction:SCREEN-VALUE = cJSonFileName
      .
   
    MESSAGE 'ResultatkodeVoidTransaction:' iStatusCode cStatusReason cType cJSonFileName SKIP(1)
    STRING(cJSonString)
    VIEW-AS ALERT-BOX.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

