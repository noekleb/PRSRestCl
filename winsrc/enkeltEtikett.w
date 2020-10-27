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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&IF "{1}" = "Developer_Studio_is_Running" &THEN
  &SCOPED-DEFINE UIB_is_Running 1 
&ENDIF   
/* Uncomment to enable use of .Net components: */
/* &SCOPED-DEFINE AdvGuiWin */ 

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER cKode AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lLagant AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER iButNr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iKampanjeId AS INTEGER NO-UNDO.
 
/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterLabelLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iBrukerBut AS INTEGER NO-UNDO.
DEFINE VARIABLE ibeLayout AS INTEGER NO-UNDO.
DEFINE VARIABLE iRFIDEtikett AS INTEGER NO-UNDO.
DEFINE VARIABLE iAltBeLayout AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 BtnDone cbPrinter BtnCancel ~
fcStrekkode tgAlle tgLager iAntEti iStartEtikett rsPris rsEtitype cbButiker 
&Scoped-Define DISPLAYED-OBJECTS cbPrinter fcStrekkode tgAlle tgLager ~
iAntEti iStartEtikett rsPris rsEtitype cbButiker 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Ok" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cbButiker AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 33 BY 1 TOOLTIP "Butikken etikettene skal skrives ut i." NO-UNDO.

DEFINE VARIABLE cbPrinter AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Skriver" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 33 BY 1 TOOLTIP "Etikettens layout" NO-UNDO.

DEFINE VARIABLE fcStrekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 33 BY 1 TOOLTIP "Strekkode det skal skrives ut etikett på" NO-UNDO.

DEFINE VARIABLE iAntEti AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Antall etiketter" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Antall etiketter som skal skrives ut." NO-UNDO.

DEFINE VARIABLE iStartEtikett AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Start etikett (1 - 24)" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE rsEtitype AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "RFID etikett", 1,
"Vanlig etikett", 2
     SIZE 17.2 BY 1.91 TOOLTIP "Overstyring av standard etikett type." NO-UNDO.

DEFINE VARIABLE rsPris AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Normalpris", 1,
"Aktuell pris", 2
     SIZE 14.8 BY 1.91 TOOLTIP "Hvilken pris skal skrives på etiketten." NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 10.48.

DEFINE VARIABLE tgAlle AS LOGICAL INITIAL NO 
     LABEL "Alle EAN/UPC" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 TOOLTIP "Skal etikett for alle strekkoder for størrelsen skrives ut?" NO-UNDO.

DEFINE VARIABLE tgLager AS LOGICAL INITIAL NO 
     LABEL "Lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 TOOLTIP "Skal antall på lager bestemme antall etiketter?" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BtnDone AT ROW 1.57 COL 59.6 WIDGET-ID 22
     cbPrinter AT ROW 1.95 COL 22 COLON-ALIGNED WIDGET-ID 2
     BtnCancel AT ROW 2.86 COL 59.6 WIDGET-ID 24
     fcStrekkode AT ROW 3.05 COL 22 COLON-ALIGNED
     tgAlle AT ROW 4.33 COL 24 WIDGET-ID 6
     tgLager AT ROW 5.29 COL 24 WIDGET-ID 8
     iAntEti AT ROW 6.24 COL 22 COLON-ALIGNED
     iStartEtikett AT ROW 7.43 COL 22 COLON-ALIGNED
     rsPris AT ROW 8.62 COL 24.2 NO-LABEL WIDGET-ID 14
     rsEtitype AT ROW 8.62 COL 39.8 NO-LABEL WIDGET-ID 26
     cbButiker AT ROW 10.62 COL 22 COLON-ALIGNED WIDGET-ID 18
     RECT-8 AT ROW 1.48 COL 4 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 74 BY 11.38
         DEFAULT-BUTTON BtnDone CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
         TITLE              = "Etikettutskrift"
         HEIGHT             = 11.33
         WIDTH              = 74.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 74.

ASSIGN 
       fcStrekkode:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Etikettutskrift */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Etikettutskrift */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Ok */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
    IF INT(iAntEti:SCREEN-VALUE) = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Antall etiketter må være større enn 0! ").
        RETURN NO-APPLY.
      END.  
    IF INT(iStartEtikett:SCREEN-VALUE) = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Start etikett må være større enn 0! ").
        RETURN NO-APPLY.
      END.  
    IF INT(cbButiker:SCREEN-VALUE) = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Butikk må angis! ").
        RETURN NO-APPLY.
      END.  
    IF TRIM(fcStrekkode:SCREEN-VALUE) = '' THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Strekkode må angis! ").
        RETURN NO-APPLY.
      END.  
    ASSIGN 
      cTekst = ENTRY(LOOKUP(cbPrinter:SCREEN-VALUE,cbPrinter:LIST-ITEMS),cPrinterIdLst) + '|' +
               fcStrekkode:SCREEN-VALUE + '|' + 
               tgAlle:SCREEN-VALUE + '|' + 
               tgLager:SCREEN-VALUE + '|' + 
               iAntEti:SCREEN-VALUE + '|' + 
               iStartEtikett:SCREEN-VALUE + '|' + 
               rsPris:SCREEN-VALUE + '|' +  
               cbButiker:SCREEN-VALUE + '|' +
               STRING(iKampanjeId) + '|' +  
               JBoxSession:Instance:UserId + '|' +
               STRING(rsEtitype) + '|' +
               STRING(iAltBeLayout) + '|' +
               STRING(iRFIDEtikett)               
               .    
       
    IF NOT JBoxServerApi:Instance:CallServerProc("etikett_enkel.p",cTekst) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved etikett utskrift: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsEtitype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsEtitype C-Win
ON VALUE-CHANGED OF rsEtitype IN FRAME DEFAULT-FRAME
DO:
  
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
  DISPLAY cbPrinter fcStrekkode tgAlle tgLager iAntEti iStartEtikett rsPris 
          rsEtitype cbButiker 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-8 BtnDone cbPrinter BtnCancel fcStrekkode tgAlle tgLager iAntEti 
         iStartEtikett rsPris rsEtitype cbButiker 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
  IF JBoxServerAPI:Instance:Find("Bruker", "WHERE BrukerId = '" + JBoxSession:Instance:UserId + "'") THEN
    ASSIGN
      iBrukerBut = INT(JBoxServerAPI:Instance:FieldValue("Bruker.ButikkNr"))
      . 
  IF JBoxServerAPI:Instance:Find("Butiker", "WHERE Butik = '" + JBoxServerAPI:Instance:FieldValue("Bruker.ButikkNr") + "'") THEN
    ASSIGN
      ibeLayout = INT(JBoxServerAPI:Instance:FieldValue("Butiker.beLayout"))
      . 
  /* Dette er samme sjekk som gjøres fra kassen i asRFIDEtikett.p. Styrer setting av etiketttype. */
  IF JBoxServerAPI:Instance:Find("Syspara", "WHERE SysHId = 5 and SysGr = 20 and ParaNr = '" + STRING(ibeLayout) + "'") THEN
  DO:
    ASSIGN
      cTekst       = JBoxServerAPI:Instance:FieldValue("SysPara.Beskrivelse")
      . 
    IF cTekst MATCHES '*RFID*' THEN
    DO: 
      iRFIDEtikett = 1.
      iAltBeLayout = INT(JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1")) NO-ERROR.
    END.
    ELSE 
      iRFIDEtikett = 2.
  END.
  ELSE 
    iRFIDEtikett = 2.
  
  cPrinterName = DYNAMIC-FUNCTION("getFieldList",
                            "SysPara;ParaNr|Parameter2;ParaNr",
                            "WHERE SysHId = 5 and sysGr = 21 and Parameter2 begins 'AVAIL'"
                           ).                                               
  ASSIGN 
    cPrinterIdLst    = ''
    cPrinterLst      = ''
    cPrinterLabelLst = ''
    .
  DO iLoop = 1 TO NUM-ENTRIES(cPrinterName,'|'):
    IF iLoop MODULO 2 = 0 THEN
    DO: 
      cPrinterIdLst = cPrinterIdLst + 
                      (IF cPrinterIdLst <> '' THEN ',' ELSE '') + 
                      ENTRY(iLoop,cPrinterName,'|').
                      
      IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 5 and SysGr = 20 and ParaNr = '" + ENTRY(iLoop,cPrinterName,'|') + "'") THEN
        ASSIGN 
          cPrinterLabelLst = cPrinterLabelLst + 
                            (IF cPrinterLabelLst <> '' THEN ',' ELSE '') + 
                             JBoxServerAPI:Instance:FieldValue("SysPara.Parameter2").                      
    END.
    ELSE DO:
      cPrinterLst = cPrinterLst + 
                    (IF cPrinterIdLst <> '' THEN ',' ELSE '') +
                    (IF NUM-ENTRIES(ENTRY(iLoop,cPrinterName,'|'),'=') = 2 
                      THEN ENTRY(2,ENTRY(iLoop,cPrinterName,'|'),'=') 
                      ELSE '').
    END.
  END.
  
  ASSIGN 
    cbButiker:DELIMITER = "|"
    cbPrinter:DELIMITER = ',' 
    .
    
  cbPrinter:LIST-ITEMS = cPrinterLabelLst.
  cbButiker:LIST-ITEM-PAIRS = '0|0|' + DYNAMIC-FUNCTION("getFieldList",
                                              "Butiker;Butik|ButNamn;Butik",
                                              "WHERE Butiker.HarButikkSystem = 'TRUE'"
                                             ).
                                             
  ASSIGN 
    cbPrinter     = ENTRY(LOOKUP(STRING(ibeLayout),cPrinterIdLst),cPrinterLabelLst)
    cbPrinter:SENSITIVE = FALSE 
    cbButiker     = IF iBrukerBut = 0 THEN 0 ELSE 
                      iButNr /* Er bruker satt opp mot butikk 0, skal han få velge butikk. */
    cbButiker:SENSITIVE = IF iBrukerBut = 0 THEN TRUE ELSE FALSE
    rsEtitype     = iRFIDEtikett
    rsEtiType:SENSITIVE = IF iRFIDEtikett = 1 THEN TRUE ELSE FALSE
    rsEtiType:HIDDEN    = IF iRFIDEtikett = 1 THEN FALSE ELSE TRUE
    fcStrekkode   = cKode 
    iStartEtikett = 1
    iAntEti       = 1
    rsPris        = 1
    NO-ERROR.
  IF iKampanjeId > 0 THEN 
  DO:
    ASSIGN 
      tgLager = TRUE
      tgLager:SENSITIVE = FALSE 
      iAntEti:SENSITIVE = FALSE 
      . 
  END.
  DISPLAY 
    cbPrinter
    cbButiker         
    fcStrekkode    
    iStartEtikett 
    iAntEti       
    rsPris        
    tgLager 
    rsEtiType WHEN iRFIDEtikett = 1
    .    
    
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

