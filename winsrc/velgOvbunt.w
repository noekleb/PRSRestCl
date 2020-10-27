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
DEFINE OUTPUT PARAMETER cTekst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER iMottagendebut AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ibutNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cbButikk slOvbunt BtnDone BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS cbButikk slOvbunt 

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

DEFINE VARIABLE cbButikk AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 20 
     LABEL "Mottagende butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "eCom lager",20,
                     "eCom overskuddslager",50
     DROP-DOWN-LIST
     SIZE 36.4 BY 1 NO-UNDO.

DEFINE VARIABLE slOvbunt AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "10"," But 1",
                     "40"," but 2" 
     SIZE 58 BY 3.57 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbButikk AT ROW 5.76 COL 23.2 COLON-ALIGNED WIDGET-ID 30
     slOvbunt AT ROW 1.48 COL 4 NO-LABEL WIDGET-ID 28
     BtnDone AT ROW 7.57 COL 47.4 WIDGET-ID 26
     BtnCancel AT ROW 7.57 COL 3 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 62.8 BY 11.38
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
         TITLE              = "Velg kampanje varene skal sendes til"
         HEIGHT             = 8
         WIDTH              = 63.4
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 62.8.

ASSIGN 
       cbButikk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "20".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Velg kampanje varene skal sendes til */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Velg kampanje varene skal sendes til */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
    ASSIGN 
      iButNr = 0.  
  
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
    ASSIGN
      cTekst = slOvBunt:SCREEN-VALUE IN FRAME DEFAULT-FRAME
      iMottagendebut = INT(cbButikk:SCREEN-VALUE)
      .
    IF cTekst = ? OR cTekst = '' THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Overføring er ikke valgt!").      
      RETURN NO-APPLY.
    END.
    ELSE 
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbButikk C-Win
ON VALUE-CHANGED OF cbButikk IN FRAME DEFAULT-FRAME /* Mottagende butikk */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slOvbunt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slOvbunt C-Win
ON VALUE-CHANGED OF slOvbunt IN FRAME DEFAULT-FRAME
DO:
 iMottagendebut = 0.
 IF JBoxServerApi:Instance:CallServerProc("ovbunt_Hent_tilBut.p",slOvbunt:SCREEN-VALUE) THEN
  DO:  
    iMottagendebut = INT(JBoxServerApi:Instance:getCallReturnParam()).
    IF iMottagendebut > 0 THEN 
    DO:
      ASSIGN 
        cbButikk:SCREEN-VALUE = STRING(iMottagendebut)
        cbButikk:SENSITIVE    = FALSE
        .
    END.
    ELSE 
      cbButikk:SENSITIVE    = TRUE.
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
  DISPLAY cbButikk slOvbunt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cbButikk slOvbunt BtnDone BtnCancel 
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
    ASSIGN 
      cTekst   = ''
      slOvBunt:DELIMITER = "|"
      slOvBunt = ''
      .
      
    slOvBunt:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                "Ovbunt;BuntNr|Merknad;BuntNr",
                                                "WHERE OvBunt.DatoOppdatert = '?'"
                                               ).                                               
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

