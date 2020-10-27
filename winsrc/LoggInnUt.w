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
DEFINE INPUT  PARAMETER cTTId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOk AS LOG INITIAL FALSE NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn AS CHARACTER NO-UNDO.


/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bOk         AS LOG    NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBrowse     AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
DEFINE VARIABLE hToolbar    AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 fiAnsattNr btnfiAnsattNr fiMerknad ~
BtnCancel BtnDone 
&Scoped-Define DISPLAYED-OBJECTS fiAnsattNr fiNavn fiMerknad fitekst 

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
     LABEL "&Lagre" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnfiAnsattNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE fiAnsattNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ansattnr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "Ansattnummer" NO-UNDO.

DEFINE VARIABLE fiMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 55 BY 1 TOOLTIP "Kort merknad til innloggingen." NO-UNDO.

DEFINE VARIABLE fiNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 36.4 BY 1 TOOLTIP "Navn på ansatt." NO-UNDO.

DEFINE VARIABLE fitekst AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 26.2 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 3.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiAnsattNr AT ROW 3.14 COL 13.4 COLON-ALIGNED
     btnfiAnsattNr AT ROW 3.14 COL 29.6 WIDGET-ID 24 NO-TAB-STOP 
     fiNavn AT ROW 3.14 COL 32 COLON-ALIGNED NO-LABEL
     fiMerknad AT ROW 4.33 COL 13.4 COLON-ALIGNED
     BtnCancel AT ROW 6.33 COL 2 WIDGET-ID 16
     BtnDone AT ROW 6.33 COL 60.8 WIDGET-ID 20
     fitekst AT ROW 1.33 COL 25.4 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 2.43 COL 2 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 76.4 BY 11.38
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
         TITLE              = "Innlogging"
         HEIGHT             = 6.67
         WIDTH              = 76.4
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
       FRAME DEFAULT-FRAME:WIDTH            = 76.4.

/* SETTINGS FOR FILL-IN fiNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fitekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Innlogging */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  
  IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal registrering avbrytes?") THEN 
      RETURN NO-APPLY.
  
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Innlogging */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Lagre */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
    IF TRIM(fiAnsattNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME) = '' OR JBoxServerAPI:Instance:FieldValues("Selger","WHERE AnsattNr = '" + fiAnsattNr:SCREEN-VALUE + "'","ForNavn") = ? THEN 
    DO:
      JBoxSession:Instance:ViewMessage('Ukjent ansatt (' + fiAnsattNr:SCREEN-VALUE + ').').
      APPLY 'ENTRY' TO fiAnsattNr IN FRAME DEFAULT-FRAME.
      RETURN NO-APPLY.
    END.
    ELSE DO: 
      ASSIGN 
        obOk     = TRUE
        ocReturn = fiAnsattNr:SCREEN-VALUE + '|' + fiMerknad:SCREEN-VALUE + '|' + cTTId.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
    END.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfiAnsattNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfiAnsattNr C-Win
ON CHOOSE OF btnfiAnsattNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  /* To manipulate the lookup query object add the callback procedure hook "myLookupObject" 
     (probably use "WHERE false" as the initial query in this case) */
  
  JBoxServerAPI:Instance:Lookup("Selger"
                    + ";AnsattNr"
                    + ";ForNavn;Navn"
                   ,"WHERE Selger.AnsattNr > '' "
                   ,"AnsattNr,ForNavn,Navn"   /* <- return values for these fields */
                    ).

  IF JBoxServerAPI:Instance:LookupOk THEN 
  DO:
    ASSIGN       
      cTekst = JBoxServerAPI:Instance:LookupValue("ForNavn") + ' ' + JBoxServerAPI:Instance:LookupValue("Navn")
      fiAnsattNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = JBoxServerAPI:Instance:LookupValue("AnsattNr")
      fiNavn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = cTekst
      .
    APPLY "ENTRY" TO fiMerknad.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAnsattNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAnsattNr C-Win
ON LEAVE OF fiAnsattNr IN FRAME DEFAULT-FRAME /* Ansattnr */
DO:
  DO WITH FRAME DEFAULT-FRAME:
    IF JBoxServerAPI:Instance:FieldValues("Selger","WHERE AnsattNr = '" + fiAnsattNr:SCREEN-VALUE + "'","ForNavn") = ? THEN 
    DO:
      JBoxSession:Instance:ViewMessage('Ukjent ansatt (' + fiAnsattNr:SCREEN-VALUE + ').').
      APPLY 'ENTRY' TO fiAnsattNr IN FRAME DEFAULT-FRAME.
      RETURN NO-APPLY.
    END.
    ELSE IF fiAnsattNr:SCREEN-VALUE = '' THEN
    DO:
    END.
    ELSE DO:
      ASSIGN 
        cTekst = JBoxServerAPI:Instance:FieldValues("Selger","WHERE AnsattNr = '" + fiAnsattNr:SCREEN-VALUE + "'","ForNavn")
        cTekst = cTekst + ' ' + JBoxServerAPI:Instance:FieldValues("Selger","WHERE AnsattNr = '" + fiAnsattNr:SCREEN-VALUE + "'","Navn")
        .
      fiNavn:SCREEN-VALUE IN FRAME DEFAULT-FRAME = cTekst.
      APPLY 'ENTRY' TO fiMerknad IN FRAME DEFAULT-FRAME.
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAnsattNr C-Win
ON RETURN OF fiAnsattNr IN FRAME DEFAULT-FRAME /* Ansattnr */
DO:
  APPLY 'TAB' TO fiAnsattNr IN FRAME DEFAULT-FRAME.
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
  DISPLAY fiAnsattNr fiNavn fiMerknad fitekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 fiAnsattNr btnfiAnsattNr fiMerknad BtnCancel BtnDone 
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
    fitekst:SCREEN-VALUE = IF cTTID = '96' THEN 'INNLOGGING' ELSE 'UTLOGGING'
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

