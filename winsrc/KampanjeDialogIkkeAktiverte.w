&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-KampanjeDialogIkkeAktiverte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-KampanjeDialogIkkeAktiverte 
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
DEFINE OUTPUT PARAMETER obOk AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER ocVgLst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocVmLst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obAktive AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE cVgRowIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVgLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVmRowIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVmLst AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 btnUtvalgHovedKategori fiVgLst ~
btnUtvalgVaremerke fiVmIdLst TGAktive edEditor BtnCancel BtnDone 
&Scoped-Define DISPLAYED-OBJECTS fiVgLst fiVmIdLst TGAktive edEditor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-KampanjeDialogIkkeAktiverte AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Ok" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnUtvalgHovedKategori 
     LABEL "Hovedkategori" 
     SIZE 20 BY 1.1.

DEFINE BUTTON btnUtvalgVaremerke 
     LABEL "Varemerke" 
     SIZE 20 BY 1.1.

DEFINE VARIABLE edEditor AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 71.4 BY 3.57 NO-UNDO.

DEFINE VARIABLE fiVgLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hovedkategori" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med hovedkategorier"
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fiVmIdLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varemerke lst" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med varemerker"
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 71.4 BY 4.05.

DEFINE VARIABLE TGAktive AS LOGICAL INITIAL no 
     LABEL "Ta med også artikler som står aktive på kampanje" 
     VIEW-AS TOGGLE-BOX
     SIZE 52.8 BY .81 TOOLTIP "Åpner for at også aktive artikler aktive på kampanje legges inn" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnUtvalgHovedKategori AT ROW 3.24 COL 44.2 WIDGET-ID 76
     fiVgLst AT ROW 3.29 COL 17.2 COLON-ALIGNED
     btnUtvalgVaremerke AT ROW 4.33 COL 44.2 WIDGET-ID 80
     fiVmIdLst AT ROW 4.38 COL 17.2 COLON-ALIGNED
     TGAktive AT ROW 5.52 COL 19.2 WIDGET-ID 96
     edEditor AT ROW 6.86 COL 1.6 NO-LABEL WIDGET-ID 90
     BtnCancel AT ROW 10.91 COL 2.2 WIDGET-ID 94
     BtnDone AT ROW 11 COL 57.6 WIDGET-ID 92
     "Filter" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.81 COL 3 WIDGET-ID 88
          FONT 6
     RECT-8 AT ROW 2.48 COL 1.6 WIDGET-ID 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 72.8 BY 11.38
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
  CREATE WINDOW C-KampanjeDialogIkkeAktiverte ASSIGN
         HIDDEN             = YES
         TITLE              = "Filter utvalg ikke aktiverte varer"
         HEIGHT             = 11.33
         WIDTH              = 73.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-KampanjeDialogIkkeAktiverte 
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
/* SETTINGS FOR WINDOW C-KampanjeDialogIkkeAktiverte
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 72.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KampanjeDialogIkkeAktiverte)
THEN C-KampanjeDialogIkkeAktiverte:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-KampanjeDialogIkkeAktiverte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KampanjeDialogIkkeAktiverte C-KampanjeDialogIkkeAktiverte
ON END-ERROR OF C-KampanjeDialogIkkeAktiverte /* Filter utvalg ikke aktiverte varer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-KampanjeDialogIkkeAktiverte C-KampanjeDialogIkkeAktiverte
ON WINDOW-CLOSE OF C-KampanjeDialogIkkeAktiverte /* Filter utvalg ikke aktiverte varer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-KampanjeDialogIkkeAktiverte
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
    ASSIGN
      ocVgLst = '' 
      ocVmLst = '' 
      obOk    = FALSE
      .
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-KampanjeDialogIkkeAktiverte
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
        ocVgLst  = cVgLst 
        ocVmLst  = cVmLst
        obAktive = IF TGAktive:SCREEN-VALUE = 'yes' THEN TRUE ELSE FALSE
        obOk     = TRUE
        .
        
      /* This event will close the window and terminate the procedure.  */
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgHovedKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgHovedKategori C-KampanjeDialogIkkeAktiverte
ON CHOOSE OF btnUtvalgHovedKategori IN FRAME DEFAULT-FRAME /* Hovedkategori */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Hovedkategori;HovedkatNr||>>>>>>>9;HovedKatTekst", 
                      "WHERE true",
                      INPUT-OUTPUT cVgRowIdLst,
                      "HovedKatNr",
                      INPUT-OUTPUT cVgLst,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiVgLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cVgLst,'|'))
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVaremerke C-KampanjeDialogIkkeAktiverte
ON CHOOSE OF btnUtvalgVaremerke IN FRAME DEFAULT-FRAME /* Varemerke */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Varemerke;VmId||>>>>>9;Beskrivelse",
                      "WHERE true",
                      INPUT-OUTPUT cVmRowIdLst,
                      "VmId",
                      INPUT-OUTPUT cVmLst,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiVmIdLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cVmLst,'|'))
    .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-KampanjeDialogIkkeAktiverte 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-KampanjeDialogIkkeAktiverte  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-KampanjeDialogIkkeAktiverte)
  THEN DELETE WIDGET C-KampanjeDialogIkkeAktiverte.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-KampanjeDialogIkkeAktiverte  _DEFAULT-ENABLE
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
  DISPLAY fiVgLst fiVmIdLst TGAktive edEditor 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KampanjeDialogIkkeAktiverte.
  ENABLE RECT-8 btnUtvalgHovedKategori fiVgLst btnUtvalgVaremerke fiVmIdLst 
         TGAktive edEditor BtnCancel BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-KampanjeDialogIkkeAktiverte.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-KampanjeDialogIkkeAktiverte 
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
  edEditor:SCREEN-VALUE = "NB1: Artikklene legges til i kampanjen. Er kampanjen aktivert og aktiv, vil artikklene som legges inn bli aktivert direkte." + CHR(10) + 
                          "NB2: Velges at også artikler som står aktive på kampanje skal tas med, legges disse inn bare på ikke aktverte kampanjer.".

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-KampanjeDialogIkkeAktiverte 
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

