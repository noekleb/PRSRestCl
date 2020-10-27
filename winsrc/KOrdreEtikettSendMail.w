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
DEFINE INPUT PARAMETER plKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER pcEkstOrdreNr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER plKundeNr AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER pcNavn AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER pcePostAdresse AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcEmne AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pcTekst AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiePostAdresse fiEmne fiTekst ~
BtnCancel BtnDone 
&Scoped-Define DISPLAYED-OBJECTS fiKOrdre_Id fiKundeNr fiNavn fiEkstOrdreNr ~
fiePostAdresse fiEmne fiTekst 

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
     LABEL "&Send" 
     SIZE 15 BY 1.14 TOOLTIP "Sender eMail med returetikett som vedlagt pdf fil."
     BGCOLOR 8 .

DEFINE VARIABLE fiTekst AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 81 BY 9.05 TOOLTIP "Tekst som legges som meldingstekst i eMail." NO-UNDO.

DEFINE VARIABLE fiEkstOrdreNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksternt ordrenr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiEmne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 81.2 BY 1 TOOLTIP "eMail overskrift/emne." NO-UNDO.

DEFINE VARIABLE fiePostAdresse AS CHARACTER FORMAT "X(256)":U 
     LABEL "ePost adresse" 
     VIEW-AS FILL-IN 
     SIZE 81.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiKOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "OrdreId" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE fiKundeNr AS DECIMAL FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 36.2 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 15.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiKOrdre_Id AT ROW 2.19 COL 17 COLON-ALIGNED
     fiKundeNr AT ROW 2.19 COL 62 COLON-ALIGNED
     fiNavn AT ROW 3.24 COL 62 COLON-ALIGNED
     fiEkstOrdreNr AT ROW 3.29 COL 17 COLON-ALIGNED
     fiePostAdresse AT ROW 4.43 COL 17 COLON-ALIGNED WIDGET-ID 20
     fiEmne AT ROW 5.52 COL 17 COLON-ALIGNED
     fiTekst AT ROW 6.71 COL 19.2 NO-LABEL WIDGET-ID 6
     BtnCancel AT ROW 16.95 COL 2.4 WIDGET-ID 4
     BtnDone AT ROW 16.95 COL 90 WIDGET-ID 2
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 107.2 BY 17.76
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
         TITLE              = "Send etikett via eMail"
         HEIGHT             = 17.76
         WIDTH              = 107.2
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
       FRAME DEFAULT-FRAME:HEIGHT           = 17.76
       FRAME DEFAULT-FRAME:WIDTH            = 107.2.

/* SETTINGS FOR FILL-IN fiEkstOrdreNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiEkstOrdreNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiKOrdre_Id IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiKOrdre_Id:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiKundeNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiKundeNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiNavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiNavn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       fiTekst:AUTO-INDENT IN FRAME DEFAULT-FRAME      = TRUE
       fiTekst:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Send etikett via eMail */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Send etikett via eMail */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Send */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
    ASSIGN 
      pcEmne  = fiEmne:SCREEN-VALUE 
      pcTekst = fiTekst:SCREEN-VALUE  
      pcePostAdresse = fiePostAdresse:SCREEN-VALUE
      pbOk    = TRUE
      .
      
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
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
  DISPLAY fiKOrdre_Id fiKundeNr fiNavn fiEkstOrdreNr fiePostAdresse fiEmne 
          fiTekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 fiePostAdresse fiEmne fiTekst BtnCancel BtnDone 
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
    pcEmne        = 'Retur etikett og pakkseddel for ordre ' + pcEkstOrdreNr + '.'
    pcTekst       = "Bruk vedlagte retur etikett for utskrift. Fest den utskrevne etikett på pakken som returneres." + CHR(10) + 
                    "Vedlagt er også utskrift av pakkseddel." +  
                    CHR(10) + 
                    CHR(10) + 
                    "Merk: Dette er en automatisk generert epost. Svar til avsender i denne eposten blir ikke behandlet." + CHR(10) +
                    CHR(10) +  
                    "Benytt epostadresse 'kundeservice@gant.no' FOR henvendelser til kundeservice." + CHR(10) + 
                    CHR(10) +  
                    "Med vennlig hilsen" + CHR(10) + 
                    "GANT Retail AS"
    fiePostAdresse = pcePostAdresse 
    fiKOrdre_Id   = plKOrdre_Id
    fiEkstOrdreNr = pcEkstOrdreNr
    fiKundeNr     = plKundeNr 
    fiNavn        = pcNavn
    fiEmne        = pcEmne
    fitekst       = pcTekst
    .
  DISPLAY 
    fiKOrdre_Id   
    fiEkstOrdreNr 
    fiKundeNr     
    fiNavn        
    fiEmne        
    fitekst       
    fiePostAdresse
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

