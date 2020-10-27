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
DEFINE INPUT-OUTPUT PARAMETER cBeskrivelse AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER plRab% AS DECIMAL FORMAT "->>9.9" NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER dStartDato AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iAktiveresTid AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER dSluttDato AS DATE NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iGyldigTiltid AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER cNotat AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER bIgnoreNOS AS LOG NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lKampanjePris AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lKroneRabatt AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lMinstepris AS DECIMAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iAvslagType AS DECIMAL NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-1 fiBeskrivelse fiRab% AvslagType ~
KampanjePris KroneRabatt Minstepris tgIgnoreNOS fiStartDato cbAktiveresTid ~
fiSluttDato cbGyldigTilTid BtnCancel BtnOK 
&Scoped-Define DISPLAYED-OBJECTS NormalPris fiBeskrivelse fiRab% AvslagType ~
KampanjePris KroneRabatt Minstepris tgIgnoreNOS fiStartDato cbAktiveresTid ~
fiSluttDato cbGyldigTilTid 

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

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE cbAktiveresTid AS CHARACTER INITIAL "00:00" 
     VIEW-AS COMBO-BOX INNER-LINES 50
     LIST-ITEMS "00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","23:59" 
     DROP-DOWN
     SIZE 11 BY 1 TOOLTIP "Tidspunkt for aktivering av kampanje." NO-UNDO.

DEFINE VARIABLE cbGyldigTilTid AS CHARACTER INITIAL "23:59" 
     VIEW-AS COMBO-BOX INNER-LINES 50
     LIST-ITEMS "00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","23:59" 
     DROP-DOWN
     SIZE 11 BY 1 TOOLTIP "Tidspunkt for deaktivering av kampanje." NO-UNDO.

DEFINE VARIABLE fiBeskrivelse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 79.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiRab% AS DECIMAL FORMAT ">9.9":U INITIAL 0 
     LABEL "Rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Ny rabatt%" NO-UNDO.

DEFINE VARIABLE fiSluttDato AS DATE FORMAT "99/99/99":U 
     LABEL "Slutt dato/tid" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Dato for avsluttning av kampanjen" NO-UNDO.

DEFINE VARIABLE fiStartDato AS DATE FORMAT "99/99/99":U 
     LABEL "Startdato/tid" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Dato for start av kampanjen." NO-UNDO.

DEFINE VARIABLE KampanjePris AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "KampanjePris" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Fast kampanjepris UANSETT førpris" NO-UNDO.

DEFINE VARIABLE KroneRabatt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "KroneRabatt" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Fast kronerabatt (Fra normalpris)" NO-UNDO.

DEFINE VARIABLE Minstepris AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Minstepris" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Minstepris på artikler ved kronerabatt" NO-UNDO.

DEFINE VARIABLE AvslagType AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Rabatt%", 1,
"Fastpris", 2,
"Kronerabatt", 3
     SIZE 17.6 BY 2.95 TOOLTIP "Velg type av rabatt" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 9.76.

DEFINE VARIABLE NormalPris AS LOGICAL INITIAL NO 
     LABEL "Normalpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.6 BY .81 TOOLTIP "Aktiveres denne, er det normalpris som endres. Ikke kampanjepris." NO-UNDO.

DEFINE VARIABLE tgIgnoreNOS AS LOGICAL INITIAL NO 
     LABEL "Ignore NOS" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 TOOLTIP "Ignorerer NOS merkede artikler ved aktivering." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     NormalPris AT ROW 2.62 COL 22.2 WIDGET-ID 94
     fiBeskrivelse AT ROW 3.57 COL 19.8 COLON-ALIGNED
     fiRab% AT ROW 4.67 COL 53.6 COLON-ALIGNED
     AvslagType AT ROW 4.81 COL 22.4 NO-LABEL WIDGET-ID 90 NO-TAB-STOP 
     KampanjePris AT ROW 5.76 COL 53.8 COLON-ALIGNED
     KroneRabatt AT ROW 6.86 COL 53.8 COLON-ALIGNED
     Minstepris AT ROW 6.86 COL 85 COLON-ALIGNED
     tgIgnoreNOS AT ROW 8.14 COL 22 WIDGET-ID 98
     fiStartDato AT ROW 9.24 COL 20 COLON-ALIGNED
     cbAktiveresTid AT ROW 9.24 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     fiSluttDato AT ROW 10.38 COL 20 COLON-ALIGNED
     cbGyldigTilTid AT ROW 10.38 COL 35.2 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     BtnCancel AT ROW 12.14 COL 3.4 WIDGET-ID 8
     BtnOK AT ROW 12.14 COL 91 WIDGET-ID 10
     "Kampanjeinformasjon" VIEW-AS TEXT
          SIZE 73.8 BY .62 AT ROW 1.57 COL 22.2 WIDGET-ID 12
          FONT 6
     RECT-1 AT ROW 2.19 COL 4 WIDGET-ID 96
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.4 BY 12.52
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel WIDGET-ID 100.


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
         TITLE              = "Kampanjerabatt"
         HEIGHT             = 12.52
         WIDTH              = 106.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 121
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 121
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       cbAktiveresTid:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "00:00".

ASSIGN 
       cbGyldigTilTid:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "23:59".

/* SETTINGS FOR TOGGLE-BOX NormalPris IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kampanjerabatt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kampanjerabatt */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AvslagType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AvslagType C-Win
ON VALUE-CHANGED OF AvslagType IN FRAME DEFAULT-FRAME
DO:
  RUN VisFelt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  ASSIGN 
    cBeskrivelse = 'AVBRYT'
    .  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  ASSIGN 
    plRab% = DEC(fiRab%:SCREEN-VALUE)
    cBeskrivelse = fiBeskrivelse:SCREEN-VALUE
    dStartDato = DATE(fiStartDato:SCREEN-VALUE) 
    dSluttDato = DATE(fiSluttDato:SCREEN-VALUE)
    iAktiveresTid = (INT(ENTRY(1,cbAktiveresTid:SCREEN-VALUE,':')) * 60 * 60) + (INT(ENTRY(2,cbAktiveresTid:SCREEN-VALUE,':')) * 60)  
    iGyldigtiltid = (INT(ENTRY(1,cbGyldigtiltid:SCREEN-VALUE,':')) * 60 * 60) + (INT(ENTRY(2,cbGyldigtiltid:SCREEN-VALUE,':')) * 60)  
    bIgnoreNOS = IF tgIgnoreNOS:SCREEN-VALUE = 'YES' THEN TRUE ELSE FALSE
    lKampanjePris = DEC(KampanjePris:SCREEN-VALUE)
    lKroneRabatt = DEC(KroneRabatt:SCREEN-VALUE)
    lMinstePris = DEC(MinstePris:SCREEN-VALUE)
    iAvslagtype = INT(AvslagType:SCREEN-VALUE)
    .
  IF dStartDato = ? OR dSluttDato = ? THEN 
    DO:
      JBoxSession:Instance:ViewMessage('Ugyldig datoangivelse!').
      RETURN NO-APPLY.  
    END.
  IF dStartDato > dSluttDato THEN 
    DO:
      JBoxSession:Instance:ViewMessage('Ugyldig datoangivelse - StartDato er større enn SluttDato!').
      RETURN NO-APPLY.  
    END.  
  IF cBeskrivelse = '' THEN 
    DO:
      JBoxSession:Instance:ViewMessage('Beskrivelse er ikke angitt!').
      RETURN NO-APPLY.  
    END.
    
  IF INT(AvslagType:SCREEN-VALUE) = 1 THEN
  RABATTYPE: 
  DO:
    IF plRab% = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage('Rabatt ikke angitt!').
        RETURN NO-APPLY.  
      END.
  END. /* RABATTYPE */
  ELSE IF INT(AvslagType:SCREEN-VALUE) = 2 THEN
  FASTPRISTYPE: 
  DO:
    IF DEC(KampanjePris:SCREEN-VALUE) = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage('Pris for kampanjen er ikke angitt!').
        RETURN NO-APPLY.  
      END.
  END. /* FASTPRISTYPE */
  IF INT(AvslagType:SCREEN-VALUE) = 3 THEN
  KRONERABATTYPE: 
  DO:
    IF DEC(Kronerabatt:SCREEN-VALUE) = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage('Kronerabatt ikke angitt!').
        RETURN NO-APPLY.  
      END.
    IF DEC(Minstepris:SCREEN-VALUE) = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage('Minstepris ikke angitt!').
        RETURN NO-APPLY.  
      END.
  END. /* KRONERABATTYPE */
    
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbAktiveresTid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbAktiveresTid C-Win
ON VALUE-CHANGED OF cbAktiveresTid IN FRAME DEFAULT-FRAME
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
  DISPLAY NormalPris fiBeskrivelse fiRab% AvslagType KampanjePris KroneRabatt 
          Minstepris tgIgnoreNOS fiStartDato cbAktiveresTid fiSluttDato 
          cbGyldigTilTid 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 fiBeskrivelse fiRab% AvslagType KampanjePris KroneRabatt 
         Minstepris tgIgnoreNOS fiStartDato cbAktiveresTid fiSluttDato 
         cbGyldigTilTid BtnCancel BtnOK 
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
    fiBeskrivelse = cBeskrivelse 
    fiBeskrivelse:SCREEN-VALUE = cBeskrivelse
    fiRab% = plRab%
    fiRab%:SCREEN-VALUE = STRING(plRab%)
    fiStartDato = ?
    cbAktiveresTid = STRING(0,"HH:MM")
    fiSluttDato = ?
/*    fiSluttDato:SCREEN-VALUE = STRING(dSluttDato)*/
    cbGyldigTilTid = STRING(iGyldigTilTid,"HH:MM")
    tgIgnoreNOS = bIgnoreNOS
    tgIgnoreNOS:SCREEN-VALUE = STRING(bIgnoreNOS)
    NormalPris:HIDDEN = TRUE
    .
    RUN VisFelt.    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisFelt C-Win 
PROCEDURE VisFelt :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF INT(AvslagType:SCREEN-VALUE) = 1 THEN 
  DO:
    ASSIGN 
      fiRab%:HIDDEN       = FALSE
      KampanjePris:HIDDEN = TRUE
      KroneRabatt:HIDDEN  = TRUE
      MinstePris:HIDDEN   = TRUE
      .
    APPLY "ENTRY" TO fiRab%.
  END.
  ELSE IF INT(AvslagType:SCREEN-VALUE) = 2 THEN 
  DO:
    ASSIGN 
      fiRab%:HIDDEN       = TRUE
      KampanjePris:HIDDEN = FALSE
      KroneRabatt:HIDDEN  = TRUE
      MinstePris:HIDDEN   = TRUE
      .
    APPLY "ENTRY" TO KampanjePris.
  END.
  ELSE IF INT(AvslagType:SCREEN-VALUE) = 3 THEN 
  DO:
    ASSIGN 
      fiRab%:HIDDEN       = TRUE
      KampanjePris:HIDDEN = TRUE
      KroneRabatt:HIDDEN  = FALSE
      MinstePris:HIDDEN   = FALSE
      .
    APPLY "ENTRY" TO KroneRabatt.
  END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

