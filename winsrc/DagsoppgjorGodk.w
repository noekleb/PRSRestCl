&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

DEF VAR oFmBokforingsVisning AS JBoxFieldMap NO-UNDO.

DEFINE TEMP-TABLE ttBokforingsbilag NO-UNDO
  FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  .

/* Code for Definitions: */

/*** Start instance property definitions for JBoxQuery object oQryBokforingsBilag ***/
DEF VAR oQryBokforingsBilag AS JBoxQuery NO-UNDO.

DEF TEMP-TABLE BokforingsBilag
    FIELD Aar AS INTEGER
    FIELD BokforingsID AS DECIMAL
    FIELD BokforingsNr AS INTEGER
    FIELD BrukerID AS CHARACTER
    FIELD ButikkNr AS INTEGER
    FIELD EDato AS DATE
    FIELD EODDato AS DATE
    FIELD EODDatoTidMottatt AS DATETIME
    FIELD EODMottatt AS LOGICAL
    FIELD ETid AS INTEGER
    FIELD GodkjentAv AS CHARACTER
    FIELD GodkjentDato AS DATE
    FIELD GodkjentFlagg AS LOGICAL
    FIELD GodkjentTid AS INTEGER
    FIELD OmsetningsDato AS DATE
    FIELD RegistrertAv AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD SendAv AS CHARACTER
    FIELD SendtDato AS DATE
    FIELD SendtRegnskap AS LOGICAL
    FIELD SendtTid AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_BokforingsBilag FOR TEMP-TABLE BokforingsBilag.

FUNCTION getTableHandleQryBokforingsBilag RETURNS HANDLE().
  RETURN BUFFER BokforingsBilag:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryBokforingsBilag RETURNS CHARACTER().
  RETURN 
      'BokforingsBilag'.
END FUNCTION.
FUNCTION getQueryJoinQryBokforingsBilag RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxQuery object oQryBokforingsBilag ***/


DEF VAR otbBokforingsbilag AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define QUERY-NAME QUERY-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BokforingsBilag

/* Definitions for QUERY QUERY-3                                        */
&Scoped-define SELF-NAME QUERY-3
&Scoped-define QUERY-STRING-QUERY-3 FOR EACH BokforingsBilag NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-3 OPEN QUERY {&SELF-NAME} FOR EACH BokforingsBilag NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-3 BokforingsBilag
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-3 BokforingsBilag


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 ~
tbBokforingsbilag refresh_tbBokforingsbilag ButikkNr Aar BokforingsNr ~
OmsetningsDato btnGodkjenn GodkjentDato EODDato GodkjentAv ~
EODDatoTidMottatt SendtDato SendAv 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr Aar BokforingsNr OmsetningsDato ~
GodkjentDato EODDato GodkjentAv EODDatoTidMottatt SendtDato SendAv fiTekst ~
EDato fiETid BrukerID RegistrertDato fiOTid RegistrertAv 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnGodkjenn 
     IMAGE-UP FILE "icon\button-blank-red.ico":U
     IMAGE-DOWN FILE "icon/button-blank-yellow.ico":U
     IMAGE-INSENSITIVE FILE "icon/button-blank-green.ico":U
     LABEL "Button 1" 
     SIZE 12.2 BY 1.86 TOOLTIP "Godkjenne bokføringsbilag".

DEFINE BUTTON refresh_tbBokforingsbilag 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE Aar AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "År" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1 TOOLTIP "År".

DEFINE VARIABLE BokforingsNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Bokføringsnummer".

DEFINE VARIABLE BrukerID AS CHARACTER FORMAT "X(10)" 
      VIEW-AS TEXT 
     SIZE 15.6 BY .62 TOOLTIP "Bruker som registrerte/endret posten".

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butnr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "Butikknummer".

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/9999" 
     LABEL "Endret" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 TOOLTIP "Endret dato".

DEFINE VARIABLE EODDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Dato da EOD er mottatt".

DEFINE VARIABLE EODDatoTidMottatt AS DATETIME FORMAT "99/99/9999 HH:MM:SS" 
     LABEL "Mottatt" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Dato og tidspunkt da bilaget ble stemplet med EOD.".

DEFINE VARIABLE EODMottatt AS LOGICAL FORMAT "yes/no" INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "EOD mottatt".

DEFINE VARIABLE fiETid AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiOTid AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiTekst AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 15.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE GodkjentAv AS CHARACTER FORMAT "X(15)" 
     LABEL "Bruker" 
     VIEW-AS FILL-IN 
     SIZE 34.2 BY 1 TOOLTIP "Godkjent av".

DEFINE VARIABLE GodkjentDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Godkjent dato".

DEFINE VARIABLE GodkjentFlagg AS LOGICAL FORMAT "yes/no" INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Godkjent".

DEFINE VARIABLE OmsetningsDato AS DATE FORMAT "99/99/99" 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Dato inneholder omsetningstall for.".

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
      VIEW-AS TEXT 
     SIZE 15.6 BY .62 TOOLTIP "Brukerid på den som registrerte posten".

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Opprettet" 
      VIEW-AS TEXT 
     SIZE 16 BY .62 TOOLTIP "Dato da posten ble registrert i registeret".

DEFINE VARIABLE SendAv AS CHARACTER FORMAT "X(15)" 
     LABEL "Bruker" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE SendtDato AS DATE FORMAT "99/99/99" 
     LABEL "Sendt" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1 TOOLTIP "Dato sendt til regnskapssystem".

DEFINE VARIABLE SendtRegnskap AS LOGICAL FORMAT "yes/no" INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 8.6 BY 1 TOOLTIP "Sendt til regnskap".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 2.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 3.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 3.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 3.

DEFINE RECTANGLE tbBokforingsbilag
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 102.8 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QUERY-3 FOR 
      BokforingsBilag SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     refresh_tbBokforingsbilag AT ROW 1.33 COL 4.2 WIDGET-ID 24
     ButikkNr AT ROW 4.24 COL 12 COLON-ALIGNED
     Aar AT ROW 4.24 COL 30 COLON-ALIGNED
     BokforingsNr AT ROW 4.24 COL 45 COLON-ALIGNED
     OmsetningsDato AT ROW 4.24 COL 64 COLON-ALIGNED
     btnGodkjenn AT ROW 6.95 COL 92 WIDGET-ID 20
     GodkjentDato AT ROW 7.05 COL 52.2 COLON-ALIGNED
     GodkjentFlagg AT ROW 7.05 COL 65.6 COLON-ALIGNED NO-LABEL
     EODDato AT ROW 7.14 COL 12 COLON-ALIGNED
     EODMottatt AT ROW 7.14 COL 25.4 COLON-ALIGNED NO-LABEL
     GodkjentAv AT ROW 8.1 COL 52.2 COLON-ALIGNED
     EODDatoTidMottatt AT ROW 8.19 COL 12 COLON-ALIGNED
     SendtDato AT ROW 10.91 COL 12 COLON-ALIGNED
     SendtRegnskap AT ROW 10.91 COL 25.4 COLON-ALIGNED NO-LABEL
     SendAv AT ROW 11.95 COL 12 COLON-ALIGNED
     fiTekst AT ROW 8.81 COL 87.4 COLON-ALIGNED NO-LABEL
     EDato AT ROW 10.76 COL 54.4 COLON-ALIGNED
     fiETid AT ROW 10.76 COL 70.6 COLON-ALIGNED NO-LABEL
     BrukerID AT ROW 10.76 COL 84.8 COLON-ALIGNED NO-LABEL
     RegistrertDato AT ROW 11.81 COL 54.6 COLON-ALIGNED
     fiOTid AT ROW 11.81 COL 70.8 COLON-ALIGNED NO-LABEL
     RegistrertAv AT ROW 11.81 COL 85 COLON-ALIGNED NO-LABEL
     "EOD" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 6 COL 4.6 WIDGET-ID 12
          FONT 6
     "Godkjent/Godkjenne" VIEW-AS TEXT
          SIZE 28.4 BY .62 AT ROW 6 COL 47.2 WIDGET-ID 16
          FONT 6
     "Bokføringsbilag" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 2.91 COL 4.6 WIDGET-ID 14
          FONT 6
     "Sendt regnskap" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 9.81 COL 4.6 WIDGET-ID 18
          FONT 6
     RECT-1 AT ROW 3.57 COL 3 WIDGET-ID 2
     RECT-2 AT ROW 6.67 COL 3 WIDGET-ID 4
     RECT-3 AT ROW 6.67 COL 45.2 WIDGET-ID 6
     RECT-4 AT ROW 10.48 COL 3 WIDGET-ID 8
     tbBokforingsbilag AT ROW 1.24 COL 3.2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.6 BY 13 WIDGET-ID 100.


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
         TITLE              = "<Insert window title>"
         HEIGHT             = 13.14
         WIDTH              = 108.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 132.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 132.2
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
/* SETTINGS FOR FILL-IN BrukerID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN EDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN EODMottatt IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       EODMottatt:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN fiETid IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiOTid IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN GodkjentFlagg IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       GodkjentFlagg:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN RegistrertAv IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RegistrertDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN SendtRegnskap IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       SendtRegnskap:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       tbBokforingsbilag:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "refresh;Refreshmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-3
/* Query rebuild information for QUERY QUERY-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH BokforingsBilag NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 3.1 , 78 )
*/  /* QUERY QUERY-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGodkjenn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGodkjenn C-Win
ON CHOOSE OF btnGodkjenn IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  IF AVAILABLE Bokforingsbilag THEN 
  DO WITH FRAME {&FRAME-NAME}:
    IF BokforingsBilag.GodkjentFlagg = TRUE THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Bilaget er allerede godkjent!").
      RETURN.
    END.
    
    IF AVAILABLE Bokforingsbilag AND BokforingsBilag.GodkjentFlagg = FALSE THEN 
    DO:
      IF Bokforingsbilag.EODMottatt = TRUE THEN 
      DO:
         IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Godkjenne bokføringsbilag?") THEN 
            RETURN.
      END.
      ELSE DO:
        IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Godkjenne bokføringsbilag selv om EOD ikke er kommet inn fra alle kassene?") THEN 
            RETURN.
      END.
    
      IF NOT JBoxServerApi:Instance:Update("Bokforingsbilag",
                                    Bokforingsbilag.RowIdent1,
                                    "GodkjentDato,GodkjentTid,GodkjentAv,GodkjentFlagg",
                                    STRING(TODAY) + '|' + STRING(TIME) + '|' + JBoxSession:Instance:UserId + '|' + "TRUE",
                                    FALSE,
                                    "bokforingsbilag_post_update.p",
                                    TRUE) THEN 
        DO: 
            JBoxSession:Instance:ViewMessage("Feil ved godkjenning av bokføringsbilaget pga.: " + JBoxServerAPI:Instance:getCallMessage()).
            RETURN.
        END.     
      /* Man kan her gjøre 
        oQryBokforingsBilag:parent-Browse-Object:refresh().
        Det vil løse problemet, men da gjøres det refresh på hele browser. For å gjøre dette bare på en rad
        må man gjøre som under. Kjøre DisplayRecord etterpå.
      */
      hBuffer = BUFFER ttBokforingsbilag:HANDLE.      
      CREATE ttBokforingsbilag. /* En record å sende med. */
      IF JBoxServerAPI:Instance:CallServerProc("Bokforingsbilag_getBlob.p",STRING(Bokforingsbilag.BokforingsId) + '|' + JBoxSession:Instance:UserId + '|SKRIVER',hBuffer) THEN
         JBoxSession:Instance:ViewMessage("Bokføringsbilaget er godkjent og sendt til skriver.").
      DELETE ttBokforingsbilag.    
      
      oQryBokforingsBilag:parent-Browse-Object:refreshRow().
      oQryBokforingsBilag:parent-Browse-Object:displayRecord().
    END.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME QUERY-3
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*  IF <browseObject>:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:*/
/*  END.                                                         */

  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    btnGodkjenn:LOAD-IMAGE-UP ("icon\button-blank-yellow.ico"). 
  END.
  
  IF oQryBokforingsBilag:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:

    IF AVAILABLE Bokforingsbilag THEN
    DO:
      ASSIGN 
        fiOTid:SCREEN-VALUE = STRING(BokforingsBilag.RegistrertTid,"HH:MM:SS")
        fiETid:SCREEN-VALUE = STRING(BokforingsBilag.ETid,"HH:MM:SS")
        .
        
      IF Bokforingsbilag.GodkjentFlagg = FALSE THEN
      DO:
        IF Bokforingsbilag.EODMottatt = FALSE THEN
        DO:
          ASSIGN
            btnGodkjenn:SENSITIVE = TRUE
            fitekst:SCREEN-VALUE = ''
            .
          btnGodkjenn:LOAD-IMAGE-INSENSITIVE ("icon\button-blank-yellow.ico").
        END.
        ELSE DO:
          ASSIGN
            btnGodkjenn:SENSITIVE = TRUE
            fitekst:SCREEN-VALUE = 'GODKJENN'
            .
          btnGodkjenn:LOAD-IMAGE-UP ("icon\button-blank-red.ico").
        END.
      END.
      ELSE DO:
        ASSIGN
          btnGodkjenn:SENSITIVE = FALSE
          fitekst:SCREEN-VALUE = ''
          .
        btnGodkjenn:LOAD-IMAGE-INSENSITIVE ("icon\button-blank-green.ico").
      END.
    END.  
    ELSE DO:
      ASSIGN
        btnGodkjenn:SENSITIVE = FALSE
        fitekst:SCREEN-VALUE = ''
        .
      btnGodkjenn:LOAD-IMAGE-INSENSITIVE ("icon\button-blank-yellow.ico").
    END.
  END.

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
  DISPLAY ButikkNr Aar BokforingsNr OmsetningsDato GodkjentDato EODDato 
          GodkjentAv EODDatoTidMottatt SendtDato SendAv fiTekst EDato fiETid 
          BrukerID RegistrertDato fiOTid RegistrertAv 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 tbBokforingsbilag 
         refresh_tbBokforingsbilag ButikkNr Aar BokforingsNr OmsetningsDato 
         btnGodkjenn GodkjentDato EODDato GodkjentAv EODDatoTidMottatt 
         SendtDato SendAv 
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

  oQryBokforingsBilag = NEW JBoxQuery('BokforingsBilag').

  oFmBokforingsVisning = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).              
  oFmBokforingsVisning:DisplayFields = 'ButikkNr,Aar,BokforingsNr,OmsetningsDato,EODMottatt,EODDato,EODDatoTidMottatt,GodkjentFlagg,GodkjentDato,GodkjentAv,SendtRegnskap,SendtDato,SendAv,RegistrertDato,RegistrertAv,EDato,BrukerID'.
  oFmBokforingsVisning:updateFields = ''.
  oFmBokforingsVisning:primaryKeyFields = 'ButikkNr,Aar,BokforingsNr'.
  oFmBokforingsVisning:postUpdateProc = "bokforingsbilag_post_update.p".  
  
/*  oFmBokforingsVisning:customUpdateValProc = "KOrdreHode_update.p".*/
    
  oFmBokforingsVisning:QUERY-OBJECT = oQryBokforingsBilag.

  oContainer:setNoResizeY("RECT-1,RECT-2,RECT-3,RECT-4,RECT-5").
  oContainer:setNoResizeX("RECT-1,RECT-2,RECT-3,RECT-4,RECT-5").

  otbBokforingsbilag = NEW JBoxToolbar(tbBokforingsbilag:HANDLE).

  oFmBokforingsVisning:TOOLBAR-OBJECT = otbBokforingsbilag.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

