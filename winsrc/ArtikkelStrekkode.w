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


/*** Start instance property definitions for JBoxBrowse object oBrwStrekKode ***/
DEF VAR oBrwStrekKode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE StrekKode
    FIELD Kode AS character
    FIELD KodeType AS integer
    FIELD HovedNr AS logical
    FIELD StrKode AS integer
    FIELD Strekkode_Storl AS CHARACTER
    FIELD IKasse AS logical
    FIELD Bestillingsnummer AS character
    FIELD ERPNr AS character
    FIELD EDato AS date
    FIELD BrukerID AS character
    FIELD RegistrertDato AS date
    FIELD RegistrertAv AS character
    FIELD ArtikkelNr AS decimal
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_StrekKode FOR TEMP-TABLE StrekKode.


FUNCTION getBuffersAndFieldsBrwStrekKode RETURNS CHARACTER():
  RETURN
    'StrekKode'
     + ';Kode'
     + ';KodeType'
     + ';HovedNr'
     + ';StrKode'
     + ';IKasse'
     + ';Bestillingsnummer'
     + ';ERPNr'
     + ';EDato'
     + ';BrukerID'
     + ';RegistrertDato'
     + ';RegistrertAv'
     + ';ArtikkelNr'
     + ';+Strekkode_Storl|CHARACTER||Strekkode_Storl|Størrelse'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwStrekKode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwStrekKode RETURNS CHARACTER():
  RETURN 
     'server/strekkode_brwcalc.p' /* Strekkode_Storl */
     .
END FUNCTION.


DEF VAR otbKOrdreHode AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwStrekKode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StrekKode

/* Definitions for BROWSE BrwStrekKode                                  */
&Scoped-define FIELDS-IN-QUERY-BrwStrekKode StrekKode.Kode ~
StrekKode.KodeType StrekKode.HovedNr StrekKode.StrKode ~
StrekKode.Strekkode_Storl StrekKode.IKasse StrekKode.Bestillingsnummer ~
StrekKode.ERPNr StrekKode.EDato StrekKode.BrukerID StrekKode.RegistrertDato ~
StrekKode.RegistrertAv StrekKode.ArtikkelNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwStrekKode StrekKode.Kode ~
StrekKode.ArtikkelNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwStrekKode StrekKode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwStrekKode StrekKode
&Scoped-define QUERY-STRING-BrwStrekKode FOR EACH StrekKode NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwStrekKode OPEN QUERY BrwStrekKode FOR EACH StrekKode NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwStrekKode StrekKode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwStrekKode StrekKode


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbKOrdreHode first_tbKOrdreHode ~
prev_tbKOrdreHode next_tbKOrdreHode last_tbKOrdreHode BrwStrekKode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON first_tbKOrdreHode 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbKOrdreHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbKOrdreHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKOrdreHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 60.8 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwStrekKode FOR 
      StrekKode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwStrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwStrekKode C-Win _STRUCTURED
  QUERY BrwStrekKode NO-LOCK DISPLAY
      StrekKode.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      StrekKode.KodeType COLUMN-LABEL "KT" FORMAT ">9":U
      StrekKode.HovedNr COLUMN-LABEL "HNr" FORMAT "yes/no":U
      StrekKode.StrKode COLUMN-LABEL "StrKode" FORMAT ">>>9":U
      StrekKode.Strekkode_Storl COLUMN-LABEL "Størrelse" FORMAT "X(30)":U
      StrekKode.IKasse COLUMN-LABEL "IKasse" FORMAT "J/N":U WIDTH 4
      StrekKode.Bestillingsnummer COLUMN-LABEL "Bestillingsnummer" FORMAT "X(25)":U
      StrekKode.ERPNr COLUMN-LABEL "ERP nr" FORMAT "X(20)":U
      StrekKode.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      StrekKode.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      StrekKode.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      StrekKode.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      StrekKode.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
  ENABLE
      StrekKode.Kode HELP "Strekkode inklusive sjekksiffer."
      StrekKode.ArtikkelNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61 BY 9.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbKOrdreHode AT ROW 1.29 COL 2.4 WIDGET-ID 4
     prev_tbKOrdreHode AT ROW 1.29 COL 7 WIDGET-ID 6
     next_tbKOrdreHode AT ROW 1.29 COL 11.8 WIDGET-ID 8
     last_tbKOrdreHode AT ROW 1.29 COL 16.4 WIDGET-ID 10
     BrwStrekKode AT ROW 2.67 COL 2 WIDGET-ID 200
     tbKOrdreHode AT ROW 1.19 COL 2.2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 62.8 BY 11.38 WIDGET-ID 100.


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
         HEIGHT             = 11.33
         WIDTH              = 63.4
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
/* BROWSE-TAB BrwStrekKode last_tbKOrdreHode DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 62.8.

ASSIGN 
       tbKOrdreHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Lastmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwStrekKode
/* Query rebuild information for BROWSE BrwStrekKode
     _TblList          = "SkoTex.StrekKode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > StrekKode.Kode
"StrekKode.Kode" "Strekkode" "X(20)" "character" ? ? ? ? ? ? yes "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > StrekKode.KodeType
"StrekKode.KodeType" "KT" ">9" "integer" ? ? ? ? ? ? no "Kodetype 1-EAN, 2-Interleave, 3-PLU" no no "2.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > StrekKode.HovedNr
"StrekKode.HovedNr" "HNr" "yes/no" "logical" ? ? ? ? ? ? no "Markerer koden som artikkelens hovednummer på HK." no no "3.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > StrekKode.StrKode
"StrekKode.StrKode" "StrKode" ">>>9" "integer" ? ? ? ? ? ? no "Numerisk kode som representerer en alfanumerisk størrelse" no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"StrekKode.Strekkode_Storl" "Størrelse" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > StrekKode.IKasse
"StrekKode.IKasse" "IKasse" "J/N" "logical" ? ? ? ? ? ? no "Strekkoden skal sendes til kasse" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > StrekKode.Bestillingsnummer
"StrekKode.Bestillingsnummer" "Bestillingsnummer" "X(25)" "character" ? ? ? ? ? ? no "Bestillingsnummer - hvis det er spesielt nr. pr. størrelse." no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > StrekKode.ERPNr
"StrekKode.ERPNr" "ERP nr" "X(20)" "character" ? ? ? ? ? ? no "Artikkelens nr. i ERP system (Ved integrasjon)." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > StrekKode.EDato
"StrekKode.EDato" "Endret" "99/99/9999" "date" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > StrekKode.BrukerID
"StrekKode.BrukerID" "Bruker" "X(10)" "character" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > StrekKode.RegistrertDato
"StrekKode.RegistrertDato" "RDato" "99/99/9999" "date" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > StrekKode.RegistrertAv
"StrekKode.RegistrertAv" "Reg.Av" "X(10)" "character" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > StrekKode.ArtikkelNr
"StrekKode.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "decimal" ? ? ? ? ? ? yes "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwStrekKode */
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


&Scoped-define BROWSE-NAME BrwStrekKode
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
{incl/conttrigg.i oBrwStrekKode:BROWSE-HANDLE}
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
  ENABLE tbKOrdreHode first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode 
         last_tbKOrdreHode BrwStrekKode 
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

  oBrwStrekKode = NEW JBoxBrowse(brwStrekKode:HANDLE).

  otbKOrdreHode = NEW JBoxToolbar(tbKOrdreHode:HANDLE).

  oBrwStrekKode:TOOLBAR-OBJECT = otbKOrdreHode.
END.
oBrwStrekKode:OpenQuery().

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

