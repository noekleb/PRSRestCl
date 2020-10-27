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


/*** Start instance property definitions for JBoxBrowse object oBrwPkSdlHode ***/
DEF VAR oBrwPkSdlHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE PkSdlHode
    FIELD PkSdlId AS DECIMAL
    FIELD PkSdlNr AS CHARACTER
    FIELD PkSdlStatus AS INTEGER
    FIELD PkSdlOpphav AS INTEGER
    FIELD Merknad AS CHARACTER
    FIELD MeldingFraLev AS CHARACTER
    FIELD cPalleNr AS CHARACTER
    FIELD Lokasjon AS CHARACTER
    FIELD ButikkNr AS INTEGER
    FIELD SendtOutlet AS INTEGER
    FIELD SendtFraLagerTilOutlet AS DATETIME
    FIELD RegistrertDato AS DATE
    FIELD RegistrertAv AS CHARACTER
    FIELD OrdreType AS CHARACTER
    FIELD FakturaNr AS DECIMAL
    FIELD LandedCost AS DECIMAL
    FIELD Sesongkode AS CHARACTER
    FIELD LeveringsDato AS DATE
    FIELD BrukerID AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_PkSdlHode FOR TEMP-TABLE PkSdlHode.


FUNCTION getBuffersAndFieldsBrwPkSdlHode RETURNS CHARACTER():
  RETURN
    'PkSdlHode'
     + ';PkSdlId'
     + ';PkSdlNr'
     + ';PkSdlStatus'
     + ';PkSdlOpphav'
     + ';Merknad'
     + ';MeldingFraLev'
     + ';cPalleNr'
     + ';Lokasjon'
     + ';ButikkNr'
     + ';SendtOutlet'
     + ';SendtFraLagerTilOutlet'
     + ';RegistrertDato'
     + ';RegistrertAv'
     + ';OrdreType'
     + ';FakturaNr'
     + ';LandedCost'
     + ';Sesongkode'
     + ';LeveringsDato'
     + ';BrukerID'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwPkSdlHode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

&Scoped-define WIDGETID-FILE-NAME 

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwPkSdlHode
&Scoped-define QUERY-NAME QUERY-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PkSdlHode

/* Definitions for BROWSE BrwPkSdlHode                                  */
&Scoped-define FIELDS-IN-QUERY-BrwPkSdlHode PkSdlHode.PkSdlId ~
PkSdlHode.PkSdlNr PkSdlHode.PkSdlStatus PkSdlHode.PkSdlOpphav ~
PkSdlHode.Merknad PkSdlHode.MeldingFraLev PkSdlHode.cPalleNr ~
PkSdlHode.Lokasjon PkSdlHode.ButikkNr PkSdlHode.SendtOutlet ~
PkSdlHode.SendtFraLagerTilOutlet PkSdlHode.RegistrertDato ~
PkSdlHode.RegistrertAv PkSdlHode.OrdreType PkSdlHode.FakturaNr ~
PkSdlHode.LandedCost PkSdlHode.Sesongkode PkSdlHode.LeveringsDato ~
PkSdlHode.BrukerID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwPkSdlHode PkSdlHode.PkSdlId 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define QUERY-STRING-BrwPkSdlHode FOR EACH PkSdlHode NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwPkSdlHode OPEN QUERY BrwPkSdlHode FOR EACH PkSdlHode NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwPkSdlHode PkSdlHode


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiPkSdlNr BrwPkSdlHode 
&Scoped-Define DISPLAYED-OBJECTS fiPkSdlNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE fiPkSdlNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pakkseddelnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwPkSdlHode FOR 
      PkSdlHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwPkSdlHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwPkSdlHode C-Win _STRUCTURED
  QUERY BrwPkSdlHode NO-LOCK DISPLAY
      PkSdlHode.PkSdlId FORMAT ">>>>>>>>>>>>9":U
      PkSdlHode.PkSdlNr FORMAT "X(15)":U
      PkSdlHode.PkSdlStatus FORMAT ">9":U
      PkSdlHode.PkSdlOpphav FORMAT ">9":U
      PkSdlHode.Merknad FORMAT "X(30)":U
      PkSdlHode.MeldingFraLev FORMAT "X(30)":U
      PkSdlHode.cPalleNr FORMAT "x(8)":U
      PkSdlHode.Lokasjon FORMAT "x(20)":U
      PkSdlHode.ButikkNr FORMAT ">>>>>9":U
      PkSdlHode.SendtOutlet FORMAT ">>>>>9":U
      PkSdlHode.SendtFraLagerTilOutlet FORMAT "99/99/9999 HH:MM:SS.SSS":U
      PkSdlHode.RegistrertDato FORMAT "99/99/9999":U
      PkSdlHode.RegistrertAv FORMAT "X(10)":U
      PkSdlHode.OrdreType FORMAT "x(5)":U
      PkSdlHode.FakturaNr FORMAT ">>>>>>>>>>>>9":U
      PkSdlHode.LandedCost FORMAT "->>,>>>,>>>,>>9.99":U
      PkSdlHode.Sesongkode FORMAT "x(8)":U
      PkSdlHode.LeveringsDato FORMAT "99/99/99":U
      PkSdlHode.BrukerID FORMAT "X(10)":U
  ENABLE
      PkSdlHode.PkSdlId
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 119 BY 14.76 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiPkSdlNr AT ROW 3.38 COL 19 COLON-ALIGNED WIDGET-ID 2
     BrwPkSdlHode AT ROW 5.76 COL 4 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 123.6 BY 19.86 WIDGET-ID 100.


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
         HEIGHT             = 19.86
         WIDTH              = 123.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 123.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 123.6
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
/* BROWSE-TAB BrwPkSdlHode fiPkSdlNr DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.86
       FRAME DEFAULT-FRAME:WIDTH            = 123.6.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwPkSdlHode
/* Query rebuild information for BROWSE BrwPkSdlHode
     _TblList          = "SkoTex.PkSdlHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > PkSdlHode.PkSdlId
"PkSdlHode.PkSdlId" "Pakkseddel id" ">>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? yes "Internt pakkseddelid." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > PkSdlHode.PkSdlNr
"PkSdlHode.PkSdlNr" "Pk.sdl.nr" "X(15)" "character" ? ? ? ? ? ? no "Pakkseddelnummer" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > PkSdlHode.PkSdlStatus
"PkSdlHode.PkSdlStatus" "Status" ">9" "integer" ? ? ? ? ? ? no "Pakkseddel status" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > PkSdlHode.PkSdlOpphav
"PkSdlHode.PkSdlOpphav" "Opphav" ">9" "integer" ? ? ? ? ? ? no "Pakkseddelopphav. 1-Internt,2-ERP,3-Pda/Ht" no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > PkSdlHode.Merknad
"PkSdlHode.Merknad" "Merknad" "X(30)" "character" ? ? ? ? ? ? no "Merknad" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > PkSdlHode.MeldingFraLev
"PkSdlHode.MeldingFraLev" "Merkn. fra lev." "X(30)" "character" ? ? ? ? ? ? no "Merknad fra leverandør." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > PkSdlHode.cPalleNr
"PkSdlHode.cPalleNr" "PalleNr" "x(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > PkSdlHode.Lokasjon
"PkSdlHode.Lokasjon" "Lokasjon" "x(20)" "character" ? ? ? ? ? ? no "Viser hvilken lokasjon pakkseddelens varer er plassert i." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > PkSdlHode.ButikkNr
"PkSdlHode.ButikkNr" "Butikknr" ">>>>>9" "integer" ? ? ? ? ? ? no "Butikk ordren er stilet til. Settes til 0  hvis den går til fle" no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > PkSdlHode.SendtOutlet
"PkSdlHode.SendtOutlet" "SB" ">>>>>9" "integer" ? ? ? ? ? ? no "Pakkseddelens varer er sendt til Outlet." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > PkSdlHode.SendtFraLagerTilOutlet
"PkSdlHode.SendtFraLagerTilOutlet" "Sendt outlet" "99/99/9999 HH:MM:SS.SSS" "datetime" ? ? ? ? ? ? no "Dato/tid når pakkseddel ble sendt fra lager til outlet" no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > PkSdlHode.RegistrertDato
"PkSdlHode.RegistrertDato" "RDato" "99/99/9999" "date" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > PkSdlHode.RegistrertAv
"PkSdlHode.RegistrertAv" "Reg.Av" "X(10)" "character" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > PkSdlHode.OrdreType
"PkSdlHode.OrdreType" "OType" "x(5)" "character" ? ? ? ? ? ? no "Ordretype." no no "6.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > PkSdlHode.FakturaNr
"PkSdlHode.FakturaNr" "FakturaNr" ">>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? no "Fakturanummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > PkSdlHode.LandedCost
"PkSdlHode.LandedCost" "LandedCost" "->>,>>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no "" no no "18.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > PkSdlHode.Sesongkode
"PkSdlHode.Sesongkode" "Sesong" "x(8)" "character" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > PkSdlHode.LeveringsDato
"PkSdlHode.LeveringsDato" "Leveringsdato" "99/99/99" "date" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > PkSdlHode.BrukerID
"PkSdlHode.BrukerID" "Bruker" "X(10)" "character" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwPkSdlHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-2
/* Query rebuild information for QUERY QUERY-2
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 3.14 , 93 )
*/  /* QUERY QUERY-2 */
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


&Scoped-define BROWSE-NAME BrwPkSdlHode
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
{incl/conttrigg.i oBrwPkSdlHode:BROWSE-HANDLE}
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
  DISPLAY fiPkSdlNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fiPkSdlNr BrwPkSdlHode 
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

  oBrwPkSdlHode = NEW JBoxBrowse(brwPkSdlHode:HANDLE).

END.
oBrwPkSdlHode:OpenQuery().

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

