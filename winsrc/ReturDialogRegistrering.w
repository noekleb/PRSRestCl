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


/* Code for Definitions: */

/*** Start instance property definitions for JBoxQuery object oQryKOrdreHode ***/
DEF VAR oQryKOrdreHode AS JBoxQuery NO-UNDO.

DEF TEMP-TABLE KOrdreHode
    FIELD Adresse1 AS CHARACTER
    FIELD Adresse2 AS CHARACTER
    FIELD AnsvVerksted AS CHARACTER
    FIELD AntApnet AS INTEGER
    FIELD AntDager AS INTEGER
    FIELD AntKolli AS INTEGER
    FIELD AntPPEti AS INTEGER
    FIELD AvdelingNr AS INTEGER
    FIELD AvgFriSalg AS DECIMAL
    FIELD AvgPlSalg AS DECIMAL
    FIELD Avrund AS DECIMAL
    FIELD AvrundingKr AS INTEGER
    FIELD AvrundingType AS INTEGER
    FIELD BetaltDato AS DATE
    FIELD BetBet AS INTEGER
    FIELD BetTekst AS CHARACTER
    FIELD BrukerID AS CHARACTER
    FIELD Bruttovekt AS DECIMAL
    FIELD Butik AS INTEGER
    FIELD ButikkNr AS INTEGER
    FIELD cOpt1 AS CHARACTER
    FIELD Dato AS DATE
    FIELD DatoTidEndret AS DATETIME
    FIELD DatoTidOpprettet AS DATETIME
    FIELD DeresRef AS CHARACTER
    FIELD dOpt1 AS DECIMAL
    FIELD EDato AS DATE
    FIELD EkstOrdreNr AS CHARACTER
    FIELD Embalage AS CHARACTER
    FIELD ePostAdresse AS CHARACTER
    FIELD ETid AS INTEGER
    FIELD FaktAdresse1 AS CHARACTER
    FIELD FaktAdresse2 AS CHARACTER
    FIELD FaktLand AS CHARACTER
    FIELD FaktPostNr AS CHARACTER
    FIELD FaktPoststed AS CHARACTER
    FIELD FaktTekstNr AS INTEGER
    FIELD Faktura_Id AS DECIMAL
    FIELD FakturertAv AS CHARACTER
    FIELD FakturertDato AS DATE
    FIELD FakturertTid AS INTEGER
    FIELD FirmaAdresse1 AS CHARACTER
    FIELD FirmaAdresse2 AS CHARACTER
    FIELD FirmaBankKonto AS CHARACTER
    FIELD FirmaEPost AS CHARACTER
    FIELD FirmaLand AS CHARACTER
    FIELD FirmaNavn AS CHARACTER
    FIELD FirmaOrganisasjonsNr AS CHARACTER
    FIELD FirmaPostgiro AS CHARACTER
    FIELD FirmaPostNr AS CHARACTER
    FIELD FirmaPoststed AS CHARACTER
    FIELD FirmaTelefaks AS CHARACTER
    FIELD FirmaTelefon AS CHARACTER
    FIELD FirmaURLAdresse AS CHARACTER
    FIELD ForfallsDato AS DATE
    FIELD ForsNr AS INTEGER
    FIELD Fraktbrevtekst AS CHARACTER
    FIELD Godsmerking AS CHARACTER
    FIELD InternMerknad AS CHARACTER
    FIELD iOpt1 AS INTEGER
    FIELD KasseNr AS INTEGER
    FIELD KontNavn AS CHARACTER
    FIELD KontoNr AS INTEGER
    FIELD KontTelefon AS CHARACTER
    FIELD KOrdre_Id AS DECIMAL
    FIELD KProsjektNr AS INTEGER
    FIELD KundeMerknad AS CHARACTER
    FIELD KundeNr AS DECIMAL
    FIELD LevAdresse1 AS CHARACTER
    FIELD LevAdresse2 AS CHARACTER
    FIELD LeveresDatoTid AS DATETIME
    FIELD Leveringsdato AS DATE
    FIELD LevFNr AS INTEGER
    FIELD LevLand AS CHARACTER
    FIELD LevPostNr AS CHARACTER
    FIELD LevPostSted AS CHARACTER
    FIELD LevStatus AS CHARACTER
    FIELD MobilTlf AS CHARACTER
    FIELD Mva AS DECIMAL
    FIELD MvaKr AS DECIMAL
    FIELD Navn AS CHARACTER
    FIELD Opphav AS INTEGER
    FIELD PostNr AS CHARACTER
    FIELD PostSted AS CHARACTER
    FIELD ProdStatus AS CHARACTER
    FIELD ProduksjonsDato AS DATE
    FIELD Referanse AS CHARACTER
    FIELD RefKOrdre_Id AS DECIMAL
    FIELD RegistrertAv AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD ReturNr AS CHARACTER
    FIELD SelgerNr AS DECIMAL
    FIELD SendingsNr AS CHARACTER
    FIELD ShipmentSendt AS DATETIME
    FIELD SvarFrist AS DATE
    FIELD Telefaks AS CHARACTER
    FIELD Telefon AS CHARACTER
    FIELD TotalRabatt% AS DECIMAL
    FIELD TotalRabattKr AS DECIMAL
    FIELD Totalt AS DECIMAL
    FIELD TotaltVolum AS DECIMAL
    FIELD Utsendelsesdato AS DATE
    FIELD VaarRef AS CHARACTER
    FIELD ValKod AS CHARACTER
    FIELD VerkstedMerknad AS CHARACTER
    FIELD Verkstedordre AS LOGICAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_KOrdreHode FOR KOrdreHode.

FUNCTION getTableHandleQryKOrdreHode RETURNS HANDLE().
  RETURN BUFFER KOrdreHode:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryKOrdreHode RETURNS CHARACTER().
  RETURN 
      'KOrdreHode'.
END FUNCTION.
FUNCTION getQueryJoinQryKOrdreHode RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxQuery object oQryKOrdreHode ***/

DEF VAR oFm AS JBoxFieldMap NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreLinje ***/
DEF VAR oBrwKOrdreLinje AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreLinje
    FIELD KOrdre_Id AS DECIMAL
    FIELD KOrdreLinjeNr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_KOrdreLinje FOR TEMP-TABLE KOrdreLinje.


FUNCTION getBuffersAndFieldsBrwKOrdreLinje RETURNS CHARACTER():
  RETURN
    'KOrdreLinje'
     + ';KOrdre_Id'
     + ';KOrdreLinjeNr'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwKOrdreLinje
&Scoped-define QUERY-NAME QUERY-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KOrdreLinje KOrdreHode

/* Definitions for BROWSE BrwKOrdreLinje                                */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdre_Id ~
KOrdreLinje.KOrdreLinjeNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdre_Id 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define QUERY-STRING-BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreLinje OPEN QUERY BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Definitions for QUERY QUERY-2                                        */
&Scoped-define SELF-NAME QUERY-2
&Scoped-define QUERY-STRING-QUERY-2 FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-2 OPEN QUERY {&SELF-NAME} FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-2 KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-2 KOrdreHode


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS KOrdre_Id BrwKOrdreLinje 
&Scoped-Define DISPLAYED-OBJECTS KOrdre_Id 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "KOrdre Id" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Internt faktura id. Tildeles autmatisk av systemet." NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwKOrdreLinje FOR 
      KOrdreLinje SCROLLING.

DEFINE QUERY QUERY-2 FOR 
      KOrdreHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwKOrdreLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreLinje C-Win _STRUCTURED
  QUERY BrwKOrdreLinje NO-LOCK DISPLAY
      KOrdreLinje.KOrdre_Id FORMAT ">>>>>>>>>>>>9":U
      KOrdreLinje.KOrdreLinjeNr FORMAT ">>>>>>9":U
  ENABLE
      KOrdreLinje.KOrdre_Id
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 105 BY 12.86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     KOrdre_Id AT ROW 3 COL 12 COLON-ALIGNED
     BrwKOrdreLinje AT ROW 6.24 COL 8 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 120.2 BY 19.19 WIDGET-ID 100.


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
         HEIGHT             = 19.19
         WIDTH              = 120.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 120.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 120.2
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
/* BROWSE-TAB BrwKOrdreLinje KOrdre_Id DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.19
       FRAME DEFAULT-FRAME:WIDTH            = 120.2.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreLinje
/* Query rebuild information for BROWSE BrwKOrdreLinje
     _TblList          = "SkoTex.KOrdreLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > KOrdreLinje.KOrdre_Id
"KOrdreLinje.KOrdre_Id" "FId" ">>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? yes "Internt ordre id. Kobler linjen til KOrdreHode." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > KOrdreLinje.KOrdreLinjeNr
"KOrdreLinje.KOrdreLinjeNr" "KOrdreLinje" ">>>>>>9" "integer" ? ? ? ? ? ? no "Linjenummer på faktura" no no "84" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreLinje */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-2
/* Query rebuild information for QUERY QUERY-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 2.19 , 102 )
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


&Scoped-define SELF-NAME QUERY-2
&Scoped-define BROWSE-NAME BrwKOrdreLinje
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
{incl/conttrigg.i oBrwKOrdreLinje:BROWSE-HANDLE}
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
  DISPLAY KOrdre_Id 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE KOrdre_Id BrwKOrdreLinje 
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

  oQryKOrdreHode = NEW JBoxQuery('KOrdreHode').

  oFm = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFm:updateFields = 'KOrdre_Id'.
  oFm:primaryKeyFields = 'KOrdre_Id'.

  oFm:QUERY-OBJECT = oQryKOrdreHode.
  oBrwKOrdreLinje = NEW JBoxBrowse(brwKOrdreLinje:HANDLE).

END.
oBrwKOrdreLinje:OpenQuery().

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

