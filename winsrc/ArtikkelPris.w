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
DEFINE VARIABLE opopupArtPris AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIkkeVisProfiler AS CHARACTER NO-UNDO.
DEFINE VARIABLE hArtpris_TilbidColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hArtPris_TilbPrisColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hArtPris_KampRab%Column AS HANDLE NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwArtPris ***/
DEF VAR oBrwArtPris AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE ArtPris
    FIELD ProfilNr AS INTEGER
    FIELD artpris_KortNavn AS CHARACTER
    FIELD artpris_ProInnkjopsPris AS CHARACTER
    FIELD artpris_prorab1% AS DECIMAL
    FIELD artpris_propris AS DECIMAL
    FIELD artpris_Tilbid AS LOGICAL
    FIELD artpris_TilbPris AS CHARACTER
    FIELD ArtPris_KampRab% AS CHARACTER
    FIELD Dummy AS CHARACTER
    FIELD ArtikkelNr AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .

FUNCTION getBuffersAndFieldsBrwArtPris RETURNS CHARACTER():
  RETURN
    'ArtPris'
     + ';ProfilNr'
     + ';ArtikkelNr'
     + ';+artpris_KortNavn|CHARACTER||artpris_KortNavn(ROWID)|Profil navn'
     + ';+artpris_ProInnkjopsPris|CHARACTER||artpris_ProInnkjopsPris(ROWID)|InnkjopsPris'
     + ';+artpris_prorab1%|DECIMAL||artpris_ProRab%(ROWID)|Rabatt'
     + ';+artpris_propris|DECIMAL||artpris_ProPris(ROWID)|Pris'
     + ';+artpris_Tilbid|LOGICAL||artpris_Tilbid(ROWID)|Tilbud'
     + ';+artpris_TilbPris|CHARACTER||artpris_TilbPris(ROWID)|TilbPris'
     + ';+ArtPris_KampRab%|CHARACTER||ArtPris_KampRab%|Rab%'
     + ';+Dummy|CHARACTER||jb_void(N/A)|.'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwArtPris RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwArtPris RETURNS CHARACTER():
  RETURN 
     'server/artpris_brwcalc.p' /* artpris_KortNavn(ROWID) */
   + ',server/artpris_brwcalc.p' /* artpris_ProInnkjopsPris(ROWID) */
   + ',server/artpris_brwcalc.p' /* artpris_ProRab%(ROWID) */
   + ',server/artpris_brwcalc.p' /* artpris_ProPris(ROWID) */
   + ',server/artpris_brwcalc.p' /* artpris_Tilbid(ROWID) */
   + ',server/artpris_brwcalc.p' /* artpris_TilbPris(ROWID) */
   + ',server/artpris_brwcalc.p' /* ArtPris_KampRab% */
     .
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwArtPris

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtPris

/* Definitions for BROWSE BrwArtPris                                    */
&Scoped-define FIELDS-IN-QUERY-BrwArtPris ArtPris.ProfilNr ~
ArtPris.artpris_KortNavn ArtPris.artpris_ProInnkjopsPris ~
ArtPris.artpris_prorab1% ArtPris.artpris_propris ArtPris.artpris_Tilbid ~
ArtPris.artpris_TilbPris ArtPris.ArtPris_KampRab% ArtPris.Dummy ~
ArtPris.ArtikkelNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwArtPris ArtPris.ProfilNr ~
ArtPris.ArtikkelNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwArtPris ArtPris
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwArtPris ArtPris
&Scoped-define QUERY-STRING-BrwArtPris FOR EACH ArtPris NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwArtPris OPEN QUERY BrwArtPris FOR EACH ArtPris NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwArtPris ArtPris
&Scoped-define FIRST-TABLE-IN-QUERY-BrwArtPris ArtPris


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwArtPris 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwArtPris FOR 
      ArtPris SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwArtPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwArtPris C-Win _STRUCTURED
  QUERY BrwArtPris NO-LOCK DISPLAY
      ArtPris.ProfilNr COLUMN-LABEL "Profil" FORMAT ">>>9":U WIDTH 5
      ArtPris.artpris_KortNavn COLUMN-LABEL "Profil navn" FORMAT "X(8)":U
            WIDTH 15
      ArtPris.artpris_ProInnkjopsPris COLUMN-LABEL "InnkjopsPris" FORMAT "X(8)":U
            WIDTH 14
      ArtPris.artpris_prorab1% COLUMN-LABEL "Rabatt" FORMAT "->>,>>9.99":U
            WIDTH 10
      ArtPris.artpris_propris COLUMN-LABEL "Pris" FORMAT "->>,>>9.99":U
            WIDTH 14
      ArtPris.artpris_Tilbid COLUMN-LABEL "Tilbud" FORMAT "*/":U
            WIDTH 4
      ArtPris.artpris_TilbPris COLUMN-LABEL "TilbPris" FORMAT "X(10)":U
      ArtPris.ArtPris_KampRab% COLUMN-LABEL "Rab%" FORMAT "X(8)":U
      ArtPris.Dummy COLUMN-LABEL "." FORMAT "X(8)":U
      ArtPris.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
  ENABLE
      ArtPris.ProfilNr HELP "Prisprofil"
      ArtPris.ArtikkelNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 61 BY 10.95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwArtPris AT ROW 1.24 COL 2 WIDGET-ID 200
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
/* BROWSE-TAB BrwArtPris 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 11.38
       FRAME DEFAULT-FRAME:WIDTH            = 62.8.

ASSIGN 
       ArtPris.ArtikkelNr:VISIBLE IN BROWSE BrwArtPris = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwArtPris
/* Query rebuild information for BROWSE BrwArtPris
     _TblList          = "SkoTex.ArtPris"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ArtPris.ProfilNr
"ArtPris.ProfilNr" "Profil" ">>>9" "INTEGER" ? ? ? ? ? ? yes "Prisprofil" no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"ArtPris.artpris_KortNavn" "Profil navn" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"ArtPris.artpris_ProInnkjopsPris" "InnkjopsPris" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"ArtPris.artpris_prorab1%" "Rabatt" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"ArtPris.artpris_propris" "Pris" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"ArtPris.artpris_Tilbid" "Tilbud" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"ArtPris.artpris_TilbPris" "TilbPris" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"ArtPris.ArtPris_KampRab%" "Rab%" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"ArtPris.Dummy" "." "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ArtPris.ArtikkelNr
"ArtPris.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? yes "" no no "14.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwArtPris */
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


&Scoped-define BROWSE-NAME BrwArtPris
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
{incl/conttrigg.i oBrwArtPris:BROWSE-HANDLE}
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
  ENABLE BrwArtPris 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndrePrisRecord C-Win 
PROCEDURE EndrePrisRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   
------------------------------------------------------------------------------*/
  IF NOT oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 OR oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Marker eCom pris for endring.").
      RETURN.
    END.
  IF oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 AND AVAILABLE ArtPris THEN
  DO:
    IF ArtPris.ProfilNr <> 16 THEN
    DO:
      JBoxSession:Instance:ViewMessage("Du kan bare endre eCom(16) pris direkte. Kjede(1) og Outlet(2) profilene vil bli oppdatert automatisk.").
      RETURN.
    END.
    RUN ArtprisOppdatering.w (Artpris.Artikkelnr, 
                              INT(ArtPris.ProfilNr),
                              DEC(artpris_proinnkjopspris),
                              DEC(artpris_ProPris),
                              OUTPUT cTekst, /* STRING(lArtikkelNr) + '|' + STRING(iProfilNr) + '|' + fiInnkjopsPris:SCREEN-VALUE + '|' + fiPris:SCREEN-VALUE + '|' + tgAktiverModell:SCREEN-VALUE */
                              OUTPUT bOk
                              ).
    IF bOk = FALSE THEN 
        JBoxSession:Instance:ViewMessage(cTekst).  
    ELSE DO:
        IF NOT oBrwArtPris:processRowsNoMessage("modell_prisoppdatering.p",cTekst) THEN
            JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
        ELSE DO:
          IF ENTRY(5,cTekst,'|') = 'yes' THEN 
            oBrwArtPris:parent-Browse-Object:openQuery().
          ELSE   
            oBrwArtPris:parent-Browse-Object:refreshRow().
          oBrwArtPris:openQuery().
        END.  
    END.    
  END.

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
    cIkkeVisProfiler = '14'
    .

  oBrwArtPris = NEW JBoxBrowse(brwArtPris:HANDLE).
/*  oBrwArtPris:baseQuery = "WHERE LOOKUP(STRING(ArtPris.ProfilNr),'" + cIkkeVisProfiler + "') = 0".*/

  hArtPris_TilbidColumn   = oBrwArtPris:getColumnHandle("ArtPris_Tilbid").
  hArtPris_TilbPrisColumn = oBrwArtPris:getColumnHandle("ArtPris_TilbPris").
  hArtPris_KampRab%Column = oBrwArtPris:getColumnHandle("ArtPris_KampRab%").

  opopupArtPris = NEW JBoxPopupMenu().
  opopupArtPris:AddToolGroup('EndrePris;Endre normalpris,KopierPris;KopierPris'  
                             ).
  oBrwArtPris:POPUP-MENU-OBJECT = opopupArtPris.

END.
oBrwArtPris:OpenQuery().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierPrisRecord C-Win 
PROCEDURE KopierPrisRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcRetParam AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrisProfilRowIdList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrisProfilIdList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cIdLst AS CHARACTER NO-UNDO.
  
  IF NOT oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 OR oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Marker den ene raden som skal kopieres.").
      RETURN.
    END.
  IF oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 AND AVAILABLE ArtPris THEN
  KOPIERPRIS:
  DO:
    IF JBoxServerAPI:Instance:CallServerProc("modell_profillst.p",STRING(ArtPris.ArtikkelNr),?) THEN
      DO:
        pcRetParam = JBoxServerApi:Instance:getCallReturnParam().
        IF NUM-ENTRIES(pcRetParam,'|') = 2 THEN
          ASSIGN 
            cIdLst  = ENTRY(2,pcRetParam,'|')
            . 
        IF TRIM(cIdLst) = '' THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Det ligger priser på alle profiler på denne artikkelen.").      
          RETURN.
        END.
      END.
    ELSE DO:
      JBoxSession:Instance:ViewMessage("Feil pga: " + JBoxServerAPI:Instance:getCallMessage()).      
      RETURN.
    END.   
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "PrisProfil;ProfilNr;Beskrivelse;KortNavn",
                        "WHERE CAN-DO('" + cIdLst + "',STRING(PrisProfil.ProfilNr))",
                        INPUT-OUTPUT cPrisProfilRowIdList,
                        "ProfilNr",
                        INPUT-OUTPUT cPrisProfilIdList,
                        "","",
                        OUTPUT bOK).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    IF bOk = FALSE OR cPrisProfilIdList = '' THEN 
        JBoxSession:Instance:ViewMessage('Ingen profil valgt for kopiering.').  
    ELSE DO:
        IF NOT oBrwArtPris:processRowsNoMessage("modell_priskopiering.p",STRING(ArtPris.ArtikkelNr) + '|' + STRING(ArtPris.ProfilNr) + '|' + REPLACE(cPrisProfilIdList,'|',',')) THEN
            JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
        ELSE DO:  
          oBrwArtPris:openQuery().
        END.  
    END.    
  END. /* KOPIERPRIS */


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  IF ArtPris.ArtPris_Tilbid = TRUE THEN 
  DO:
    ASSIGN 
      hArtPris_TilbidColumn:BGCOLOR   = 13
      hArtPris_TilbPrisColumn:BGCOLOR = 13
      hArtPris_KampRab%Column:BGCOLOR = 13
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

