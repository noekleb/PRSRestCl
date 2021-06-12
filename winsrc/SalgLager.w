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
DEFINE INPUT PARAMETER icParam AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE hLager AS HANDLE NO-UNDO.
DEFINE VARIABLE hByggerRapport AS HANDLE NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cModus AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE opopupLager AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProfilLst AS CHARACTER NO-UNDO.

DEFINE VARIABLE cVgLst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVgRowIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVmLst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVmRowIdLst AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwLager ***/
DEF VAR oBrwLager AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Lager
    FIELD Varetekst AS CHARACTER
    FIELD LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD Sasong AS CHARACTER
    FIELD Pris AS DECIMAL
    FIELD Solgt% AS DECIMAL
    FIELD Solgt AS DECIMAL
    FIELD VerdiSolgt AS DECIMAL
    FIELD Lagant AS DECIMAL
    FIELD VerdiLager AS DECIMAL
    FIELD Lager_AntProfil AS DECIMAL
    FIELD Lager_VerdiProfil AS DECIMAL
    FIELD Varegruppe AS CHARACTER
    FIELD Hovedgruppe AS CHARACTER
    FIELD Produsent AS CHARACTER
    FIELD Varemerke AS CHARACTER
    FIELD VVarekost AS DECIMAL
    FIELD VmId AS INTEGER
    FIELD HovedKAtNr AS INTEGER
    FIELD Butik AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
/*DEF BUFFER v_Lager FOR TEMP-TABLE Lager.*/

FUNCTION getBuffersAndFieldsBrwLager RETURNS CHARACTER():
  RETURN
    'Lager'
     + ';Butik'
     + ';ArtikkelNr'
     + ';+Varetekst|CHARACTER||jb_void(N/A)|Varetekst'
     + ';+LevKod|CHARACTER||jb_void(N/A)|Lev.artikkelnr'
     + ';+LevFargKod|CHARACTER||jb_void(N/A)|Lev.fargekode'
     + ';+Sasong|CHARACTER||jb_void(N/A)|Sesong'
     + ';+Pris|DECIMAL||jb_void(N/A)|Pris'
     + ';+Solgt%|DECIMAL||jb_void(N/A)|Solgt%'
     + ';+Solgt|DECIMAL||jb_void(N/A)|Solgt'
     + ';+VerdiSolgt|DECIMAL||jb_void(N/A)|Verdi solgt'
     + ';+Lagant|DECIMAL||jb_void(N/A)|Lager'
     + ';+VerdiLager|DECIMAL||jb_void(N/A)|Verdi lager'
     + ';+Lager_AntProfil|DECIMAL||jb_void(N/A)|Antall i profil'
     + ';+Lager_VerdiProfil|DECIMAL||jb_void(N/A)|Verdi i profil'
     + ';+Varegruppe|CHARACTER||jb_void(N/A)|Varegruppe'
     + ';+Hovedgruppe|CHARACTER||jb_void(N/A)|Hovedgruppe'
     + ';+Produsent|CHARACTER||jb_void(N/A)|Produsent'
     + ';+Varemerke|CHARACTER||jb_void(N/A)|Varemerke'
     + ';+VVarekost|DECIMAL||jb_void(N/A)|Vektet varekost'
     + ';+VmId|INTEGER||jb_void(N/A)|Varemerke Id'
     + ';+HovedKAtNr|INTEGER||jb_void(N/A)|HovedKatNr'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwLager RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR otbLager AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwLager

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Lager

/* Definitions for BROWSE BrwLager                                      */
&Scoped-define FIELDS-IN-QUERY-BrwLager Lager.Varetekst Lager.LevKod ~
Lager.LevFargKod Lager.Sasong Lager.Pris Lager.Solgt% Lager.Solgt ~
Lager.VerdiSolgt Lager.Lagant Lager.VerdiLager Lager.Lager_AntProfil ~
Lager.Lager_VerdiProfil Lager.Varegruppe Lager.Hovedgruppe Lager.Produsent ~
Lager.Varemerke Lager.VVarekost Lager.VmId Lager.HovedKAtNr Lager.Butik ~
Lager.ArtikkelNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwLager Lager.Varetekst Lager.Butik 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwLager Lager
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwLager Lager
&Scoped-define QUERY-STRING-BrwLager FOR EACH Lager NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwLager OPEN QUERY BrwLager FOR EACH Lager NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwLager Lager
&Scoped-define FIRST-TABLE-IN-QUERY-BrwLager Lager


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbLager RECT-1 searchLager first_tbLager ~
prev_tbLager next_tbLager last_tbLager refresh_tbLager filter_tbLager ~
excel_tbLager btnUtvalgHovedKategori fiSolgt%Fra fiSolgt%Til fiVgLst ~
cbSolgt% btnBlank btnUtvalgVaremerke cbLagant fiVmIdLst BrwLager 
&Scoped-Define DISPLAYED-OBJECTS fiSolgt%Fra fiSolgt%Til fiVgLst cbSolgt% ~
cbLagant fiVmIdLst FI-LagerInfo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "<Blank filter>" 
     SIZE 15 BY 1 TOOLTIP "Renser filter og åpner spørringen igjen.".

DEFINE BUTTON btnUtvalgHovedKategori 
     LABEL "Hovedkategori" 
     SIZE 20 BY 1.1.

DEFINE BUTTON btnUtvalgVaremerke 
     LABEL "Varemerke" 
     SIZE 20 BY 1.1.

DEFINE BUTTON excel_tbLager 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbLager 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbLager 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbLager 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbLager 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbLager 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbLager 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbLagant AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Bare vis lager" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "<Alle>",0,
                     "Større enn 0",1,
                     "Mindre enn 0",2,
                     "Ulik 0",3,
                     "Lik 0",4
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE cbSolgt% AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Bare vis solgt%" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "<Alle>",0,
                     "Større enn 0",1,
                     "Mindre enn 0",2,
                     "Ulik 0",3,
                     "Lik 0",4
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LagerInfo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 43.6 BY 1 TOOLTIP "Viser hvilke lagerantalls kolonne lagerfilter går mot."
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiSolgt%Fra AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     LABEL "Fra/til Solgt%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fra og med Solgt%" NO-UNDO.

DEFINE VARIABLE fiSolgt%Til AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Til og med Solgt%" NO-UNDO.

DEFINE VARIABLE fiVgLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hovedkategori" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med varegrupper"
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fiVmIdLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varemerke lst" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med varemerker"
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 190 BY 2.57.

DEFINE RECTANGLE searchLager
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY .91.

DEFINE RECTANGLE tbLager
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 230.8 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwLager FOR 
      Lager SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwLager C-Win _STRUCTURED
  QUERY BrwLager NO-LOCK DISPLAY
      Lager.Varetekst COLUMN-LABEL "Varetekst" FORMAT "X(40)":U
      Lager.LevKod COLUMN-LABEL "Lev.artikkelnr" FORMAT "X(30)":U
      Lager.LevFargKod COLUMN-LABEL "Lev.fargekode" FORMAT "X(30)":U
      Lager.Sasong COLUMN-LABEL "Sesong" FORMAT "X(10)":U
      Lager.Pris COLUMN-LABEL "Pris" FORMAT "->,>>>,>>9.99":U
      Lager.Solgt% COLUMN-LABEL "Solgt%" FORMAT "->>,>>9.9":U
      Lager.Solgt COLUMN-LABEL "Solgt" FORMAT "->>,>>>,>>9":U
      Lager.VerdiSolgt COLUMN-LABEL "Verdi solgt" FORMAT "->>,>>>,>>9.99":U
      Lager.Lagant COLUMN-LABEL "Lager" FORMAT "->>,>>9.99":U WIDTH 14.4
      Lager.VerdiLager COLUMN-LABEL "Verdi lager" FORMAT "->>,>>>,>>9.99":U
      Lager.Lager_AntProfil COLUMN-LABEL "Antall i profil" FORMAT "->>>,>>9":U
      Lager.Lager_VerdiProfil COLUMN-LABEL "Verdi i profil" FORMAT "->>,>>>,>>9":U
            WIDTH 10.8
      Lager.Varegruppe COLUMN-LABEL "Varegruppe" FORMAT "X(30)":U
      Lager.Hovedgruppe COLUMN-LABEL "Hovedgruppe" FORMAT "X(30)":U
      Lager.Produsent COLUMN-LABEL "Produsent" FORMAT "X(30)":U
      Lager.Varemerke COLUMN-LABEL "Varemerke" FORMAT "X(30)":U
      Lager.VVarekost COLUMN-LABEL "Vektet varekost" FORMAT "->,>>>,>>9.99":U
      Lager.VmId COLUMN-LABEL "Varemerke Id" FORMAT ">>>>>9":U
      Lager.HovedKAtNr COLUMN-LABEL "HovedKatNr" FORMAT ">>>>>9":U
      Lager.Butik COLUMN-LABEL "ButNr" FORMAT ">>>>>9":U
      Lager.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 54.2
  ENABLE
      Lager.Varetekst
      Lager.Butik HELP "Butikknummer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 231 BY 17 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbLager AT ROW 1.19 COL 2.4 WIDGET-ID 4
     prev_tbLager AT ROW 1.19 COL 7.2 WIDGET-ID 6
     next_tbLager AT ROW 1.19 COL 11.8 WIDGET-ID 8
     last_tbLager AT ROW 1.19 COL 16.4 WIDGET-ID 10
     refresh_tbLager AT ROW 1.19 COL 21 WIDGET-ID 12
     filter_tbLager AT ROW 1.19 COL 25.6 WIDGET-ID 14
     excel_tbLager AT ROW 1.19 COL 30.2 WIDGET-ID 16
     btnUtvalgHovedKategori AT ROW 3.24 COL 130.2 WIDGET-ID 76
     fiSolgt%Fra AT ROW 3.29 COL 58 COLON-ALIGNED
     fiSolgt%Til AT ROW 3.29 COL 72.4 COLON-ALIGNED NO-LABEL
     fiVgLst AT ROW 3.29 COL 103.6 COLON-ALIGNED
     cbSolgt% AT ROW 3.29 COL 165.4 COLON-ALIGNED WIDGET-ID 38
     btnBlank AT ROW 3.29 COL 188.2 WIDGET-ID 50
     btnUtvalgVaremerke AT ROW 4.33 COL 130.2 WIDGET-ID 80
     cbLagant AT ROW 4.33 COL 165.4 COLON-ALIGNED WIDGET-ID 36
     fiVmIdLst AT ROW 4.38 COL 103.6 COLON-ALIGNED
     BrwLager AT ROW 5.86 COL 2 WIDGET-ID 200
     FI-LagerInfo AT ROW 4.33 COL 186.4 COLON-ALIGNED NO-LABEL
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.71 COL 44 WIDGET-ID 48
          FONT 6
     tbLager AT ROW 1.14 COL 2 WIDGET-ID 2
     RECT-1 AT ROW 3.05 COL 43 WIDGET-ID 44
     searchLager AT ROW 4.81 COL 3 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 233 BY 22.24 WIDGET-ID 100.


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
         TITLE              = "Solgt rapport lager"
         HEIGHT             = 22.24
         WIDTH              = 233
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 233
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 233
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
/* BROWSE-TAB BrwLager fiVmIdLst DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 22.24
       FRAME DEFAULT-FRAME:WIDTH            = 233.

/* SETTINGS FOR FILL-IN FI-LagerInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-LagerInfo:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       tbLager:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwLager
/* Query rebuild information for BROWSE BrwLager
     _TblList          = "SkoTex.Lager"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Lager.Varetekst" "Varetekst" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Lager.LevKod" "Lev.artikkelnr" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Lager.LevFargKod" "Lev.fargekode" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Lager.Sasong" "Sesong" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Lager.Pris" "Pris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Lager.Solgt%" "Solgt%" "->>,>>9.9" "DECIMAL" ? ? ? ? ? ? no "" no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Lager.Solgt" "Solgt" "->>,>>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Lager.VerdiSolgt" "Verdi solgt" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Lager.Lagant" "Lager" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"Lager.VerdiLager" "Verdi lager" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Lager.Lager_AntProfil" "Antall i profil" "->>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"Lager.Lager_VerdiProfil" "Verdi i profil" "->>,>>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"Lager.Varegruppe" "Varegruppe" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"Lager.Hovedgruppe" "Hovedgruppe" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"Lager.Produsent" "Produsent" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"Lager.Varemerke" "Varemerke" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"Lager.VVarekost" "Vektet varekost" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"Lager.VmId" "Varemerke Id" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "12.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"Lager.HovedKAtNr" "HovedKatNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "11.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Lager.Butik
"Lager.Butik" "ButNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikknummer" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Lager.ArtikkelNr
"Lager.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "" no no "54.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwLager */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Solgt rapport lager */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Solgt rapport lager */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME DEFAULT-FRAME /* <Blank filter> */
DO:
  IF cVgLst <> '' OR cVmLst <> '' THEN 
  DO:
    ASSIGN 
      cVgLst                   = ''
      cVgRowIdLst              = ''
      cVmLst                   = ''
      cVmRowIdLst              = ''
      .

      oBrwLager:setCalcFieldParam('Lager_Vg', cVgLst).
      oBrwLager:setCalcFieldParam('Lager_VmId', cVmLst).

      hLager = BUFFER Lager:HANDLE.
      RUN ByggerRapport.w PERSISTENT SET hByggerRapport.
      RUN MoveToTop IN hByggerRapport.
      RUN vistekst IN hByggerRapport.
      SESSION:SET-WAIT-STATE("GENERAL").  
      IF JBoxServerAPI:Instance:CallServerProc("lager_salg.p",cVgLst + '¤' + cVmLst + '¤' + cProfilLst,hLager) THEN
        hLager = JBoxServerAPI:Instance:getCallReturnTable().
      SESSION:SET-WAIT-STATE("").
      RUN avsluttDialog IN hByggerRapport.
      IF VALID-HANDLE(hByggerRapport) THEN 
        DELETE OBJECT hByggerRapport.
  END.
  ASSIGN 
    fiSolgt%Fra:SCREEN-VALUE = ''
    fisolgt%Til:SCREEN-VALUE = ''
    fiVgLst:SCREEN-VALUE     = ''
    fiVmIdLst:SCREEN-VALUE   = ''
    .
  RUN setfilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgHovedKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgHovedKategori C-Win
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
  IF bOk THEN 
    RUN RefreshRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgVaremerke
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgVaremerke C-Win
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
  IF bOk THEN 
    RUN RefreshRecord.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLagant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLagant C-Win
ON VALUE-CHANGED OF cbLagant IN FRAME DEFAULT-FRAME /* Bare vis lager */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbSolgt%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbSolgt% C-Win
ON VALUE-CHANGED OF cbSolgt% IN FRAME DEFAULT-FRAME /* Bare vis solgt% */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSolgt%Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Fra C-Win
ON RETURN OF fiSolgt%Fra IN FRAME DEFAULT-FRAME /* Fra/til Solgt% */
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Fra C-Win
ON TAB OF fiSolgt%Fra IN FRAME DEFAULT-FRAME /* Fra/til Solgt% */
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSolgt%Til
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Til C-Win
ON RETURN OF fiSolgt%Til IN FRAME DEFAULT-FRAME
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Til C-Win
ON TAB OF fiSolgt%Til IN FRAME DEFAULT-FRAME
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwLager
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
{incl/conttrigg.i oBrwLager:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
DO WITH FRAME {&FRAME-NAME}:
    CASE cModus:
      WHEN '20' THEN 
        DO:
          RUN ValgtVareRecord.
        END.
    END CASE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY fiSolgt%Fra fiSolgt%Til fiVgLst cbSolgt% cbLagant fiVmIdLst 
          FI-LagerInfo 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbLager RECT-1 searchLager first_tbLager prev_tbLager next_tbLager 
         last_tbLager refresh_tbLager filter_tbLager excel_tbLager 
         btnUtvalgHovedKategori fiSolgt%Fra fiSolgt%Til fiVgLst cbSolgt% 
         btnBlank btnUtvalgVaremerke cbLagant fiVmIdLst BrwLager 
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
    iButNr     = INT(ENTRY(1,icParam,'|'))
    cModus     = ENTRY(2,icParam,'|') /*10=Fra meny, 20=Fra kampanjeregister NB: Overstyres ved oppstart fra kampanje. */
    cProfilLst = ENTRY(3,icParam,'|')
    .
  IF cModus = '20' THEN 
  DO:
    IF NUM-ENTRIES(icParam) >= 3 THEN
      ASSIGN  
        cProfilLst = ENTRY(3,ICParam,'|')
        cProfilLst = REPLACE(cProfilLst,'¤',',')
        .
    IF NUM-ENTRIES(icParam,'|') >= 4 THEN 
      iKampanjeId = INT(ENTRY(4,icParam,'|')).
  END.

  oBrwLager = NEW JBoxBrowse(brwLager:HANDLE).
  oBrwLager:useLocalData = YES. /* Flagger at browser går rundt en temp-tabell, ikke en database tabell. */
  otbLager = NEW JBoxToolbar(tbLager:HANDLE).
  oBrwLager:TOOLBAR-OBJECT = otbLager.
  otbLager:addToolGroup("ValgtVare;Legg varer til kampanje...").

  oContainer:setNoResizeY("RECT-1").

  ASSIGN 
    cbLagant:SCREEN-VALUE = '1'
    cbLagant              = 1
    .

  oBrwLager:setSearchField(searchLager:HANDLE,"LevKod").

  opopupLager = NEW JBoxPopupMenu().
  opopupLager:AddToolGroup('OppslagModell;Vis i modell liste'  
                           ).
  oBrwLager:POPUP-MENU-OBJECT = opopupLager.

  FI-LagerInfo:SCREEN-VALUE = IF cModus = '10' THEN 'Filter på nettbutikk' ELSE 'Filter på profil'.
  /* Bygger tempTabell */
  RUN RefreshRecord.
  
  /* Innholder OpenQuery */
  RUN setFilter.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
hLager = BUFFER Lager:HANDLE.

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppslagModellRecord C-Win 
PROCEDURE OppslagModellRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE Lager THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',Lager.LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',Lager.LevFargKod)
        .
      DO ON ERROR UNDO, LEAVE:  
        oContainer:StartTabWindow('LagerListeModButStr.w', 'Modelliste', FALSE, YES).
        PUBLISH 'settModellFilter' (cFilterTekst).
      END.
    END.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
RUN ByggerRapport.w PERSISTENT SET hByggerRapport.
  RUN MoveToTop IN hByggerRapport.
  RUN vistekst IN hByggerRapport.

  hLager = BUFFER Lager:HANDLE.

  SESSION:SET-WAIT-STATE("GENERAL").  
  IF JBoxServerAPI:Instance:CallServerProc("lager_salg.p",cVgLst + '¤' + cVmLst + '¤' + cProfilLst,hLager) THEN
    hLager = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").

  RUN avsluttDialog IN hByggerRapport.
  IF VALID-HANDLE(hByggerRapport) THEN 
    DELETE OBJECT hByggerRapport.

/*  RUN SUPER.*/
/*  oBrwLager:OpenQuery().*/
  RUN setFilter.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowsToBatchRecord C-Win 
PROCEDURE rowsToBatchRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectAllRowsRecord C-Win 
PROCEDURE SelectAllRowsRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*DEF VAR hRecordSelectWidget AS HANDLE NO-UNDO.                                                  */
/*DEF VAR hBrowse             AS HANDLE NO-UNDO.                                                  */
/*                                                                                                */
/*IF AVAIL ttObject THEN DO:                                                                      */
/*  IF ttObject.hObject:TYPE NE "browse" THEN DO:                                                 */
/*    hBrowse = DYNAMIC-FUNCTION("getLinkedObject",ttObject.hObject,"browse","from").             */
/*    IF NOT VALID-HANDLE(hBrowse) OR hBrowse:TYPE NE "browse" THEN DO:                           */
/*      MESSAGE "Missing association (link) from " ttObject.cObjectType " to browse" SKIP         */
/*              "Programmers mistake"                                                             */
/*              VIEW-AS ALERT-BOX.                                                                */
/*      RETURN.                                                                                   */
/*    END.                                                                                        */
/*  END.                                                                                          */
/*  ELSE hBrowse = ttObject.hObject.                                                              */
/*END.                                                                                            */
/*ELSE RETURN.                                                                                    */
/*                                                                                                */
/*hBrowse:SELECT-ALL().                                                                           */
/*hRecordSelectWidget = WIDGET-HANDLE(getAttribute(hBrowse:WINDOW,"RecordSelectWidget")) NO-ERROR.*/
/*                                                                                                */
/*IF VALID-HANDLE(hRecordSelectWidget) THEN                                                       */
/*  IF hBrowse:NUM-SELECTED-ROWS > 0 THEN                                                         */
/*    hRecordSelectWidget:SCREEN-VALUE = STRING(hBrowse:NUM-SELECTED-ROWS,"zzzzz9").              */
/*  ELSE                                                                                          */
/*    hRecordSelectWidget:SCREEN-VALUE = "".                                                      */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilter C-Win 
PROCEDURE setFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcWhere AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcOperator AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcFelt AS CHARACTER NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    IF cVgLst <> '' OR cVmLst <> '' THEN 
    DO:
      oBrwLager:setCalcFieldParam('Lager_Vg', cVgLst).
      oBrwLager:setCalcFieldParam('Lager_VmId', cVmLst).
      
/*      hLager = BUFFER Lager:HANDLE.*/
/*      RUN ByggerRapport.w PERSISTENT SET hByggerRapport.                                                           */
/*      RUN MoveToTop IN hByggerRapport.                                                                             */
/*      RUN vistekst IN hByggerRapport.                                                                              */
/*      SESSION:SET-WAIT-STATE("GENERAL").                                                                           */
/*      IF JBoxServerAPI:Instance:CallServerProc("lager_salg.p",cVgLst + '¤' + cVmLst + '¤' + cProfilLst,hLager) THEN*/
/*        hLager = JBoxServerAPI:Instance:getCallReturnTable().                                                      */
/*      SESSION:SET-WAIT-STATE("").                                                                                  */
/*      RUN avsluttDialog IN hByggerRapport.                                                                         */
/*      IF VALID-HANDLE(hByggerRapport) THEN                                                                         */
/*        DELETE OBJECT hByggerRapport.                                                                              */
    END.    
    
    pcWhere    = (IF cbLagant:SCREEN-VALUE <> '0' THEN '0|' ELSE '|') +
                 (IF cbSolgt%:SCREEN-VALUE <> '0' THEN '0' ELSE '').  
      .
    pcOperator = (IF cbLagant:SCREEN-VALUE = '1' THEN '>,' 
                  ELSE IF cbLagant:SCREEN-VALUE = '2' THEN '<,'
                  ELSE IF cbLagant:SCREEN-VALUE = '3' THEN '<>,'
                  ELSE IF cbLagant:SCREEN-VALUE = '4' THEN '=,'
                  ELSE ',') + 
                  (IF cbSolgt%:SCREEN-VALUE = '1' THEN '>' 
                  ELSE IF cbSolgt%:SCREEN-VALUE = '2' THEN '<'
                  ELSE IF cbSolgt%:SCREEN-VALUE = '3' THEN '<>'
                  ELSE IF cbSolgt%:SCREEN-VALUE = '4' THEN '='
                  ELSE '')
      .
    /* TN 12/6-21 Når programmet er startet fra kampanje, skal Lager filteret gå mot Lager_AntProfil feltet. */  
    pcFelt = (IF cbLagant:SCREEN-VALUE <> '0' THEN (IF cModus = '20' THEN 'Lager_AntProfil,' ELSE 'Lagant,') ELSE ',') + 
             (IF cbSolgt%:SCREEN-VALUE <> '0' THEN 'Solgt%' ELSE '')
             .

    IF DEC(fiSolgt%Fra:SCREEN-VALUE) <> 0 THEN
      ASSIGN
        pcWhere    = pcWhere + (IF pcWhere <> '' THEN '|' ELSE '') + fiSolgt%Fra:SCREEN-VALUE
        pcOperator = pcOperator + (IF pcOperator <> '' THEN ',' ELSE '') + '>='
        pcFelt     = pcFelt + (IF pcFelt <> '' THEN ',' ELSE '') + 'Solgt%'  
      . 

    IF DEC(fiSolgt%Til:SCREEN-VALUE) <> 0 THEN
      ASSIGN
        pcWhere    = pcWhere + (IF pcWhere <> '' THEN '|' ELSE '') + fiSolgt%Til:SCREEN-VALUE
        pcOperator = pcOperator + (IF pcOperator <> '' THEN ',' ELSE '') + '<='
        pcFelt     = pcFelt + (IF pcFelt <> '' THEN ',' ELSE '') + 'Solgt%'  
      . 

    oBrwLager:setFilter(pcFelt,pcOperator,pcWhere).
  END.  
  oBrwLager:OpenQuery().
  APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValgtVareRecord C-Win 
PROCEDURE ValgtVareRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

  IF oBrwLager:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  MARKERTEPOSTER:
  DO:    
    IF NOT CAN-DO('20',cModus) THEN 
    DO:    
      iKampanjeId = 0.
      RUN velgKampanje.w (OUTPUT pcTekst).
      IF pcTekst = ? OR pcTekst = '' OR INT(pcTekst) = 0 THEN
        RETURN.
      ELSE 
        iKampanjeId = INT(pcTekst).
    END.
    oBrwLager:processRowsNoMessage("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).
    PUBLISH "OpenQueryKampanjeLinje".      
      
    IF NOT CAN-DO('20',cModus) THEN 
      iKampanjeId = 0.      
  END. /* MARKERTEPOSTER */
  ELSE DO:
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal alle varer i utvalget sendes til kampanje?") THEN 
        RETURN.    
    ELSE DO:
      /* Ved å gjøre dette, får vi bare postene i view-porten. */
      oBrwLager:BROWSE-HANDLE:SELECT-ALL().
      oBrwLager:processRowsNoMessage("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).
      /* Ved å gjøre dette får vi med alle postene i spørringen. Ikke begrenset av filteret. */
/*      oBrwLager:processSet("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).*/
      PUBLISH "OpenQueryKampanjeLinje".      
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

