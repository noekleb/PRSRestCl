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
DEFINE VARIABLE hArtPris AS HANDLE NO-UNDO.
DEFINE VARIABLE hByggerRapport AS HANDLE NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cModus AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE opopupArtPris AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwArtPris ***/
DEF VAR oBrwArtPris AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE ArtPris
    FIELD artpris_Beskr AS CHARACTER
    FIELD artpris_LevKod AS CHARACTER
    FIELD artpris_LevFargKod AS CHARACTER
    FIELD artpris_Sesong AS CHARACTER
    FIELD artpris_TilbudFraDato AS DATETIME
    FIELD artpris_TilbudTilDato AS DATETIME
    FIELD Pris_1 AS DECIMAL
    FIELD jbextent_1_Pris AS DECIMAL /* placeholder for calculation */
    FIELD Rab1%_2 AS DECIMAL
    FIELD jbextent_2_Rab1% AS DECIMAL /* placeholder for calculation */
    FIELD Pris_2 AS DECIMAL
    FIELD jbextent_2_Pris AS DECIMAL /* placeholder for calculation */
    FIELD artpris_Solgt% AS DECIMAL
    FIELD artpris_AntSolgt AS INTEGER
    FIELD artpris_VerdiSolgt AS DECIMAL
    FIELD artpris_LagAnt AS DECIMAL
    FIELD artpris_LagVerdi AS DECIMAL
    FIELD artpris_Varemerke AS CHARACTER
    FIELD artpris_Produsent AS CHARACTER
    FIELD artpris_Varegruppe AS CHARACTER
    FIELD artpris_Hovedgruppe AS CHARACTER
    FIELD ProfilNr AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD artpris_Dummy AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_ArtPris FOR TEMP-TABLE ArtPris.


FUNCTION getBuffersAndFieldsBrwArtPris RETURNS CHARACTER():
  RETURN
    'ArtPris'
     + ';Pris[1]'
     + ';Rab1%[2]'
     + ';Pris[2]'
     + ';ProfilNr'
     + ';ArtikkelNr'
     + ';+artpris_Beskr|CHARACTER||artpris_Beskr(ArtikkelNr)|Varetekst'
     + ';+artpris_LevKod|CHARACTER||artpris_LevKod(ArtikkelNr)|Lev.artikkelnr'
     + ';+artpris_LevFargKod|CHARACTER||artpris_LevFargKod(ArtikkelNr)|Lev.farkgekode'
     + ';+artpris_Sesong|CHARACTER||artpris_Sesong(ROWID)|Sesong'
     + ';+artpris_TilbudFraDato|DATETIME||artpris_TilbudFraDato(ROWID)|TilbudFraDato'
     + ';+artpris_TilbudTilDato|DATETIME||artpris_TilbudTilDato(ROWID)|TilbudTilDato'
     + ';+artpris_Solgt%|DECIMAL||artpris_Solgt%(ROWID)|Solgt%'
     + ';+artpris_AntSolgt|INTEGER||artpris_AntSolgt(ROWID)|AntSolgt'
     + ';+artpris_VerdiSolgt|DECIMAL||artpris_VerdiSolgt(ROWID)|VerdiSolgt'
     + ';+artpris_LagAnt|DECIMAL||artpris_LagAnt(ROWID)|Lager'
     + ';+artpris_LagVerdi|DECIMAL||artpris_LagVerdi(ROWID)|Lager verdi'
     + ';+artpris_Varemerke|CHARACTER||artpris_Varemerke(ROWID)|Varemerke'
     + ';+artpris_Produsent|CHARACTER||artpris_Produsent(ROWID)|Produsent'
     + ';+artpris_Varegruppe|CHARACTER||artpris_Varegruppe(ROWID)|Varegruppe'
     + ';+artpris_Hovedgruppe|CHARACTER||artpris_Hovedgruppe(ROWID)|Hovedgruppe'
     + ';+artpris_Dummy|CHARACTER||artpris_Dummy(ROWID)|.'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwArtPris RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwArtPris RETURNS CHARACTER():
  RETURN 
     'server/KampanjeSalg_brwcalc.p' /* artpris_Beskr(ArtikkelNr) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_LevKod(ArtikkelNr) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_LevFargKod(ArtikkelNr) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Sesong(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_TilbudFraDato(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_TilbudTilDato(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Solgt%(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_AntSolgt(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_VerdiSolgt(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_LagAnt(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_LagVerdi(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Varemerke(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Produsent(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Varegruppe(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Hovedgruppe(ROWID) */
   + ',server/KampanjeSalg_brwcalc.p' /* artpris_Dummy(ROWID) */
     .
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
&Scoped-define BROWSE-NAME BrwArtPris

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtPris

/* Definitions for BROWSE BrwArtPris                                    */
&Scoped-define FIELDS-IN-QUERY-BrwArtPris ArtPris.artpris_Beskr ~
ArtPris.artpris_LevKod ArtPris.artpris_LevFargKod ArtPris.artpris_Sesong ~
ArtPris.artpris_TilbudFraDato ArtPris.artpris_TilbudTilDato ArtPris.Pris_1 ~
ArtPris.Rab1%_2 ArtPris.Pris_2 ArtPris.artpris_Solgt% ~
ArtPris.artpris_AntSolgt ArtPris.artpris_VerdiSolgt ArtPris.artpris_LagAnt ~
ArtPris.artpris_LagVerdi ArtPris.artpris_Varemerke ~
ArtPris.artpris_Produsent ArtPris.artpris_Varegruppe ~
ArtPris.artpris_Hovedgruppe ArtPris.ProfilNr ArtPris.ArtikkelNr ~
ArtPris.artpris_Dummy 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwArtPris ArtPris.artpris_Beskr 
&Scoped-define QUERY-STRING-BrwArtPris FOR EACH ArtPris NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwArtPris OPEN QUERY BrwArtPris FOR EACH ArtPris NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwArtPris ArtPris
&Scoped-define FIRST-TABLE-IN-QUERY-BrwArtPris ArtPris


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbLager RECT-1 searchArtPris first_tbLager ~
prev_tbLager next_tbLager last_tbLager refresh_tbLager excel_tbLager ~
filter_tbLager cbSolgt% cbLagant btnBlank fiSolgt%Fra fiSolgt%Til ~
BrwArtPris 
&Scoped-Define DISPLAYED-OBJECTS cbSolgt% cbLagant fiSolgt%Fra fiSolgt%Til 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBlank 
     LABEL "<Blank>" 
     SIZE 15 BY 1.14 TOOLTIP "Renser filter og åpner spørringen igjen.".

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

DEFINE VARIABLE fiSolgt%Fra AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     LABEL "Fra Solgt%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fra og med Solgt%" NO-UNDO.

DEFINE VARIABLE fiSolgt%Til AS DECIMAL FORMAT "->>,>>9.9":U INITIAL 0 
     LABEL "Til Solgt%" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Til og med Solgt%" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.38.

DEFINE RECTANGLE searchArtPris
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30 BY .91.

DEFINE RECTANGLE tbLager
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 232 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwArtPris FOR 
      ArtPris SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwArtPris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwArtPris C-Win _STRUCTURED
  QUERY BrwArtPris NO-LOCK DISPLAY
      ArtPris.artpris_Beskr COLUMN-LABEL "Varetekst" FORMAT "X(40)":U
            WIDTH 44.2
      ArtPris.artpris_LevKod COLUMN-LABEL "Lev.artikkelnr" FORMAT "X(30)":U
            WIDTH 23.2
      ArtPris.artpris_LevFargKod COLUMN-LABEL "Lev.farkgekode" FORMAT "X(30)":U
            WIDTH 23.2
      ArtPris.artpris_Sesong COLUMN-LABEL "Sesong" FORMAT "X(30)":U
      ArtPris.artpris_TilbudFraDato COLUMN-LABEL "TilbudFraDato" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 22
      ArtPris.artpris_TilbudTilDato COLUMN-LABEL "TilbudTilDato" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 22
      ArtPris.Pris_1 COLUMN-LABEL "Pris" FORMAT "->,>>>,>>9.99":U
      ArtPris.Rab1%_2 COLUMN-LABEL "%Rabatt 1" FORMAT "->>9.99":U
      ArtPris.Pris_2 COLUMN-LABEL "Pris" FORMAT "->,>>>,>>9.99":U
            WIDTH 13.6
      ArtPris.artpris_Solgt% COLUMN-LABEL "Solgt%" FORMAT "->>>9.9":U
            WIDTH 10
      ArtPris.artpris_AntSolgt COLUMN-LABEL "AntSolgt" FORMAT "->,>>>,>>9":U
      ArtPris.artpris_VerdiSolgt COLUMN-LABEL "VerdiSolgt" FORMAT "->>,>>>,>>9.99":U
      ArtPris.artpris_LagAnt COLUMN-LABEL "Lager" FORMAT "->>>,>>9":U
      ArtPris.artpris_LagVerdi COLUMN-LABEL "Lager verdi" FORMAT "->>,>>>,>>9":U
            WIDTH 15.2
      ArtPris.artpris_Varemerke COLUMN-LABEL "Varemerke" FORMAT "X(30)":U
      ArtPris.artpris_Produsent COLUMN-LABEL "Produsent" FORMAT "X(30)":U
      ArtPris.artpris_Varegruppe COLUMN-LABEL "Varegruppe" FORMAT "X(30)":U
      ArtPris.artpris_Hovedgruppe COLUMN-LABEL "Hovedgruppe" FORMAT "X(30)":U
      ArtPris.ProfilNr COLUMN-LABEL "Prisprofil" FORMAT ">>>>>>9":U
      ArtPris.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 18
      ArtPris.artpris_Dummy COLUMN-LABEL "." FORMAT "X(1)":U
  ENABLE
      ArtPris.artpris_Beskr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 232 BY 15.95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbLager AT ROW 1.29 COL 2.2 WIDGET-ID 4
     prev_tbLager AT ROW 1.29 COL 7 WIDGET-ID 6
     next_tbLager AT ROW 1.29 COL 11.6 WIDGET-ID 8
     last_tbLager AT ROW 1.29 COL 16.2 WIDGET-ID 10
     refresh_tbLager AT ROW 1.29 COL 20.8 WIDGET-ID 12
     excel_tbLager AT ROW 1.29 COL 25.4 WIDGET-ID 14
     filter_tbLager AT ROW 1.29 COL 30 WIDGET-ID 16
     cbSolgt% AT ROW 1.33 COL 175.8 COLON-ALIGNED WIDGET-ID 38
     cbLagant AT ROW 1.33 COL 211 COLON-ALIGNED WIDGET-ID 36
     btnBlank AT ROW 3.19 COL 106.6 WIDGET-ID 50
     fiSolgt%Fra AT ROW 3.29 COL 58 COLON-ALIGNED
     fiSolgt%Til AT ROW 3.29 COL 85 COLON-ALIGNED
     BrwArtPris AT ROW 4.57 COL 2 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.48 COL 44 WIDGET-ID 48
          FONT 6
     tbLager AT ROW 1.19 COL 2 WIDGET-ID 2
     RECT-1 AT ROW 3.1 COL 43 WIDGET-ID 44
     searchArtPris AT ROW 3.52 COL 2.8 WIDGET-ID 56
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 233.4 BY 19.57 WIDGET-ID 100.


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
         HEIGHT             = 19.57
         WIDTH              = 233.4
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 233.4
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 233.4
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
/* BROWSE-TAB BrwArtPris fiSolgt%Til DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.57
       FRAME DEFAULT-FRAME:WIDTH            = 233.4.

ASSIGN 
       tbLager:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,excel;Eksporter til E&xcel,filter;Filtermaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwArtPris
/* Query rebuild information for BROWSE BrwArtPris
     _TblList          = "SkoTex.ArtPris"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"ArtPris.artpris_Beskr" "Varetekst" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "" no no "44.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"ArtPris.artpris_LevKod" "Lev.artikkelnr" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"ArtPris.artpris_LevFargKod" "Lev.farkgekode" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"ArtPris.artpris_Sesong" "Sesong" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"ArtPris.artpris_TilbudFraDato" "TilbudFraDato" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"ArtPris.artpris_TilbudTilDato" "TilbudTilDato" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"ArtPris.Pris_1" "Pris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris inkl. mva." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"ArtPris.Rab1%_2" "%Rabatt 1" "->>9.99" "DECIMAL" ? ? ? ? ? ? no "Rabatt 1 prosent" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"ArtPris.Pris_2" "Pris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris inkl. mva." no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"ArtPris.artpris_Solgt%" "Solgt%" "->>>9.9" "DECIMAL" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"ArtPris.artpris_AntSolgt" "AntSolgt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"ArtPris.artpris_VerdiSolgt" "VerdiSolgt" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"ArtPris.artpris_LagAnt" "Lager" "->>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"ArtPris.artpris_LagVerdi" "Lager verdi" "->>,>>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "15.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"ArtPris.artpris_Varemerke" "Varemerke" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"ArtPris.artpris_Produsent" "Produsent" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"ArtPris.artpris_Varegruppe" "Varegruppe" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"ArtPris.artpris_Hovedgruppe" "Hovedgruppe" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"ArtPris.ProfilNr" "Prisprofil" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Prisprofil" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"ArtPris.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"ArtPris.artpris_Dummy" "." "X(1)" "CHARACTER" ? ? ? ? ? ? no "" no no "1" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME btnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlank C-Win
ON CHOOSE OF btnBlank IN FRAME DEFAULT-FRAME /* <Blank> */
DO:
  ASSIGN 
    fiSolgt%Fra:SCREEN-VALUE = ''
    fisolgt%Til:SCREEN-VALUE = ''
    .
  RUN setfilter.  
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
ON RETURN OF fiSolgt%Fra IN FRAME DEFAULT-FRAME /* Fra Solgt% */
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Fra C-Win
ON TAB OF fiSolgt%Fra IN FRAME DEFAULT-FRAME /* Fra Solgt% */
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSolgt%Til
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Til C-Win
ON RETURN OF fiSolgt%Til IN FRAME DEFAULT-FRAME /* Til Solgt% */
DO:
    RUN setfilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSolgt%Til C-Win
ON TAB OF fiSolgt%Til IN FRAME DEFAULT-FRAME /* Til Solgt% */
DO:
    RUN setfilter.  
  
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
{incl/conttrigg.i oBrwArtpris:BROWSE-HANDLE}
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
  DISPLAY cbSolgt% cbLagant fiSolgt%Fra fiSolgt%Til 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbLager RECT-1 searchArtPris first_tbLager prev_tbLager next_tbLager 
         last_tbLager refresh_tbLager excel_tbLager filter_tbLager cbSolgt% 
         cbLagant btnBlank fiSolgt%Fra fiSolgt%Til BrwArtPris 
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
    iButNr = INT(ENTRY(1,icParam,'|'))
    cModus = ENTRY(2,icParam,'|') /*10=Fra meny, 20=Fra kampanjeregister */
    .
  IF NUM-ENTRIES(icParam,'|') >= 3 THEN 
    iKampanjeId = INT(ENTRY(3,icParam,'|')).

  oBrwArtPris = NEW JBoxBrowse(brwArtPris:HANDLE).
  oBrwArtPris:useLocalData = YES.

  otbLager = NEW JBoxToolbar(tbLager:HANDLE).
  oBrwArtPris:TOOLBAR-OBJECT = otbLager.
  otbLager:addToolGroup("ValgtVare;Legg markerte varer til kampanje...").

  oContainer:setNoResizeY("RECT-1").
  
  hArtPris = BUFFER ArtPris:HANDLE.
  RUN ByggerRapport.w PERSISTENT SET hByggerRapport.
  RUN MoveToTop IN hByggerRapport.
  RUN vistekst IN hByggerRapport.
  SESSION:SET-WAIT-STATE("GENERAL").  
  IF JBoxServerAPI:Instance:CallServerProc("artpris_kampanje.p",'',hArtPris) THEN
    hArtPris = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
  RUN avsluttDialog IN hByggerRapport.
  IF VALID-HANDLE(hByggerRapport) THEN 
    DELETE OBJECT hByggerRapport.
  IF CAN-DO('20',cModus) THEN 
    ASSIGN 
      cbLagant:SCREEN-VALUE = '1'
      cbLagant              = 1
      .
  ELSE 
    ASSIGN 
      cbLagant:SCREEN-VALUE = '1'
      cbLagant              = 1
      .
  oBrwArtPris:setSearchField(searchArtPris:HANDLE,"artpris_LevKod").
  
  opopupArtPris = NEW JBoxPopupMenu().
  opopupArtPris:AddToolGroup('OppslagModell;Vis i modell liste'  
                           ).
  oBrwArtPris:POPUP-MENU-OBJECT = opopupArtPris.
  
  /* Inneholder OpenQuery. */
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
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppslagModellRecord C-Win
PROCEDURE OppslagModellRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE ArtPris THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',ArtPris.ArtPris_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',ArtPris.ArtPris_LevFargKod)
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
hArtPris = BUFFER ArtPris:HANDLE.
  RUN ByggerRapport.w PERSISTENT SET hByggerRapport.
  RUN MoveToTop IN hByggerRapport.
  RUN vistekst IN hByggerRapport.
  SESSION:SET-WAIT-STATE("GENERAL").  
  IF JBoxServerAPI:Instance:CallServerProc("artpris_kampanje.p",'',hArtPris) THEN
    hArtPris = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
  RUN avsluttDialog IN hByggerRapport.
  IF VALID-HANDLE(hByggerRapport) THEN 
    DELETE OBJECT hByggerRapport.

  oBrwArtPris:OpenQuery().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowsToBatchRecord C-Win 
PROCEDURE rowsToBatchRecord :
RUN SUPER.


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
    pcWhere    = (IF cbLagant:SCREEN-VALUE <> '0' THEN '0|' ELSE '|') +
                 (IF cbSolgt%:SCREEN-VALUE <> '0' THEN '0' ELSE '')  
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
    pcFelt = (IF cbLagant:SCREEN-VALUE <> '0' THEN 'ArtPris_Lagant,' ELSE ',') + 
             (IF cbSolgt%:SCREEN-VALUE <> '0' THEN 'ArtPris_Solgt%' ELSE '')
             .
             
    IF DEC(fiSolgt%Fra:SCREEN-VALUE) <> 0 THEN
      ASSIGN
        pcWhere    = pcWhere + (IF pcWhere <> '' THEN '|' ELSE '') + fiSolgt%Fra:SCREEN-VALUE
        pcOperator = pcOperator + (IF pcOperator <> '' THEN ',' ELSE '') + '>='
        pcFelt     = pcFelt + (IF pcFelt <> '' THEN ',' ELSE '') + 'artpris_Solgt%'  
      . 

    IF DEC(fiSolgt%Til:SCREEN-VALUE) <> 0 THEN
      ASSIGN
        pcWhere    = pcWhere + (IF pcWhere <> '' THEN '|' ELSE '') + fiSolgt%Til:SCREEN-VALUE
        pcOperator = pcOperator + (IF pcOperator <> '' THEN ',' ELSE '') + '<='
        pcFelt     = pcFelt + (IF pcFelt <> '' THEN ',' ELSE '') + 'artpris_Solgt%'  
      . 
             
    oBrwArtPris:setFilter(pcFelt,pcOperator,pcWhere).
  END.  
  oBrwArtPris:OpenQuery().
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

  IF oBrwArtPris:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
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
    
    oBrwArtPris:processRowsNoMessage("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).
    PUBLISH "OpenQueryKampanjeLinje".
    
    IF NOT CAN-DO('20',cModus) THEN 
      iKampanjeId = 0.
  END.  
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker en eller flere rader først! ").
    RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

