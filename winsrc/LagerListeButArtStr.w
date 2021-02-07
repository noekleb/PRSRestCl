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
/*          This .W file was created with the Progress AppBuilde&Scoped-define SELF-NAME DEFAULT-FRAME
r.      */
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

DEFINE VARIABLE bOk                   AS LOG                              NO-UNDO. 
DEFINE VARIABLE ix                    AS INTEGER                          NO-UNDO. 
DEFINE VARIABLE hBrowse               AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hQuery                AS HANDLE                           NO-UNDO. 
DEFINE VARIABLE hToolbar              AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hFieldMap             AS HANDLE                           NO-UNDO.
DEFINE VARIABLE oContainer            AS JBoxContainer                    NO-UNDO.
DEFINE VARIABLE cWhere                AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cOperator             AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE iButNr                AS INTEGER                          NO-UNDO.
DEFINE VARIABLE iTilbut               AS INTEGER                          NO-UNDO.
DEFINE VARIABLE iBuntNr               AS INTEGER                          NO-UNDO.
DEFINE VARIABLE cModus                AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cRowId                AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE iKampanjeId           AS INTEGER                          FORMAT ">>>>>>>>9" NO-UNDO. 
DEFINE VARIABLE lPrisAvvik            AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE lPris                 AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE lMaksPris             AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE lMinPris              AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE iLevNr                AS INTEGER                          NO-UNDO.
DEFINE VARIABLE cbEndret              AS LOG                              NO-UNDO.
DEFINE VARIABLE cLagerkoder           AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE iLoop                 AS INTEGER                          NO-UNDO.
DEFINE VARIABLE cTekst                AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE hArtLag_NOSColumn     AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hArtLag_KodeColumn    AS HANDLE                           NO-UNDO.
DEFINE VARIABLE lPkSdlId              AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE cKode                 AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE iLinjeNr              AS INTEGER                          NO-UNDO.
DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE httImpFilLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE opopupModell AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE hArtLag_TilbudsPrisColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hArtLag AS HANDLE NO-UNDO.
DEFINE VARIABLE hByggerRapport AS HANDLE NO-UNDO.

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

{ cls\StdFunk\dsttImpFil.i }

/*** Start instance property definitions for JBoxBrowse object oBrwArtLag ***/
DEFINE VARIABLE oBrwArtLag            AS JBoxBrowse                       NO-UNDO.
DEF TEMP-TABLE ArtLag
    FIELD butik AS INTEGER
    FIELD ArtLag_VmId AS INTEGER
    FIELD ArtLag_Varemerke AS CHARACTER
    FIELD ArtLag_NOS AS CHARACTER
    FIELD ArtLag_LevNr AS INTEGER
    FIELD ArtLag_Kode AS CHARACTER
    FIELD ArtLag_Beskr AS CHARACTER
    FIELD ArtLag_LevKod AS CHARACTER
    FIELD ArtLag_LevFargKod AS CHARACTER
    FIELD storl AS CHARACTER
    FIELD lagant AS DECIMAL
    FIELD ArtLag_Sasong AS INTEGER
    FIELD ArtLag_Pris AS DECIMAL
    FIELD ArtLag_Tilbud AS CHARACTER
    FIELD ArtLag_TilbPris AS DECIMAL
    FIELD ArtLag_KampRab% AS DECIMAL
    FIELD ArtLag_Reservert AS DECIMAL
    FIELD ArtLag_Varekost AS DECIMAL
    FIELD ArtLag_Rab% AS DECIMAL
    FIELD ArtLag_VVareKost AS DECIMAL
    FIELD ArtBas_WebButikkArtikkel AS LOGICAL
    FIELD ArtBas_PubliserINettButikk AS LOGICAL
    FIELD EndretDatoTid AS DATETIME
    FIELD ArtikkelNr AS DECIMAL
    FIELD StrKode AS INTEGER
    FIELD ArtLag_Recid AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
  .

/* ************************  Function Implementations ***************** */



FUNCTION getBuffersAndFieldsBrwArtLag RETURNS CHARACTER():
  RETURN
    'ArtLag'
     + ';butik'
     + ';storl'
     + ';lagant'
     + ';EndretDatoTid'
     + ';ArtikkelNr'
     + ';StrKode'
     + ';+ArtLag_VmId|INTEGER||ArtLag_VmId|Vm.Id'
     + ';+ArtLag_Varemerke|CHARACTER||ArtLag_Varemerke|Varemerke'
     + ';+ArtLag_NOS|CHARACTER||ArtLag_NOS|Lagerkoder'
     + ';+ArtLag_LevNr|INTEGER||ArtLag_LevNr|Levnr'
     + ';+ArtLag_Kode|CHARACTER||ArtLag_Kode|Strekkode'
     + ';+ArtLag_Beskr|CHARACTER||ArtLag_Beskr|Varetekst'
     + ';+ArtLag_LevKod|CHARACTER||ArtLag_LevKod|Lev.artikkelnr'
     + ';+ArtLag_LevFargKod|CHARACTER||ArtLag_LevFargKod|Lev.fargekode'
     + ';+ArtLag_Sasong|INTEGER||ArtLag_Sasong|Sesong'
     + ';+ArtLag_Pris|DECIMAL||ArtLag_Pris|Pris'
     + ';+ArtLag_Tilbud|CHARACTER||ArtLag_Tilbud|Tilbud'
     + ';+ArtLag_TilbPris|DECIMAL||ArtLag_TilbPris|TilbudsPris'
     + ';+ArtLag_KampRab%|DECIMAL||ArtLag_KampRab%|Rab%'
     + ';+ArtLag_Reservert|DECIMAL||ArtLag_Reservert|Reservert'
     + ';+ArtLag_Varekost|DECIMAL||ArtLag_Varekost|Varekost'
     + ';+ArtLag_Rab%|DECIMAL||ArtLag_Rab%|Rab%'
     + ';+ArtLag_VVareKost|DECIMAL||ArtLag_VVareKost|Veid kost'
     + ';+ArtBas_WebButikkArtikkel|LOGICAL||ArtBas_WebButikkArtikkel|Nettbutikk'
     + ';+ArtBas_PubliserINettButikk|LOGICAL||ArtBas_PubliserINettButikk|Publiser'
     + ';+ArtLag_Recid|CHARACTER||ArtLag_Recid|ArtLag_Recid'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwArtLag RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwArtLag RETURNS CHARACTER():
  RETURN 
     'server/artlag_brwcalc.p' /* ArtLag_VmId */
   + ',server/artlag_brwcalc.p' /* ArtLag_Varemerke */
   + ',server/artlag_brwcalc.p' /* ArtLag_NOS */
   + ',server/artlag_brwcalc.p' /* ArtLag_LevNr */
   + ',server/artlag_brwcalc.p' /* ArtLag_Kode */
   + ',server/artlag_brwcalc.p' /* ArtLag_Beskr */
   + ',server/artlag_brwcalc.p' /* ArtLag_LevKod */
   + ',server/artlag_brwcalc.p' /* ArtLag_LevFargKod */
   + ',server/artlag_brwcalc.p' /* ArtLag_Sasong */
   + ',server/artlag_brwcalc.p' /* ArtLag_Pris */
   + ',server/artlag_brwcalc.p' /* ArtLag_Tilbud */
   + ',server/artlag_brwcalc.p' /* ArtLag_TilbPris */
   + ',server/artlag_brwcalc.p' /* ArtLag_KampRab% */
   + ',server/artlag_brwcalc.p' /* ArtLag_Reservert */
   + ',server/artlag_brwcalc.p' /* ArtLag_Varekost */
   + ',server/artlag_brwcalc.p' /* ArtLag_Rab% */
   + ',server/artlag_brwcalc.p' /* ArtLag_VVareKost */
   + ',server/artlag_brwcalc.p' /* ArtBas_WebButikkArtikkel */
   + ',server/artlag_brwcalc.p' /* ArtBas_PubliserINettButikk */
   + ',server/artlag_brwcalc.p' /* ArtLag_Recid */
     .
END FUNCTION.


DEFINE VARIABLE otbArtLag AS JBoxToolbar   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwArtLag

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtLag

/* Definitions for BROWSE BrwArtLag                                     */
&Scoped-define FIELDS-IN-QUERY-BrwArtLag ArtLag.butik ArtLag.ArtLag_VmId ~
ArtLag.ArtLag_Varemerke ArtLag.ArtLag_NOS ArtLag.ArtLag_LevNr ~
ArtLag.ArtLag_Kode ArtLag.ArtLag_Beskr ArtLag.ArtLag_LevKod ~
ArtLag.ArtLag_LevFargKod ArtLag.storl ArtLag.lagant ArtLag.ArtLag_Sasong ~
ArtLag.ArtLag_Pris ArtLag.ArtLag_Tilbud ArtLag.ArtLag_TilbPris ~
ArtLag.ArtLag_KampRab% ArtLag.ArtLag_Reservert ArtLag.ArtLag_Varekost ~
ArtLag.ArtLag_Rab% ArtLag.ArtLag_VVareKost ArtLag.ArtBas_WebButikkArtikkel ~
ArtLag.ArtBas_PubliserINettButikk ArtLag.EndretDatoTid ArtLag.ArtikkelNr ~
ArtLag.StrKode ArtLag.ArtLag_Recid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwArtLag ArtLag.butik 
&Scoped-define QUERY-STRING-BrwArtLag FOR EACH ArtLag NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwArtLag OPEN QUERY BrwArtLag FOR EACH ArtLag NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwArtLag ArtLag
&Scoped-define FIRST-TABLE-IN-QUERY-BrwArtLag ArtLag


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbArtLag searchArtLag RECT-3 first_tbArtLag ~
prev_tbArtLag next_tbArtLag last_tbArtLag refresh_tbArtLag filter_tbArtLag ~
browseconfig_tbArtLag excel_tbArtLag close_tbArtLag fiBeskr fiLevKod ~
fiLevFargKod fiStr cbLagant BtnBlank fiKode cbLagerkoder BrwArtLag 
&Scoped-Define DISPLAYED-OBJECTS fiBeskr fiLevKod fiLevFargKod fiStr ~
cbLagant fiKode cbLagerkoder 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbArtLag 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Column setup" 
     SIZE 4.6 BY 1.1 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON BtnBlank 
     LABEL "<Blank filter>" 
     SIZE 13.8 BY 1.14 TOOLTIP "Blanker filter".

DEFINE BUTTON close_tbArtLag 
     IMAGE-UP FILE "bmp/e-exit.bmp":U
     LABEL "Close" 
     SIZE 4.6 BY 1.1 TOOLTIP "Close (ALT-C)".

DEFINE BUTTON excel_tbArtLag 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbArtLag 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbArtLag 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbArtLag 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbArtLag 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbArtLag 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbArtLag 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbLagant AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Vis lager" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "<Alle>",0,
                     "Har lager",1,
                     "Har ikke lager",2,
                     "Ulik 0",3,
                     "Lik 0",4
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE cbLagerkoder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lagerkoder" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "","",
                     "<Merkede>","MERKET",
                     "NOS","NOS",
                     "K","K",
                     "L","L"
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 TOOLTIP "Filter på varetekst. Jokertegn '*' kan benytts før, i og etter tekst." NO-UNDO.

DEFINE VARIABLE fiKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 TOOLTIP "Ved søk på strekkode, hentes modellen frem." NO-UNDO.

DEFINE VARIABLE fiLevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 TOOLTIP "Filter på lev. fargekode." NO-UNDO.

DEFINE VARIABLE fiLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Art.nr" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 TOOLTIP "Filter på leverandørs artikkelnr." NO-UNDO.

DEFINE VARIABLE fiStr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Str." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Filter på størrelse." NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 198 BY 2.86.

DEFINE RECTANGLE searchArtLag
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.8 BY 1.

DEFINE RECTANGLE tbArtLag
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 229.8 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwArtLag FOR 
      ArtLag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwArtLag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwArtLag C-Win _STRUCTURED
  QUERY BrwArtLag NO-LOCK DISPLAY
      ArtLag.butik COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      ArtLag.ArtLag_VmId COLUMN-LABEL "Vm.Id" FORMAT ">>>>>>>9":U
      ArtLag.ArtLag_Varemerke COLUMN-LABEL "Varemerke" FORMAT "X(8)":U
      ArtLag.ArtLag_NOS COLUMN-LABEL "Lagerkoder" FORMAT "X(40)":U
            WIDTH 8
      ArtLag.ArtLag_LevNr COLUMN-LABEL "Levnr" FORMAT ">>>>>9":U
      ArtLag.ArtLag_Kode COLUMN-LABEL "Strekkode" FORMAT "X(16)":U
      ArtLag.ArtLag_Beskr COLUMN-LABEL "Varetekst" FORMAT "X(40)":U
            WIDTH 49.8
      ArtLag.ArtLag_LevKod COLUMN-LABEL "Lev.artikkelnr" FORMAT "X(25)":U
      ArtLag.ArtLag_LevFargKod COLUMN-LABEL "Lev.fargekode" FORMAT "X(8)":U
      ArtLag.storl COLUMN-LABEL "Str." FORMAT "x(10)":U
      ArtLag.lagant COLUMN-LABEL "Antall" FORMAT "->>>,>>9":U
      ArtLag.ArtLag_Sasong COLUMN-LABEL "Sesong" FORMAT ">>>>>>9":U
            WIDTH 8
      ArtLag.ArtLag_Pris COLUMN-LABEL "Pris" FORMAT "->,>>>,>>9.99":U
            WIDTH 8
      ArtLag.ArtLag_Tilbud COLUMN-LABEL "Tilbud" FORMAT "X(8)":U
            WIDTH 6.6
      ArtLag.ArtLag_TilbPris COLUMN-LABEL "TilbudsPris" FORMAT "->>>,>>9.99":U
      ArtLag.ArtLag_KampRab% COLUMN-LABEL "Rab%" FORMAT "->>>,>>9.99":U
      ArtLag.ArtLag_Reservert COLUMN-LABEL "Reservert" FORMAT "->>>,>>9":U
      ArtLag.ArtLag_Varekost COLUMN-LABEL "Varekost" FORMAT "->,>>>,>>9.99":U
            WIDTH 8.4
      ArtLag.ArtLag_Rab% COLUMN-LABEL "Rab%" FORMAT "->,>>9.99":U
            WIDTH 8
      ArtLag.ArtLag_VVareKost COLUMN-LABEL "Veid kost" FORMAT "->,>>>,>>9.99":U
            WIDTH 8.8
      ArtLag.ArtBas_WebButikkArtikkel COLUMN-LABEL "Nettbutikk" FORMAT "Ja/Nei":U
      ArtLag.ArtBas_PubliserINettButikk COLUMN-LABEL "Publiser" FORMAT "Ja/Nei":U
      ArtLag.EndretDatoTid COLUMN-LABEL "Endret dato/tid" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 23
      ArtLag.ArtikkelNr COLUMN-LABEL "PRS art.nr." FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      ArtLag.StrKode COLUMN-LABEL "Num storl" FORMAT ">>>9":U
      ArtLag.ArtLag_Recid COLUMN-LABEL "ArtLag_Recid" FORMAT "X(40)":U
  ENABLE
      ArtLag.butik HELP "Butikknummer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE DROP-TARGET SIZE 230 BY 13.33 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbArtLag AT ROW 1.57 COL 2.4 WIDGET-ID 4
     prev_tbArtLag AT ROW 1.57 COL 7.2 WIDGET-ID 6
     next_tbArtLag AT ROW 1.57 COL 11.8 WIDGET-ID 8
     last_tbArtLag AT ROW 1.57 COL 16.4 WIDGET-ID 10
     refresh_tbArtLag AT ROW 1.57 COL 21 WIDGET-ID 12
     filter_tbArtLag AT ROW 1.57 COL 25.6 WIDGET-ID 14
     browseconfig_tbArtLag AT ROW 1.57 COL 30.2 WIDGET-ID 42
     excel_tbArtLag AT ROW 1.57 COL 34.8 WIDGET-ID 18
     close_tbArtLag AT ROW 1.57 COL 39.4 WIDGET-ID 40
     fiBeskr AT ROW 3.43 COL 43.4 COLON-ALIGNED
     fiLevKod AT ROW 3.43 COL 73.2 COLON-ALIGNED
     fiLevFargKod AT ROW 3.43 COL 103.6 COLON-ALIGNED
     fiStr AT ROW 3.43 COL 126.8 COLON-ALIGNED
     cbLagant AT ROW 3.43 COL 155.8 COLON-ALIGNED WIDGET-ID 36
     BtnBlank AT ROW 4.57 COL 178.6 WIDGET-ID 32
     fiKode AT ROW 4.62 COL 43.4 COLON-ALIGNED
     cbLagerkoder AT ROW 4.62 COL 155.8 COLON-ALIGNED WIDGET-ID 38
     BrwArtLag AT ROW 6.24 COL 2 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.67 COL 42.8 RIGHT-ALIGNED WIDGET-ID 24
          FONT 6
     tbArtLag AT ROW 1.38 COL 1.6 WIDGET-ID 2
     searchArtLag AT ROW 4.1 COL 2.2 WIDGET-ID 20
     RECT-3 AT ROW 3.14 COL 34 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 231.8 BY 18.81 WIDGET-ID 100.


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
         TITLE              = "Lagerliste"
         HEIGHT             = 18.81
         WIDTH              = 231.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 231.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 231.8
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
/* BROWSE-TAB BrwArtLag cbLagerkoder DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 18.81
       FRAME DEFAULT-FRAME:WIDTH            = 231.8.

ASSIGN 
       ArtLag.ArtLag_VmId:VISIBLE IN BROWSE BrwArtLag = FALSE
       ArtLag.ArtLag_Varekost:VISIBLE IN BROWSE BrwArtLag = FALSE
       ArtLag.ArtLag_Rab%:VISIBLE IN BROWSE BrwArtLag = FALSE
       ArtLag.ArtLag_VVareKost:VISIBLE IN BROWSE BrwArtLag = FALSE
       ArtLag.ArtikkelNr:VISIBLE IN BROWSE BrwArtLag = FALSE
       ArtLag.StrKode:VISIBLE IN BROWSE BrwArtLag = FALSE.

ASSIGN 
       tbArtLag:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,browseconfig;Column setup,excel;Eksporter til E&xcel,close;Closemaxborder".

/* SETTINGS FOR TEXT-LITERAL "Filter"
          SIZE 8 BY .62 AT ROW 2.67 COL 42.8 RIGHT-ALIGNED              */

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwArtLag
/* Query rebuild information for BROWSE BrwArtLag
     _TblList          = "SkoTex.ArtLag"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"ArtLag.butik" "Butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikknummer" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"ArtLag.ArtLag_VmId" "Vm.Id" ">>>>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "9.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"ArtLag.ArtLag_Varemerke" "Varemerke" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"ArtLag.ArtLag_NOS" "Lagerkoder" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"ArtLag.ArtLag_LevNr" "Levnr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"ArtLag.ArtLag_Kode" "Strekkode" "X(16)" "CHARACTER" ? ? ? ? ? ? no "" no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"ArtLag.ArtLag_Beskr" "Varetekst" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "49.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"ArtLag.ArtLag_LevKod" "Lev.artikkelnr" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"ArtLag.ArtLag_LevFargKod" "Lev.fargekode" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "13.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"ArtLag.storl" "Str." "x(10)" "CHARACTER" ? ? ? ? ? ? no "Stï¿½rrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"ArtLag.lagant" "Antall" "->>>,>>9" "DECIMAL" ? ? ? ? ? ? no "Antall i lager" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"ArtLag.ArtLag_Sasong" "Sesong" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"ArtLag.ArtLag_Pris" "Pris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"ArtLag.ArtLag_Tilbud" "Tilbud" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "6.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"ArtLag.ArtLag_TilbPris" "TilbudsPris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"ArtLag.ArtLag_KampRab%" "Rab%" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"ArtLag.ArtLag_Reservert" "Reservert" "->>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"ArtLag.ArtLag_Varekost" "Varekost" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "8.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"ArtLag.ArtLag_Rab%" "Rab%" "->,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"ArtLag.ArtLag_VVareKost" "Veid kost" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "8.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"ArtLag.ArtBas_WebButikkArtikkel" "Nettbutikk" "Ja/Nei" "LOGICAL" ? ? ? ? ? ? no "" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"ArtLag.ArtBas_PubliserINettButikk" "Publiser" "Ja/Nei" "LOGICAL" ? ? ? ? ? ? no "" no no "7.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"ArtLag.EndretDatoTid" "Endret dato/tid" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "" no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"ArtLag.ArtikkelNr" "PRS art.nr." "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "Artikkelens unike nummer. Tildeles automatisk." no no "14.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"ArtLag.StrKode" "Num storl" ">>>9" "INTEGER" ? ? ? ? ? ? no "Numerisk stï¿½rrelseskode" no no "8.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"ArtLag.ArtLag_Recid" "ArtLag_Recid" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwArtLag */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Lagerliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
  DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Lagerliste */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBlank C-Win
ON CHOOSE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank filter> */
DO:
    RUN blankFilter. 
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBlank C-Win
ON LEAVE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank filter> */
DO:
    APPLY 'ENTRY' TO fiBeskr IN FRAME DEFAULT-FRAME.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLagant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLagant C-Win
ON VALUE-CHANGED OF cbLagant IN FRAME DEFAULT-FRAME /* Vis lager */
DO:
    cbEndret = TRUE.
    RUN setFilter.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLagerkoder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLagerkoder C-Win
ON VALUE-CHANGED OF cbLagerkoder IN FRAME DEFAULT-FRAME /* Lagerkoder */
DO:
    cbEndret = TRUE.
    RUN setFilter.
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeskr C-Win
ON RETURN OF fiBeskr IN FRAME DEFAULT-FRAME /* Varetekst */
DO:
    RUN setFilter.
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeskr C-Win
ON TAB OF fiBeskr IN FRAME DEFAULT-FRAME /* Varetekst */
DO:
    RUN setFilter.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKode C-Win
ON RETURN OF fiKode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  RUN sokStrekkode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiKode C-Win
ON TAB OF fiKode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  RUN sokStrekkode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON RETURN OF fiLevFargKod IN FRAME DEFAULT-FRAME /* Farge */
DO:
    RUN setFilter.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON TAB OF fiLevFargKod IN FRAME DEFAULT-FRAME /* Farge */
DO:
    RUN setFilter.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON RETURN OF fiLevKod IN FRAME DEFAULT-FRAME /* Art.nr */
DO:
    RUN setFilter.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON TAB OF fiLevKod IN FRAME DEFAULT-FRAME /* Art.nr */
DO:
    RUN setFilter.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStr C-Win
ON RETURN OF fiStr IN FRAME DEFAULT-FRAME /* Str. */
DO:
    SELF:SCREEN-VALUE = rclStandardFunksjoner:FixStorl( SELF:SCREEN-VALUE ).
    RUN setFilter.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStr C-Win
ON TAB OF fiStr IN FRAME DEFAULT-FRAME /* Str. */
DO:
    SELF:SCREEN-VALUE = rclStandardFunksjoner:FixStorl( SELF:SCREEN-VALUE ).
    RUN setFilter.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwArtLag
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
ON CLOSE OF THIS-PROCEDURE 
  DO:
    IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
    DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
    PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
    RUN disable_UI.
  END.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
{incl/wintrigg.i}
{incl/conttrigg.i oBrwArtLag:BROWSE-HANDLE}
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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN 
  DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE blankFilter C-Win 
PROCEDURE blankFilter :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DO:
      ASSIGN
        fiBeskr:SCREEN-VALUE      = ''
        fiLevKod:SCREEN-VALUE     = ''
        fiLevFargKod:SCREEN-VALUE = ''
        fiStr:SCREEN-VALUE        = ''
        fiKode:SCREEN-VALUE       = ''
        .
      oBrwArtLag:setFilter('ArtLag_Beskr,ArtLag_LevKod,ArtLag_LevFargKod,Storl',
        ',,,',
        fiBeskr:SCREEN-VALUE + "|" + fiLevKod:SCREEN-VALUE + "|" + fiLevFargKod:SCREEN-VALUE + "|" + fiStr:SCREEN-VALUE 
        ).
      oBrwArtLag:OpenQuery().
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeRecord C-Win 
PROCEDURE closeRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*  DO WITH FRAME {&FRAME-NAME}:*/
  /*  END.                        */
  /*                              */
  /*  RUN SUPER.                  */
  
  DO WITH FRAME {&FRAME-NAME}:
    CASE cModus:
      WHEN '20' OR 
      WHEN '25' OR /* RETUR */ 
      WHEN '27' OR /* BYTTE */
      WHEN '30' OR
      WHEN '45' OR 
      WHEN '40' THEN 
        DO:
          RUN ValgtVareRecord.
        END.
      WHEN '50' OR /* Overføring */
      WHEN '70 'THEN /* Reservasjon */ 
        DO:
          RUN ValgtOverforRecord.
        END.
      WHEN '60' THEN 
        DO:
          RUN ValgtPkSdlRecord.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
IF oBrwArtLag:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}:
  END.
  
  RUN SUPER.
  
  IF oBrwArtLag:isCurrent THEN  
  DO WITH FRAME {&FRAME-NAME}:
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
DEFINE VARIABLE cFileNames AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFilNavn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bNyFil     AS LOG       NO-UNDO.
  DEFINE VARIABLE bNullstill AS LOG       NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    /* Bare når programmet er startet fra menyen skal fil kunne importeres. */
    IF cModus <> '10' THEN 
      RETURN.
      
    ASSIGN 
      bNullstill = FALSE 
      .
      
    IF SELF:NUM-DROPPED-FILES = 1 THEN 
    DO:
      ASSIGN 
        cFilNavn = SELF:GET-DROPPED-FILE(1).
      IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
        MESSAGE "Tillatte filtyper: '.xls,.xlsx'"
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      ELSE 
      DO:
        IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal fil " + cFilNavn + " importeres?") THEN 
          RETURN.
          
        /* Importer fil */
        RUN importerFil (INPUT cFilNavn, 
          OUTPUT iAntLinjer,
          OUTPUT iType, 
          OUTPUT bOk,
          OUTPUT cMsg). /* Les inn filen i en temp tabell/Datasett. */
        IF NOT bOk THEN 
        DO:
          JBoxSession:Instance:ViewMessage(cMsg).
          RETURN.
        END.

        /* Henter ny info til SendtOutlet. */
        ASSIGN
          bNyFil = TRUE /* hantering av temptablar i LesExcel */
          .
          
        IF CAN-FIND(FIRST ttImpFil) THEN 
        DO:
          
          IF JBoxSession:Instance:ViewQuestionOkCancel("Skal tidligere merkede NOS artikler nullstilles før import?") THEN 
            bNullstill = TRUE.
          
          httImpFilLinje = BUFFER ttImpFilLinje:HANDLE.
            
          JBoxServerAPI:Instance:CallServerProc("artlag_sendtimport.p",
            STRING(iType)+ '|' + cLogg + '|' + STRING(bNullstill),
            httImpFilLinje
            ).
          oBrwArtlag:openQuery().
            
          JBoxSession:Instance:ViewMessage("Fil " + cFilNavn + "importert.").
        END.
          
      END.
    END.
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Bare en fil er tillatt importer ad gangen!").
      RETURN NO-APPLY.
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
  DISPLAY fiBeskr fiLevKod fiLevFargKod fiStr cbLagant fiKode cbLagerkoder 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbArtLag searchArtLag RECT-3 first_tbArtLag prev_tbArtLag 
         next_tbArtLag last_tbArtLag refresh_tbArtLag filter_tbArtLag 
         browseconfig_tbArtLag excel_tbArtLag close_tbArtLag fiBeskr fiLevKod 
         fiLevFargKod fiStr cbLagant BtnBlank fiKode cbLagerkoder BrwArtLag 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  IF otbArtLag:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}: 
    IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
    DO:
      IF AVAILABLE ArtLag THEN 
        RUN enkeltEtikett.w (INPUT ArtLag.ArtLag_Kode, 
          INPUT ArtLag.Lagant, 
          INPUT ArtLag.Butik,
          INPUT 0 /* KampanjeId = 0 */
          ).
    END.
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Marker EN rad for etikettutskrift! ").
      RETURN.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFilterTekst C-Win 
PROCEDURE getFilterTekst :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcFilterTekst AS CHARACTER NO-UNDO.

  ASSIGN 
    pcFilterTekst = cFilterTekst
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importerFil C-Win 
PROCEDURE importerFil :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcFilNavn AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pianLinjer AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER piType AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
  DEFINE OUTPUT PARAMETER pcMsg AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE pcLinje   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTempFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cJSonFil  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType1    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType2    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType3    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType4    AS CHARACTER NO-UNDO.

  SESSION:SET-WAIT-STATE("GENERAL").
  
  ASSIGN 
    cType1 = "Butikk;Varemerke;Lagerkoder;Levn;Lev.artikkelnr;Lev.fargekode"
    cType2 = "Butikk;Varemerke;Lagerkoder;LevNr;Lev.artikkkelnr;Lev.fargekode"
    cType3 = "Art.nr;Farge;NOS"
    .

  rclStandardFunksjoner:konvExcel2csv( INPUT pcFilNavn,
    INPUT '',
    OUTPUT pcTempFil ).
                                       
  IF SEARCH(pcTempFil) <> ? THEN 
  IMPORTER:
  DO:
    rclStandardFunksjoner:importerImpFil( INPUT pcTempFil,
      INPUT 1,
      INPUT-OUTPUT DATASET dsttImpFil ).
    rclStandardFunksjoner:SkrivTilLogg(cLogg, 'Lagerliste - Importert fil ' + pcTempFil).
    rclStandardFunksjoner:SkrivTilLogg(cLogg, 'Lagerliste - JSonFil   fil ' + cJSonFil).
    
    FIND FIRST ttImpFilLinje WHERE 
      ttImpFilLinje.LinjeNr = 1 NO-ERROR.
    IF AVAILABLE ttImpFilLinje THEN 
    DO:      
      IF ttImpfilLinje.Record BEGINS cType1 OR ttImpfilLinje.Record BEGINS cType2 THEN 
        piType = 1.
      ELSE IF ttImpfilLinje.Record BEGINS cType3 THEN 
        piType = 3.

      rclStandardFunksjoner:SkrivTilLogg(cLogg, '  Record: ' + ttImpfilLinje.Record).
      CASE pitype:
        WHEN 1 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType1: ' + cType1).
        WHEN 3 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType3: ' + cType1).
        OTHERWISE 
        DO:
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  Ingen match på type.').
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType1: ' + cType1).
        END.
      END CASE.
    END.
    
    pianLinjer = 0.
    FOR EACH ttImpFilLinje:
      pianLinjer = pianLinjer + 1.
    END.
    
    IF CAN-DO('1,3',STRING(piType)) THEN 
      ASSIGN 
        pbOk  = TRUE 
        pcMsg = 'Fil ' + pcFilNavn + ' importert i pakkseddel register.'
        .
    ELSE 
      ASSIGN 
        pbOk  = FALSE 
        pcMsg = '** Feil format på fil. Kan ikke importeres.'
        .
  END. /* IMPORTER */
  ELSE 
  DO:
    ASSIGN 
      pbOk  = FALSE 
      pcMsg = '** Finner ikke filen ' + pcTempFil + ' (Konvertert fra: '+ pcFilNavn + '). '.
    .
  END.
  
  SESSION:SET-WAIT-STATE("").
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
    SUBSCRIBE 'getFilterTekst' ANYWHERE.
    
    rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.

    cLogg  = 'LagerListeButArtStr' + REPLACE(STRING(TODAY),'/','').
    cModus = IF NUM-ENTRIES(IcParam,'|') >= 2 THEN ENTRY(2,icParam,'|') ELSE ''.

    CASE cModus:
      WHEN '50' OR WHEN '70 'THEN /* Overføringer / Reservasjon */ 
        DO:
          ASSIGN 
            iButNr  = INT(ENTRY(1,icParam,'|'))
            cRowId  = IF NUM-ENTRIES(IcParam,'|') >= 3 THEN ENTRY(3,icParam,'|') ELSE ''
            iBuntNr = IF NUM-ENTRIES(IcParam,'|') >= 4 THEN INT(ENTRY(4,icParam,'|')) ELSE 0
            iTilBut = IF NUM-ENTRIES(IcParam,'|') >= 5 THEN INT(ENTRY(5,icParam,'|')) ELSE 0
            .
        END.
      WHEN '60' THEN /* Pakkseddel */ 
        DO:
          ASSIGN 
            iButNr   = INT(ENTRY(1,icParam,'|'))
            lPkSdlId = IF NUM-ENTRIES(IcParam,'|') >= 3 THEN DEC(ENTRY(3,icParam,'|')) ELSE 0
            iLevNr   = IF NUM-ENTRIES(IcParam,'|') >= 4 THEN INT(ENTRY(4,icParam,'|')) ELSE 0
            .
        END.
      WHEN '45' THEN 
        DO:
          ASSIGN 
            iButNr      = INT(ENTRY(1,icParam,'|'))
            cRowId      = IF NUM-ENTRIES(IcParam,'|') >= 3 THEN ENTRY(3,icParam,'|') ELSE ''
            iKampanjeId = IF NUM-ENTRIES(IcParam,'|') >= 4 THEN DEC(ENTRY(4,icParam,'|')) ELSE 0
            .
        END.
      OTHERWISE 
      DO:
        ASSIGN 
          iButNr     = INT(ENTRY(1,icParam,'|'))
          cRowId     = IF NUM-ENTRIES(IcParam,'|') >= 3 THEN ENTRY(3,icParam,'|') ELSE ''
          iLinjeNr   = IF NUM-ENTRIES(IcParam,'|') >= 4 THEN INT(ENTRY(4,icParam,'|')) ELSE 0
          lPrisAvvik = IF NUM-ENTRIES(IcParam,'|') >= 5 THEN ABS(DEC(ENTRY(5,icParam,'|'))) ELSE 0          
          lPris      = IF NUM-ENTRIES(IcParam,'|') >= 6 THEN ABS(DEC(ENTRY(6,icParam,'|'))) ELSE 0
          iLevNr     = IF NUM-ENTRIES(IcParam,'|') >= 7 THEN DEC(ENTRY(7,icParam,'|')) ELSE 0
          cKode      = IF NUM-ENTRIES(IcParam,'|') >= 8 THEN ENTRY(8,icParam,'|') ELSE ''    
          .  
      END.
    END CASE.
    IF lPris > 0 AND lPrisAvvik> 0 THEN
    DO: 
      /* Er tillatt avvik mer enn Kr. 50, beregnes 10% av salgspris som tillatt avvik. */
      IF lPrisAvvik < ((lPris * 10) / 100) THEN 
        lPrisAvvik = ROUND(((lPris * 10) / 100),0).
      ASSIGN
        lMaksPris = lPris + lPrisavvik 
        lMinPris  = lPris - lPrisavvik
        lMinPris  = IF lMinPris < 0 THEN 0 ELSE lMinPris 
        .
    END.
    oBrwArtLag = NEW JBoxBrowse(brwArtLag:HANDLE).
    oBrwArtLag:useLocalData = YES. /* Flagger at browser går rundt en temp-tabell, ikke en database tabell. */
    otbArtLag = NEW JBoxToolbar(tbArtLag:HANDLE).
    oBrwArtLag:TOOLBAR-OBJECT = otbArtLag.

    hArtLag_TilbudsPrisColumn = oBrwArtLag:getColumnHandle("ArtLag_TilbPris").
    oBrwArtLag:baseQuery  = "WHERE Butik = '16' ".
    /*  oBrwArtLag:setJBoxAttribute("prescanbasequery","PkSdlLinje where butikknr = 10, FIRST PkSdlHode NO-LOCK OF PkSdlLinje").*/

    CASE cModus:
      WHEN '10' THEN 
        DO:
          otbArtLag:addToolGroup("SendTilKasse;Send til Nettbutikk,SettWebArt;Aktiver i nettbutikk(Toggler),SettPubliser;Publiser i nettbutikk(Toggler),ValgtVare;Legg markerte varer til kampanje...,ValgtOverfor;Legg markerte varer til overføring...,SettLagerkoder;Sett lagerkoder...,Etikett;Etikett").
        END.
      WHEN '20' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtVare;Velg vare...").
        END.
      WHEN '25' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtVare;Velg vare...").
        END.
      WHEN '30' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtVare;Velg vare for internkjøp...").
        END.
      WHEN '40' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtVare;Velg vare for varemottak...").
        END.
      WHEN '45' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtVare;Legg markerte varer til kampanje...").
        END.
      WHEN '50' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtOverfor;Legg markerte varer til overføring...").
        END.
      WHEN '60' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtPkSdl;Legg markerte varer til pakkseddel...").
        END.
      WHEN '70' THEN 
        DO:
          otbArtLag:addToolGroup("ValgtOverfor;Legg markerte varer til overføring...").
        END.
    END CASE.
  
    oBrwArtLag:setSearchField(searchArtLag:HANDLE,"ArtLag_LevKod").
    oBrwArtLag:rowsToBatch = 50.
  END.

  hArtLag_NOSColumn = oBrwArtLag:getColumnHandle("ArtLag_NOS").
  hArtLag_KodeColumn = oBrwArtLag:getColumnHandle("ArtLag_Kode").
  
  IF CAN-DO('20,30,40,45,50,70',cModus) THEN  
    ASSIGN 
      cbLagant:SCREEN-VALUE = '1'
      cbLagant              = 1
      .
  ELSE 
    ASSIGN 
      cbLagant:SCREEN-VALUE = '1'
      cbLagant              = 1
      .
  /* Legger på eventuelle nye lagerkoder som ikke er satt opp i combo boksen. */
  IF JBoxServerApi:Instance:CallServerProc("artbas_Lagerkoder.p",cbLagerKoder:LIST-ITEM-PAIRS) THEN 
    cbLagerKoder:LIST-ITEM-PAIRS = JBoxServerApi:Instance:getCallReturnParam().
  ASSIGN 
    cbLagerkoder = ENTRY(2,cbLagerKoder:LIST-ITEM-PAIRS)
    .    

  opopupModell = NEW JBoxPopupMenu().
  opopupModell:AddToolGroup('OppslagModell;Vis i modell liste'  
                           ).
  oBrwArtLag:POPUP-MENU-OBJECT = opopupModell.

  /* Bygger tempTabell */
  RUN RefreshRecord.

  cbEndret = TRUE.    
  RUN setFilter.

  APPLY 'ENTRY' TO fiBeskr IN FRAME DEFAULT-FRAME.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN
  oContainer:setNoResizeY("RECT-3").
  oContainer:setNoResizeX("RECT-3").
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
hArtLag = BUFFER ArtLag:HANDLE.
  
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
    IF AVAILABLE ArtLag THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',ArtLag.ArtLag_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',ArtLag.ArtLag_LevFargKod)
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
/*  RUN SUPER.*/

  RUN ByggerRapport.w PERSISTENT SET hByggerRapport.
  RUN MoveToTop IN hByggerRapport.
  RUN vistekst IN hByggerRapport.
  hArtLag = BUFFER ArtLag:HANDLE.
  SESSION:SET-WAIT-STATE("GENERAL").  
  IF JBoxServerAPI:Instance:CallServerProc("artlag_ByggTmp.p",'',hArtLag) THEN
    hArtLag = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
  RUN avsluttDialog IN hByggerRapport.
  IF VALID-HANDLE(hByggerRapport) THEN 
    DELETE OBJECT hByggerRapport.
  oBrwArtLag:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  IF AVAILABLE ArtLag THEN 
  DO:
    IF ArtLag.ArtLag_NOS MATCHES '*NOS*' THEN
      hArtLag_NOSColumn:BGCOLOR = 12.
    ELSE IF ArtLag.ArtLag_NOS <> '' THEN
        hArtLag_NOSColumn:BGCOLOR = 10.
      
    /* Benyttes lagerlisten til varesøk ved endringav vare på kundeordre, skal ikke samme vare kunne legges inn. Varsles visuelt med farge. */  
    IF ArtLag_Kode = cKode AND cKode <> '' THEN 
      hArtLag_KodeColumn:BGCOLOR = 12.

    IF ArtLag.ArtLag_TilbPris > 0 THEN
      ASSIGN
        hArtLag_TilbudsPrisColumn:BGCOLOR = 13
        .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTilKasseRecord C-Win 
PROCEDURE SendTilKasseRecord :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

  IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send artikler for valgt(e) post(er) til Nettbutikk?") THEN 
      RETURN.
    IF NOT oBrwArtlag:processRowsNoMessage("artlag_SendTilKasse.p", "") THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    oBrwArtlag:refreshSelectedRows().  
  END.
  ELSE 
  DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send artikler for alle valgte poster til Nettbutikk?") THEN 
      RETURN.
    oBrwArtlag:processSet("artlag_SendTilKasse.p","").
    oBrwArtlag:OpenQuery().
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilter C-Win 
PROCEDURE setFilter :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    /* Gjør bare noe hvis filter skal blankes eller nytt filter settes. */
    IF (fiBeskr:SCREEN-VALUE + fiLevKod:SCREEN-VALUE + fiLevFargKod:SCREEN-VALUE + fiStr:SCREEN-VALUE <> '') OR
      (cWhere + cOperator <> '') OR 
      lPrisAvvik > 0 OR 
      iLevNr > 0 OR 
      cbEndret = TRUE THEN
    DO:
      cWhere = (IF cbLagant:SCREEN-VALUE <> '0' THEN '0|' ELSE '') +
        (
        (IF cbLagerkoder:SCREEN-VALUE = 'MERKET' THEN '' ELSE 
        IF cbLagerkoder:SCREEN-VALUE <> '' THEN '*' + cbLagerkoder:SCREEN-VALUE + '*' ELSE 
        cbLagerkoder:SCREEN-VALUE
        ) + '|') +
        (IF TRIM(fiBeskr:SCREEN-VALUE,'*') <> '' THEN '*' ELSE '') +
        TRIM(fiBeskr:SCREEN-VALUE,'*') +
        (IF TRIM(fiBeskr:SCREEN-VALUE,'*') <> '' THEN '*|' ELSE '|') +
        fiLevKod:SCREEN-VALUE + "|" +
        fiLevFargKod:SCREEN-VALUE + "|" +
        fiStr:SCREEN-VALUE
        .
      /* Lagerantall combo. */  
      pcTekst   = (IF cbLagant:SCREEN-VALUE = '1' THEN '>,' 
      ELSE IF cbLagant:SCREEN-VALUE = '2' THEN '<,'
      ELSE IF cbLagant:SCREEN-VALUE = '3' THEN '<>,'
      ELSE IF cbLagant:SCREEN-VALUE = '4' THEN '=,'
      ELSE '')
        .              

      /* Lagerkoder combo. */
      pcTekst   = pcTekst +   
        (IF cbLagerkoder:SCREEN-VALUE = '' THEN '>=,'   
        ELSE IF cbLagerkoder:SCREEN-VALUE = 'MERKET' THEN '>,'  
        ELSE IF cbLagerkoder:SCREEN-VALUE <> '' THEN 'MATCHES,'
        ELSE '')
        .              

      cOperator = pcTekst +
        (IF fiBeskr:SCREEN-VALUE <> '' THEN 'MATCHES' ELSE '') + ',' +
        (IF fiLevKod:SCREEN-VALUE <> '' THEN 'BEGINS' ELSE '') + ',' +
        (IF fiLevFargKod:SCREEN-VALUE <> '' THEN '=' ELSE '') + ',' +
        (IF fiStr:SCREEN-VALUE <> '' THEN '=' ELSE '').
        .
        
      CASE cModus:
        WHEN '10' OR 
        WHEN '30' OR 
        WHEN '50' OR WHEN '70' THEN 
          DO:
            oBrwArtLag:setFilter((IF cbLagant:SCREEN-VALUE <> '0' THEN 'Lagant,' ELSE '') + 'ArtLag_NOS,ArtLag_Beskr,ArtLag_LevKod,ArtLag_LevFargKod,Storl',cOperator,cWhere).
          END.
        WHEN '20' OR 
        WHEN '25' OR /* RETUR */ 
        WHEN '27' /* BYTTE */ THEN 
          DO:
            cWhere = cwhere + '|' + STRING(lMinPris) + '|' + STRING(lMaksPris).
            cOperator = cOperator + ',>=,<='.
            oBrwArtLag:setFilter((IF cbLagant:SCREEN-VALUE <> '0' THEN 'Lagant,' ELSE '') + 'ArtLag_NOS,ArtLag_Beskr,ArtLag_LevKod,ArtLag_LevFargKod,Storl,ArtLag_Pris,ArtLag_Pris',cOperator,cWhere).
          END.
        WHEN '40' OR 
        WHEN '60' THEN 
          DO:
            cFields = (IF cbLagant:SCREEN-VALUE <> '0' THEN 'Lagant,' ELSE '') + 'ArtLag_NOS,ArtLag_Beskr,ArtLag_LevKod,ArtLag_LevFargKod,Storlt,ArtLag_LevNr'.
            cWhere = cwhere + '|' + STRING(iLevNr).
            cOperator = cOperator + ',='.
            /*
            MESSAGE ':cFields:' cFields NUM-ENTRIES(cFields) SKIP 
                    'cwhere:' cwhere NUM-ENTRIES(cwhere,'|') SKIP
                    'cOperator:' cOperator NUM-ENTRIES(cOperator)
            VIEW-AS ALERT-BOX.
            */
            oBrwArtLag:setFilter(cFields,cOperator,cWhere).
          END.
        WHEN '45' THEN 
          DO:
            oBrwArtLag:setFilter((IF cbLagant:SCREEN-VALUE <> '0' THEN 'Lagant,' ELSE '') + 'ArtLag_NOS,ArtLag_Beskr,ArtLag_LevKod,ArtLag_LevFargKod,Storl',cOperator,cWhere).
          END.
      END CASE.
    END.
    
    cbEndret = FALSE.    
    oBrwArtLag:OpenQuery().
    APPLY 'ENTRY' TO fiBeskr IN FRAME DEFAULT-FRAME.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettLagerkoderRecord C-Win 
PROCEDURE SettLagerkoderRecord :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO WITH FRAME {&FRAME-NAME}:    
    ASSIGN 
      cLagerkoder = IF AVAILABLE ArtLag THEN ArtLAg.Artlag_NOS ELSE ''
      .

    RUN settLagerkoder.w (INPUT oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS,
      INPUT-OUTPUT cLagerkoder).
    IF cLagerkoder = 'AVBRYT' THEN
    DO:
      JBoxSession:Instance:ViewMessage("Avbrutt. Ingen oppdatering av artiklene er gjort!").
      RETURN.
    END.

    IF NOT oBrwArtlag:processRowsNoMessage("artlag_settLagerkoder.p", cLagerkoder) THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).

    /* Legger på eventuelle nye lagerkoder som ikke er satt opp i combo boksen. */
    IF JBoxServerApi:Instance:CallServerProc("artbas_Lagerkoder.p",cbLagerKoder:LIST-ITEM-PAIRS) THEN 
      cbLagerKoder:LIST-ITEM-PAIRS = JBoxServerApi:Instance:getCallReturnParam().
      
    oBrwArtlag:refreshSelectedRows().  
  END.
  ELSE 
  DO: 
    JBoxSession:Instance:ViewMessage("Marker en eller flere linjer hvor lagerkoder skal endres.").
    RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettPubliserRecord C-Win 
PROCEDURE SettPubliserRecord :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Publiser artikler for valgt(e) post(er) (Toggler Ja/Nei)?") THEN 
      RETURN.
    IF NOT oBrwArtlag:processRowsNoMessage("artlag_Publiser.p", "") THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    oBrwArtlag:refreshSelectedRows().  
  END.
  ELSE 
  DO:  
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Publiser artikler for alle valgte poster (Toggler Ja/Nei)?") THEN 
      RETURN.
    oBrwArtlag:processSet("artlag_Publiser.p","").
    oBrwArtlag:OpenQuery().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettWebArtRecord C-Win 
PROCEDURE SettWebArtRecord :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Aktiver artikler for valgt(e) post(er) i nettbutikk (Toggler Ja/Nei)?") THEN 
      RETURN.
    IF NOT oBrwArtlag:processRowsNoMessage("artlag_AktiverNettBut.p", "") THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    oBrwArtlag:refreshSelectedRows().  
  END.
  ELSE 
  DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Aktiver artikler for alle valgte poster i nettbutikk (Toggler Ja/Nei)?") THEN 
      RETURN.
    oBrwArtlag:processSet("artlag_AktiverNettBut.p","").
    oBrwArtlag:OpenQuery().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokStrekkode C-Win 
PROCEDURE sokStrekkode :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE plArtikkelNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE pctekst      AS CHARACTER NO-UNDO.
  
DO WITH FRAME {&FRAME-NAME}:
  /* Gjør oppslag og henter modellnr på artikkel. Setter filterverdier og kjører deretter standard */
  /* rutine for å aktivere filter.                                                                 */ 

  IF fiKode:SCREEN-VALUE <> '' THEN 
  DO:
    IF NOT JBoxServerAPI:Instance:Find("Strekkode", "WHERE Kode = '"+ fiKode:SCREEN-VALUE + "'") THEN
    DO:
      JBoxSession:Instance:ViewMessage("Ukjent strekkode!").
      RETURN.
    END.  
    ELSE DO:
      ASSIGN 
        fiBeskr:SCREEN-VALUE  = ''
        fiLevKod:SCREEN-VALUE = ''
        .
      
      plArtikkelNr = DEC(JBoxServerAPI:Instance:FieldValue("Strekkode.ArtikkelNr")).
      IF JBoxServerAPI:Instance:Find("ArtBas", "WHERE ArtikkelNr = '" + STRING(plArtikkelNr) + "'") THEN
      DO:
        fiBeskr:SCREEN-VALUE = (JBoxServerAPI:Instance:FieldValue("ArtBas.Beskr")).
        fiLevKod:SCREEN-VALUE = (JBoxServerAPI:Instance:FieldValue("ArtBas.LevKod")).
        fiKode:SCREEN-VALUE = ''.
        
      END.
    END.
    RUN setFilter.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValgtOverforRecord C-Win 
PROCEDURE ValgtOverforRecord :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  
  /* Fra LagerListeButArtStr.w (10), Internoverforinger.w (50) og Reservasjon (70). */
  IF CAN-DO('10,50,70',cModus) THEN
  OVERFORVALG: 
  DO:
    IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      IF iBuntNr = 0 THEN 
      DO:
        RUN velgOvbunt.w (OUTPUT pcTekst, OUTPUT iTilBut).
        IF pcTekst = ? OR pcTekst = '' OR INT(pcTekst) = 0 THEN
        DO:
          PUBLISH "msgFraLagerListeButArtStr" ("","").
          RETURN.
        END.
        ELSE 
          ASSIGN 
            iBuntNr = INT(pcTekst)
            .
      END.
      IF oBrwArtlag:processRowsNoMessage("vare_til_overfor.p", STRING(iBuntNr) + '|' + STRING(iButNr) + '|' + STRING(iTilBut)) THEN
      DO:
        IF cModus = '50' THEN
        DO: 
          PUBLISH "msgFraLagerListeButArtStr" ("Overfor","").
          PUBLISH "OpenQueryOverfor".
          APPLY "CLOSE":U TO THIS-PROCEDURE.
          RETURN.
        END. 
        IF cModus = '70' THEN
        DO: 
          PUBLISH "msgFraLagerListeButArtStr" ("Reservasjon",""). /* Reservasjon */
          PUBLISH "OpenQueryOverfor".
          APPLY "CLOSE":U TO THIS-PROCEDURE.
          RETURN.
        END. 
        ELSE 
        DO: 
          iBuntNr = 0.
          PUBLISH "OpenQueryOverfor".
        END.
      END.
      /* TN 7/10-19 Etter råd fra Brynjar. WorkAroound for bug - Se mail.          */
      /* Problem her med at vinduet fryser når det legges over varer flere ganger. */
      APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    END.  
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Marker en eller flere rader først! ").
      PUBLISH "msgFraLagerListeButArtStr" ("","").
      RETURN.
    END.
  END. /* KAMPANJEVALG */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValgtPkSdlRecord C-Win 
PROCEDURE ValgtPkSdlRecord :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  /* Fra LagerListeButArtStr.w (10) og Internoverforinger.w (50). */
  IF CAN-DO('60',cModus) THEN
  PKSDLVALG: 
  DO:
    IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      IF oBrwArtlag:processRowsNoMessage("vare_til_pksdllinje.p", STRING(lPkSdlId) + '|' + STRING(iButNr)) THEN
        PUBLISH "OpenQueryPkSdlLinje".
    END.  
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Marker en eller flere rader først! ").
      RETURN.
    END.
  END. /* PKSDLVALG */
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
  /* Fra LagerListeButArtStr.w og KampanjeRegister.w. */
  IF CAN-DO('10,45',cModus) THEN
  KAMPANJEVALG: 
  DO:
    IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      IF NOT CAN-DO('45',cModus) THEN
      DO:
        iKampanjeId = 0.
        RUN velgKampanje.w (OUTPUT pcTekst).
        IF pcTekst = ? OR pcTekst = '' OR INT(pcTekst) = 0 THEN
          RETURN.
        ELSE 
          iKampanjeId = INT(pcTekst).
      END.
      oBrwArtLag:processRowsNoMessage("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).
      PUBLISH "OpenQueryKampanjeLinje".
    
      IF NOT CAN-DO('45',cModus) THEN 
        iKampanjeId = 0.
    END.  
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Marker en eller flere rader først! ").
      RETURN.
    END.
  END. /* KAMPANJEVALG */
  
  ELSE 
  ANDRAVALG:
  DO:
    IF oBrwArtlag:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
    DO:    
      CASE cModus:
        /* kundeordreEndre.w */
        WHEN '20' THEN 
          DO:
            IF cKode = Artlag.ArtLag_Kode THEN 
            DO:
              JBoxSession:Instance:ViewMessage("Du kan ikke legge inn samme vare/størrelse!").
              RETURN.
            END.            
            IF NOT oBrwArtlag:processRowsNoMessage("kordrelinje_byttvare.p", cRowId + '|' + STRING(iLinjeNr)) THEN
              JBoxSession:Instance:ViewMessage("Feil ved bytte pga " + JBoxServerAPI:Instance:getCallMessage()).
          END.
        WHEN '25' THEN /* RETUR */ 
          DO:
            IF cKode = Artlag.ArtLag_Kode THEN 
            DO:
              JBoxSession:Instance:ViewMessage("Du kan ikke bytte til samme vare/størrelse!").
              RETURN.
            END.            
            IF NOT oBrwArtlag:processRowsNoMessage("kordrelinje_returbyttvare.p", cRowId + '|' + STRING(iLinjeNr)) THEN
              JBoxSession:Instance:ViewMessage("Feil ved bytte pga " + JBoxServerAPI:Instance:getCallMessage()).
          END.
        WHEN '27' THEN /* BYTTE */
          DO:
            IF cKode = Artlag.ArtLag_Kode THEN 
            DO:
              JBoxSession:Instance:ViewMessage("Du kan ikke bytte til samme vare/størrelse!").
              RETURN.
            END.            
            IF NOT oBrwArtlag:processRowsNoMessage("kordrelinje_returbyttvare.p", cRowId + '|' + STRING(iLinjeNr)) THEN
              JBoxSession:Instance:ViewMessage("Feil ved bytte pga " + JBoxServerAPI:Instance:getCallMessage()).
          END.
        /* Internoverføringer.w */  
        WHEN '30' THEN 
          DO:
            IF AVAILABLE ArtLag THEN 
            DO:
              PUBLISH "msgFraLagerListeButArtStr" ("ArtikkelNr,Storl", 
                STRING(ArtLag.ArtikkelNr) + "|" + STRING(ArtLag.Storl)).
            END.
          END.
        /* PkSdlBehandling.w */
        WHEN '40' THEN 
          DO:
            IF AVAILABLE ArtLag THEN 
            DO:
              PUBLISH "msgFraLagerListeButArtStr" ("ArtikkelNr,StrKode,Kode,LevNr,Beskr,LevKod,LevFargKod", 
                STRING(ArtLag.ArtikkelNr) + "|" + 
                STRING(ArtLag.StrKode) + "|" + 
                STRING(ArtLag.ArtLag_Kode) + "|" + 
                STRING(ArtLag.ArtLag_LevNr) + "|" + 
                STRING(ArtLag.ArtLag_Beskr) + "|" + 
                STRING(ArtLag.ArtLag_LevKod) + "|" + 
                STRING(ArtLag.ArtLag_LevFargKod) 
                ).
                                                   
            END.
          END.
      END CASE.
      oBrwArtlag:refreshSelectedRows(). 
/*      APPLY "CLOSE":U TO THIS-PROCEDURE.*/
    END.
    ELSE 
    DO: 
      JBoxSession:Instance:ViewMessage("Marker bare en rad! Rad må velges, og det kan ikke være markert mer enn en rad. ").
      RETURN.
    END.
  END. /* ANDRAVALG */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

