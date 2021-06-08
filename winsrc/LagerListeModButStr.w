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

  Author:            Tom Nøkleby

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
&SCOPED-DEFINE AdvGuiWin 

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

DEFINE VARIABLE cLagerkoder AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWhere      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cOperator   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE hLagerkoderColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hArtBas_TilbudsPrisColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hArtBas_KampRab%Column AS HANDLE NO-UNDO.
DEFINE VARIABLE hArtBas_KampPrisColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE opopupModell AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cLevKod AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLevFargKod AS CHARACTER NO-UNDO.
DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cKnapp AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwArtBas ***/
DEF VAR oBrwArtBas AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE ArtBas
    FIELD LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD Beskr AS CHARACTER
    FIELD SaSong AS INTEGER
    FIELD Lagerkoder AS CHARACTER
    FIELD ArtBas_Pris AS DECIMAL
    FIELD ArtBas_TilbudsPris AS DECIMAL
    FIELD ArtBas_KampRab% AS DECIMAL
    FIELD WebButikkArtikkel AS LOGICAL
    FIELD ArtBas_HarLager AS INTEGER
    FIELD ArtBas_Kampanje AS INTEGER
    FIELD ArtBas_HovedKategori AS CHARACTER
    FIELD ArtBas_Varemerke AS CHARACTER
    FIELD ArtBas_Brukskode AS CHARACTER
    FIELD ArtBas_Produsent AS CHARACTER
    FIELD HovedModellFarge AS LOGICAL
    FIELD ArtikkelNr AS DECIMAL
    FIELD ModellFarge AS DECIMAL
    FIELD Vg AS INTEGER
    FIELD Hg AS INTEGER
    FIELD VMId AS INTEGER
    FIELD LevNr AS INTEGER
    FIELD lager AS LOGICAL
    FIELD ProdNr AS INTEGER
    FIELD HovedKatNr AS INTEGER
    FIELD anv-id AS INTEGER
    FIELD RegistrertDato AS DATE
    FIELD EDato AS DATE
    FIELD ArtBas_KampPris AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_ArtBas FOR TEMP-TABLE ArtBas.


FUNCTION getBuffersAndFieldsBrwArtBas RETURNS CHARACTER():
  RETURN
    'ArtBas'
     + ';LevKod'
     + ';LevFargKod'
     + ';Beskr'
     + ';SaSong'
     + ';Lagerkoder'
     + ';WebButikkArtikkel'
     + ';HovedModellFarge'
     + ';ArtikkelNr'
     + ';ModellFarge'
     + ';Vg'
     + ';Hg'
     + ';VMId'
     + ';LevNr'
     + ';lager'
     + ';ProdNr'
     + ';HovedKatNr'
     + ';anv-id'
     + ';RegistrertDato'
     + ';EDato'
     + ';+ArtBas_Pris|DECIMAL||ArtBas_Pris|Pris'
     + ';+ArtBas_TilbudsPris|DECIMAL||ArtBas_TilbudsPris|TilbudsPris'
     + ';+ArtBas_KampRab%|DECIMAL||ArtBas_KampRab%|KampRab%'
     + ';+ArtBas_HarLager|INTEGER||ArtBas_HarLager|Har lager'
     + ';+ArtBas_Kampanje|INTEGER||ArtBas_Kampanje|Kamp'
     + ';+ArtBas_HovedKategori|CHARACTER||ArtBas_HovedKategori|HovedKatNavn'
     + ';+ArtBas_Varemerke|CHARACTER||ArtBas_Varemerke|VaremerkeNavn'
     + ';+ArtBas_Brukskode|CHARACTER||ArtBas_Brukskode|BrukskodeNavn'
     + ';+ArtBas_Produsent|CHARACTER||ArtBas_Produsent|ProdusentNavn'
     + ';+ArtBas_KampPris|DECIMAL||ArtBas_KampPris|KampPrris'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwArtBas RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwArtBas RETURNS CHARACTER():
  RETURN 
     'server/artbas_brwcalc.p' /* ArtBas_Pris */
   + ',server/artbas_brwcalc.p' /* ArtBas_TilbudsPris */
   + ',server/artbas_brwcalc.p' /* ArtBas_KampRab% */
   + ',server/artbas_brwcalc.p' /* ArtBas_HarLager */
   + ',server/artbas_brwcalc.p' /* ArtBas_Kampanje */
   + ',server/artbas_brwcalc.p' /* ArtBas_HovedKategori */
   + ',server/artbas_brwcalc.p' /* ArtBas_Varemerke */
   + ',server/artbas_brwcalc.p' /* ArtBas_Brukskode */
   + ',server/artbas_brwcalc.p' /* ArtBas_Produsent */
   + ',server/artbas_brwcalc.p' /* ArtBas_KampPris */
     .
END FUNCTION.


DEF VAR otbArtBas AS JBoxToolbar NO-UNDO.


DEF VAR oTabArtBas AS JBoxMsTabs NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwArtBas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for BROWSE BrwArtBas                                     */
&Scoped-define FIELDS-IN-QUERY-BrwArtBas ArtBas.LevKod ArtBas.LevFargKod ~
ArtBas.Beskr ArtBas.SaSong ArtBas.Lagerkoder ArtBas.ArtBas_Pris ~
ArtBas.ArtBas_TilbudsPris ArtBas.ArtBas_KampRab% ArtBas.WebButikkArtikkel ~
ArtBas.ArtBas_HarLager ArtBas.ArtBas_Kampanje ArtBas.ArtBas_HovedKategori ~
ArtBas.ArtBas_Varemerke ArtBas.ArtBas_Brukskode ArtBas.ArtBas_Produsent ~
ArtBas.HovedModellFarge ArtBas.ArtikkelNr ArtBas.ModellFarge ArtBas.Vg ~
ArtBas.Hg ArtBas.VMId ArtBas.LevNr ArtBas.lager ArtBas.ProdNr ~
ArtBas.HovedKatNr ArtBas.anv-id ArtBas.RegistrertDato ArtBas.EDato ~
ArtBas.ArtBas_KampPris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwArtBas ArtBas.LevKod ArtBas.Beskr ~
ArtBas.ArtikkelNr ArtBas.Vg 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwArtBas ArtBas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwArtBas ArtBas
&Scoped-define QUERY-STRING-BrwArtBas FOR EACH ArtBas NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwArtBas OPEN QUERY BrwArtBas FOR EACH ArtBas NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwArtBas ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-BrwArtBas ArtBas


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbArtBas tbFilter TabArtBas searchArtBas ~
first_tbArtBas prev_tbArtBas next_tbArtBas last_tbArtBas refresh_tbArtBas ~
filter_tbArtBas excel_tbArtBas SendTilKasse_tbArtBas SettPubliser_tbArtBas ~
SendTilKampanje_tbArtBas TGKampanje BtnBlank fiBeskr fiLevKod fiLevFargKod ~
cbLagant cbLagerkoder tgNettbutikk BrwArtBas 
&Scoped-Define DISPLAYED-OBJECTS TGKampanje fiBeskr fiLevKod fiLevFargKod ~
cbLagant cbLagerkoder tgNettbutikk 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnBlank 
     LABEL "<Blank>" 
     SIZE 9.4 BY 1.14 TOOLTIP "Blanker filter".

DEFINE BUTTON excel_tbArtBas 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbArtBas 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbArtBas 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbArtBas 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbArtBas 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbArtBas 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbArtBas 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON SendTilKampanje_tbArtBas 
     LABEL "Send til kampanje" 
     SIZE 18 BY 1.1 TOOLTIP "Sender varene til en kampanje".

DEFINE BUTTON SendTilKasse_tbArtBas 
     LABEL "Send til nettbutik" 
     SIZE 19 BY 1.1 TOOLTIP "Initierer sending av vareinformasjonen til kasse og nettbutikk".

DEFINE BUTTON SettPubliser_tbArtBas 
     LABEL "Publiser i nettbutikk(Toggler)" 
     SIZE 31 BY 1.1 TOOLTIP "Publiserer artikkel i nettbutikk".

DEFINE VARIABLE cbLagant AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "<Alle>",0,
                     "Har lager",1,
                     "Har ikke lager",2
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE cbLagerkoder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "","",
                     "<Merkede>","MERKET",
                     "NOS","NOS",
                     "K","K",
                     "L","L"
     DROP-DOWN-LIST
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 TOOLTIP "Søkefilter på varetekst." NO-UNDO.

DEFINE VARIABLE fiLevFargKod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 TOOLTIP "Søkefilter på leverandørs fargekode" NO-UNDO.

DEFINE VARIABLE fiLevKod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Søkefilter på leverandørs artikkelnummer" NO-UNDO.

DEFINE RECTANGLE searchArtBas
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY .91.

DEFINE RECTANGLE TabArtBas
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 21.67.

DEFINE RECTANGLE tbArtBas
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 209 BY 1.29.

DEFINE RECTANGLE tbFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 2.

DEFINE VARIABLE TGKampanje AS LOGICAL INITIAL NO 
     LABEL "Kampanje" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE tgNettbutikk AS LOGICAL INITIAL NO 
     LABEL "Nett" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 TOOLTIP "Vis bare artikler som er aktivert for nettbutikk" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1.2 BY 19.29
     BGCOLOR 12 FGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwArtBas FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwArtBas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwArtBas C-Win _STRUCTURED
  QUERY BrwArtBas NO-LOCK DISPLAY
      ArtBas.LevKod COLUMN-LABEL "LevArtNr" FORMAT "x(20)":U
      ArtBas.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(15)":U
      ArtBas.Beskr COLUMN-LABEL "Beskrivelse" FORMAT "x(50)":U
            WIDTH 30
      ArtBas.SaSong COLUMN-LABEL "Sesong" FORMAT "zzzzz9":U
      ArtBas.Lagerkoder COLUMN-LABEL "Lagerkoder" FORMAT "x(30)":U
            WIDTH 12
      ArtBas.ArtBas_Pris COLUMN-LABEL "Pris" FORMAT "->>>,>>9.99":U
      ArtBas.ArtBas_TilbudsPris COLUMN-LABEL "TilbudsPris" FORMAT "->>,>>9.99":U
            WIDTH 12
      ArtBas.ArtBas_KampRab% COLUMN-LABEL "KampRab%" FORMAT "->>,>>9.99":U
            WIDTH 12
      ArtBas.WebButikkArtikkel COLUMN-LABEL "Nett" FORMAT "*/":U
      ArtBas.ArtBas_HarLager COLUMN-LABEL "Har lager" FORMAT "9":U
            WIDTH 7
      ArtBas.ArtBas_Kampanje COLUMN-LABEL "Kamp" FORMAT "9":U
      ArtBas.ArtBas_HovedKategori COLUMN-LABEL "HovedKatNavn" FORMAT "X(20)":U
      ArtBas.ArtBas_Varemerke COLUMN-LABEL "VaremerkeNavn" FORMAT "X(20)":U
      ArtBas.ArtBas_Brukskode COLUMN-LABEL "BrukskodeNavn" FORMAT "X(20)":U
      ArtBas.ArtBas_Produsent COLUMN-LABEL "ProdusentNavn" FORMAT "X(20)":U
      ArtBas.HovedModellFarge COLUMN-LABEL "HMF" FORMAT "*/":U
            WIDTH 4
      ArtBas.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      ArtBas.ModellFarge COLUMN-LABEL "ModellFarge" FORMAT ">>>>>>>>>>>>9":U
      ArtBas.Vg COLUMN-LABEL "Varegruppe" FORMAT "zzzzz9":U
      ArtBas.Hg COLUMN-LABEL "Hovedgruppe" FORMAT ">>>>9":U
      ArtBas.VMId COLUMN-LABEL "VareMerke" FORMAT ">>>>>9":U
      ArtBas.LevNr COLUMN-LABEL "Leverandørnummer" FORMAT "zzzzz9":U
      ArtBas.lager COLUMN-LABEL "Lagerstyrt" FORMAT "*/":U WIDTH 6
      ArtBas.ProdNr COLUMN-LABEL "Produsent" FORMAT "zzzzzz9":U
      ArtBas.HovedKatNr COLUMN-LABEL "Hovedkategori" FORMAT ">>>>>9":U
            WIDTH 7.2
      ArtBas.anv-id COLUMN-LABEL "Brukskode" FORMAT ">>>>9":U WIDTH 6
      ArtBas.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      ArtBas.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      ArtBas.ArtBas_KampPris COLUMN-LABEL "KampPrris" FORMAT "->>>,>>9.99":U
            WIDTH 12
  ENABLE
      ArtBas.LevKod HELP "Leverandørens artikkelnummer"
      ArtBas.Beskr HELP "Kort beskrivelse av artikkelen"
      ArtBas.ArtikkelNr
      ArtBas.Vg HELP "'varegruppenummer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 141.2 BY 19.33 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbArtBas AT ROW 1.33 COL 2 WIDGET-ID 6
     prev_tbArtBas AT ROW 1.33 COL 6.8 WIDGET-ID 8
     next_tbArtBas AT ROW 1.33 COL 11.4 WIDGET-ID 10
     last_tbArtBas AT ROW 1.33 COL 16 WIDGET-ID 12
     refresh_tbArtBas AT ROW 1.33 COL 20.6 WIDGET-ID 14
     filter_tbArtBas AT ROW 1.33 COL 25.2 WIDGET-ID 16
     excel_tbArtBas AT ROW 1.33 COL 29.8 WIDGET-ID 18
     SendTilKasse_tbArtBas AT ROW 1.33 COL 34.4 WIDGET-ID 54
     SettPubliser_tbArtBas AT ROW 1.33 COL 53.4 WIDGET-ID 58
     SendTilKampanje_tbArtBas AT ROW 1.33 COL 84.6 WIDGET-ID 56
     TGKampanje AT ROW 3.24 COL 88.6 WIDGET-ID 52
     BtnBlank AT ROW 3.76 COL 78.4 WIDGET-ID 42
     fiBeskr AT ROW 3.81 COL 1.6 COLON-ALIGNED NO-LABEL
     fiLevKod AT ROW 3.81 COL 35.4 COLON-ALIGNED NO-LABEL
     fiLevFargKod AT ROW 3.81 COL 54.4 COLON-ALIGNED NO-LABEL
     cbLagant AT ROW 3.81 COL 100.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     cbLagerkoder AT ROW 3.81 COL 120.8 COLON-ALIGNED NO-LABEL WIDGET-ID 40
     tgNettbutikk AT ROW 4 COL 88.6 WIDGET-ID 46
     BrwArtBas AT ROW 6.24 COL 2.8 WIDGET-ID 200
     "Varetekst" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.19 COL 4.2 WIDGET-ID 30
          FONT 6
     "Lev.artikkelnr" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.19 COL 38 WIDGET-ID 32
          FONT 6
     "Lager" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.19 COL 102.6 WIDGET-ID 48
          FONT 6
     "Lagerkoder" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.19 COL 123.4 WIDGET-ID 38
          FONT 6
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.48 COL 4.2 WIDGET-ID 28
          FONT 6
     "lev.fargekode" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 3.19 COL 56.6 WIDGET-ID 34
          FONT 6
     tbArtBas AT ROW 1.24 COL 2 WIDGET-ID 4
     tbFilter AT ROW 3.05 COL 3 WIDGET-ID 26
     TabArtBas AT ROW 3.86 COL 147 WIDGET-ID 44
     searchArtBas AT ROW 5.19 COL 3.4 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 217.2 BY 24.67 WIDGET-ID 100.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1.57 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 144 ROW 5.29
         SIZE 33.8 BY 20 WIDGET-ID 300.


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
         HEIGHT             = 24.67
         WIDTH              = 217.2
         MAX-HEIGHT         = 24.67
         MAX-WIDTH          = 217.2
         VIRTUAL-HEIGHT     = 24.67
         VIRTUAL-WIDTH      = 217.2
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
/* REPARENT FRAME */
ASSIGN FRAME frSplitBarX:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrwArtBas frSplitBarX DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.67
       FRAME DEFAULT-FRAME:WIDTH            = 217.2.

ASSIGN 
       BrwArtBas:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3.

ASSIGN 
       ArtBas.lager:VISIBLE IN BROWSE BrwArtBas = FALSE.

ASSIGN 
       tbArtBas:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,SendTilKasse;Send til nettbutik,SettPubliser;Publiser i nettbutikk(Toggler),SendTilKampanje;Send til kampanjemaxborder".

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwArtBas
/* Query rebuild information for BROWSE BrwArtBas
     _TblList          = "SkoTex.ArtBas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ArtBas.LevKod
"ArtBas.LevKod" "LevArtNr" "x(20)" "CHARACTER" ? ? ? ? ? ? yes "Leverandørens artikkelnummer" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ArtBas.LevFargKod
"ArtBas.LevFargKod" "LevFargKod" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ArtBas.Beskr
"ArtBas.Beskr" "Beskrivelse" "x(50)" "CHARACTER" ? ? ? ? ? ? yes "Kort beskrivelse av artikkelen" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ArtBas.SaSong
"ArtBas.SaSong" "Sesong" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "Sesong" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ArtBas.Lagerkoder
"ArtBas.Lagerkoder" "Lagerkoder" "x(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"ArtBas.ArtBas_Pris" "Pris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"ArtBas.ArtBas_TilbudsPris" "TilbudsPris" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"ArtBas.ArtBas_KampRab%" "KampRab%" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ArtBas.WebButikkArtikkel
"ArtBas.WebButikkArtikkel" "Nett" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Artikkelen skal distribueres til nett butikk" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"ArtBas.ArtBas_HarLager" "Har lager" "9" "INTEGER" ? ? ? ? ? ? no "" no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"ArtBas.ArtBas_Kampanje" "Kamp" "9" "INTEGER" ? ? ? ? ? ? no "" no no "5.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"ArtBas.ArtBas_HovedKategori" "HovedKatNavn" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"ArtBas.ArtBas_Varemerke" "VaremerkeNavn" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"ArtBas.ArtBas_Brukskode" "BrukskodeNavn" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"ArtBas.ArtBas_Produsent" "ProdusentNavn" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ArtBas.HovedModellFarge
"ArtBas.HovedModellFarge" "HMF" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Artikkelen er hovedartikkel i en model/farge." no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ArtBas.ArtikkelNr
"ArtBas.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? yes "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ArtBas.ModellFarge
"ArtBas.ModellFarge" "ModellFarge" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kobler sammen flere artikler som utgjør en modell." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ArtBas.Vg
"ArtBas.Vg" "Varegruppe" "zzzzz9" "INTEGER" ? ? ? ? ? ? yes "'varegruppenummer" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ArtBas.Hg
"ArtBas.Hg" "Hovedgruppe" ">>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ArtBas.VMId
"ArtBas.VMId" "VareMerke" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Varemerke ('Brand')." no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ArtBas.LevNr
"ArtBas.LevNr" "Leverandørnummer" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "Leverandørnummer" no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ArtBas.lager
"ArtBas.lager" "Lagerstyrt" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Artikkelen har lagerstyring." no no "6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ArtBas.ProdNr
"ArtBas.ProdNr" "Produsent" "zzzzzz9" "INTEGER" ? ? ? ? ? ? no "Produsent" no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ArtBas.HovedKatNr
"ArtBas.HovedKatNr" "Hovedkategori" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Kobling av artikkel til hovedkategori" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ArtBas.anv-id
"ArtBas.anv-id" "Brukskode" ">>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > ArtBas.RegistrertDato
"ArtBas.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ArtBas.EDato
"ArtBas.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"ArtBas.ArtBas_KampPris" "KampPrris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris som var gjeldende når kampanjen ble aktivert." no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwArtBas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
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


&Scoped-define SELF-NAME BtnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBlank C-Win
ON CHOOSE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank> */
DO:
    RUN blankFilter. 
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBlank C-Win
ON LEAVE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank> */
DO:
    APPLY 'ENTRY' TO fiBeskr IN FRAME DEFAULT-FRAME.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frSplitBarX
&Scoped-define SELF-NAME btnSplitBarX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON END-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("setSplitBarX" , THIS-PROCEDURE:CURRENT-WINDOW, btnSplitBarX:HANDLE IN FRAME frSplitBarX,NO).
/*  RUN MoveToTop IN hCurrTabProc NO-ERROR.*/

  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",?).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarX C-Win
ON START-MOVE OF btnSplitBarX IN FRAME frSplitBarX
DO:
  DYNAMIC-FUNCTION("ResizeKeepsWindowLocked",THIS-PROCEDURE:CURRENT-WINDOW).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME cbLagant
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLagant C-Win
ON VALUE-CHANGED OF cbLagant IN FRAME DEFAULT-FRAME
DO:
    RUN setFilter.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbLagerkoder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbLagerkoder C-Win
ON VALUE-CHANGED OF cbLagerkoder IN FRAME DEFAULT-FRAME
DO:
    RUN setFilter.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeskr C-Win
ON RETURN OF fiBeskr IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiBeskr C-Win
ON TAB OF fiBeskr IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON RETURN OF fiLevFargKod IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON TAB OF fiLevFargKod IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON RETURN OF fiLevKod IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON TAB OF fiLevKod IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TGKampanje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TGKampanje C-Win
ON VALUE-CHANGED OF TGKampanje IN FRAME DEFAULT-FRAME /* Kampanje */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgNettbutikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgNettbutikk C-Win
ON VALUE-CHANGED OF tgNettbutikk IN FRAME DEFAULT-FRAME /* Nett */
DO:
  RUN setFilter.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwArtBas
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
{incl/conttrigg.i oBrwArtBas:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvsluttKampanjeRecord C-Win 
PROCEDURE AvsluttKampanjeRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Linjen(e) med den/de modeller som skal tas av kampanje, må markeres ").
      RETURN.
    END.


  IF oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal markerte modeller tas av kampanje?" + CHR(10) + 
                                                     "  - Aktive kampanje stoppes." + CHR(10) + 
                                                     "  - Fremtidige kampanjer stoppes." + CHR(10) + 
                                                     "  - Artikkelen slettes fra alle kampanjelister.") THEN 
        RETURN.

    IF NOT oBrwArtBas:processRowsNoMessage("modell_avsluttKampanje.p",'16') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE 
        oBrwArtBas:REFRESH().    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
        cbLagerkoder:SCREEN-VALUE = ENTRY(2,cbLagerkoder:LIST-ITEM-PAIRS)
        cbLagAnt:SCREEN-VALUE = ENTRY(4,cbLagAnt:LIST-ITEM-PAIRS)
        .
      oBrwArtBas:setFilter('Beskr,LevKod,LevFargKod',
        ',,',
        fiBeskr:SCREEN-VALUE + "|" + fiLevKod:SCREEN-VALUE + "|" + fiLevFargKod:SCREEN-VALUE 
        ).
      oBrwArtBas:OpenQuery().
    END.
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
IF oBrwArtBas:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}:
    IF oTabArtBas:CurrTabNum = 1 AND VALID-HANDLE(oTabArtBas:getPageHandle(1)) AND AVAILABLE ArtBas THEN
      RUN getLagerForArtBas IN oTabArtBas:getPageHandle(1) (ArtBas.ArtikkelNr).
  END.
  
  RUN SUPER.
  
  IF oBrwArtBas:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
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
  DISPLAY TGKampanje fiBeskr fiLevKod fiLevFargKod cbLagant cbLagerkoder 
          tgNettbutikk 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbArtBas tbFilter TabArtBas searchArtBas first_tbArtBas prev_tbArtBas 
         next_tbArtBas last_tbArtBas refresh_tbArtBas filter_tbArtBas 
         excel_tbArtBas SendTilKasse_tbArtBas SettPubliser_tbArtBas 
         SendTilKampanje_tbArtBas TGKampanje BtnBlank fiBeskr fiLevKod 
         fiLevFargKod cbLagant cbLagerkoder tgNettbutikk BrwArtBas 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FrigiModellRecord C-Win 
PROCEDURE FrigiModellRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DO:
    IF oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      oBrwArtBas:processRowsNoMessage("artbas_frigimodell.p", STRING(iKampanjeId)).
      oBrwArtBas:openQuery().
      JBoxSession:Instance:ViewMessage("Markerte rader er frikoblet fra modell.").
    END.  
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Marker en eller flere rader som skal slås sammen til modell.").
      RETURN.
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
  SUBSCRIBE 'settModellFilter' ANYWHERE.
  
  oBrwArtBas = NEW JBoxBrowse(brwArtBas:HANDLE).
  otbArtBas = NEW JBoxToolbar(tbArtBas:HANDLE).
 
  /* Legger på eventuelle nye lagerkoder som ikke er satt opp i combo boksen. */
  IF JBoxServerApi:Instance:CallServerProc("artbas_Lagerkoder.p",cbLagerKoder:LIST-ITEM-PAIRS) THEN 
      cbLagerKoder:LIST-ITEM-PAIRS = JBoxServerApi:Instance:getCallReturnParam().
  ASSIGN 
    cbLagerkoder = ENTRY(2,cbLagerKoder:LIST-ITEM-PAIRS)
    .    
  hLagerkoderColumn = oBrwArtBas:getColumnHandle("Lagerkoder").
  hArtBas_TilbudsPrisColumn = oBrwArtBas:getColumnHandle("ArtBas_TilbudsPris").
  hArtBas_KampRab%Column = oBrwArtBas:getColumnHandle("ArtBas_KampRab%").
  hArtBas_KampPrisColumn = oBrwArtBas:getColumnHandle("ArtBas_KampPris").  

  oContainer:setNoResizeY("tbFilter").
  oContainer:setSplitBarX(btnSplitBarX:HANDLE IN FRAME frSplitBarX).
  oContainer:setFollowSplitBarX(STRING(BrwArtBas:HANDLE) + ',' + STRING(TabArtBas:HANDLE)).
  oContainer:setNoResizeX("BrwArtBas,tbFilter").

  oTabArtBas = NEW JBoxMsTabs(TabArtBas:HANDLE,oBrwArtBas).

  oTabArtBas:pageOneType = "child".

  oTabArtBas:setLinkFields("ArtikkelNr").
  oTabArtBas:AddPage("Lager butikk/størrelse","ArtikkelButStr.w").
  oTabArtBas:AddPage("Artikkel/strekkoder","ArtikkelStrekkode.w").
  oTabArtBas:AddPage("Artikkel/Prisr","ArtikkelPris.w").
  oTabArtBas:AddPage("Varetransaksjoner","Varetransaksjoner.w").

  oBrwArtBas:setSearchField(searchArtBas:HANDLE,"LevKod").

  cbLagAnt:SCREEN-VALUE = ENTRY(4,cbLagAnt:LIST-ITEM-PAIRS).

  opopupModell = NEW JBoxPopupMenu().
  opopupModell:AddToolGroup('AvsluttKampanje;Avslutt kampanje,SettModell;Slå sammen til modell,FrigiModell;Frigi fra modell'  
                           ).
  oBrwArtBas:POPUP-MENU-OBJECT = opopupModell.
  oBrwArtBas:TOOLBAR-OBJECT = otbArtBas.

  /* Oppslag fra eksternt program.  */
  /* FILTER:LevKod=XX,LevFargKod=XX */
/*  PUBLISH 'getFilterTekst' (OUTPUT pcTekst). */
/*  IF pcTekst BEGINS 'FILTER:' THEN           */
/*  DO:                                        */
/*    ASSIGN                                   */
/*      pcTekst     = ENTRY(2,pcTekst,':')     */
/*      cLevKod     = ENTRY(1,pcTekst)         */
/*      cLevFargKod = ENTRY(2,pcTekst)         */
/*      cLevKod     = ENTRY(2,cLevKod,'=')     */
/*      cLevFargKod = ENTRY(2,cLevFargKod,'=') */
/*      fiLevKod:SCREEN-VALUE     = cLevKod    */
/*      fiLevFargKod:SCREEN-VALUE = cLevFargKod*/
/*      .                                      */
/*  END.                                       */
END.
RUN setFilter.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  IF AVAILABLE ArtBas THEN 
  DO:
    IF ArtBas.Lagerkoder MATCHES '*NOS*' THEN
      hLagerkoderColumn:BGCOLOR = 12.
    ELSE IF ArtBas.Lagerkoder <> '' THEN
      hLagerkoderColumn:BGCOLOR = 10.
  IF ArtBas.ArtBas_Kampanje = 1 THEN 
  DO:
    ASSIGN 
      hArtBas_TilbudsPrisColumn:BGCOLOR = 13
      hArtBas_KampRab%Column:BGCOLOR = 13
      hArtBas_KampPrisColumn:BGCOLOR = 13
      .
  END.
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
DO WITH FRAME {&FRAME-NAME}:
  END.
/*  RUN SUPER.*/
  DO WITH FRAME {&FRAME-NAME}:
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTilKampanjeRecord C-Win 
PROCEDURE SendTilKampanjeRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF iKampanjeId > 0 THEN 
  DO:
    IF oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      oBrwArtBas:processRowsNoMessage("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).
      PUBLISH "OpenQueryKampanjeLinje".
    END.
    ELSE DO:
      IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal alle varer i utvalget sendes til kampanje?") THEN 
        RETURN.    
      ELSE DO:
        /* Ved å gjøre dette, får vi bare postene i view-porten. */
        oBrwArtBas:BROWSE-HANDLE:SELECT-ALL().
        oBrwArtBas:processRowsNoMessage("kampanjelinje_til_kampanje.p", STRING(iKampanjeId)).
        PUBLISH "OpenQueryKampanjeLinje".
      END.      
    END.  
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Funksjonen er bare tilgjengelig når listen er startet fra en kampanje! ").
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
  IF oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send artikler for valgt(e) post(er) til Nettbutikk?") THEN 
      RETURN.
    IF NOT oBrwArtBas:processRowsNoMessage("artlag_SendTilKasse.p", "") THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    ELSE 
      JBoxSession:Instance:ViewMessage("Valgte varer er sendt til kasse og nettbutikk").
  END.
  ELSE 
  DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send artikler for alle valgte poster til Nettbutikk?") THEN 
      RETURN.
    ELSE 
      JBoxSession:Instance:ViewMessage("Valgt vare er sendt til kasse og nettbutikk").
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilter C-Win 
PROCEDURE setFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   
    FIELD WebButikkArtikkel AS LOGICAL
    FIELD lager AS LOGICAL
    FIELD ArtBas_HarLager AS LOGICAL
   
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcFieldLst AS CHARACTER NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      pcFieldLst = ''
      cWhere     = ''
      pcTekst    = ''
      cOperator  = ''
      .

    IF cbLagant:SCREEN-VALUE <> '0' THEN 
      ASSIGN   
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'ArtBas_HarLager'
        cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + (IF cbLagant:SCREEN-VALUE = '1' THEN '=' ELSE '=')
        cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + (IF cbLagant:SCREEN-VALUE = '1' THEN '1' ELSE '0')
        .

    IF cbLagerkoder:SCREEN-VALUE <> '' THEN 
      ASSIGN       
      pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Lagerkoder'
      cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + (IF cbLagerkoder:SCREEN-VALUE = 'MERKET' THEN '>'  
                                                                         ELSE 'MATCHES'
                                                                         )
      cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + (IF cbLagerkoder:SCREEN-VALUE = 'MERKET' THEN '<BLANK>' ELSE IF cbLagerkoder:SCREEN-VALUE <> '' THEN '*' + cbLagerkoder:SCREEN-VALUE + '*' ELSE cbLagerkoder:SCREEN-VALUE)
      .
    
    IF fiBeskr:SCREEN-VALUE > '' THEN 
      ASSIGN 
      pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Beskr'
      cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + 'MATCHES'
      cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + ((IF TRIM(fiBeskr:SCREEN-VALUE,'*') <> '' THEN '*' ELSE '') + TRIM(fiBeskr:SCREEN-VALUE,'*') + (IF TRIM(fiBeskr:SCREEN-VALUE,'*') <> '' THEN '*' ELSE ''))
      .
                   
    IF fiLevKod:SCREEN-VALUE > '' THEN 
      ASSIGN 
      pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'LevKod'
      cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + 'BEGINS'
      cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + fiLevKod:SCREEN-VALUE
      .
      
    IF fiLevFargKod:SCREEN-VALUE > '' THEN   
      ASSIGN 
      pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'LevFargKod'
      cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + 'BEGINS'
      cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + fiLevFargKod:SCREEN-VALUE
      .               

    IF tgNettbutikk:SCREEN-VALUE = 'yes' THEN 
      ASSIGN 
      pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'WebButikkArtikkel'
      cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + '='
      cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + 'TRUE'
      .
      
    IF tgKampanje:SCREEN-VALUE = 'yes' THEN
      ASSIGN  
      pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'ArtBas_Kampanje'
      cOperator  = cOperator  + (IF cOperator  = '' THEN '' ELSE ',') + '='
      cWhere = cWhere + (IF cWhere = '' THEN '' ELSE '|') + '1'
      .
    cWhere = REPLACE(cWhere,'<BLANK>','').
          
/*MESSAGE                     */
/*'pcFieldLst' pcFieldLst SKIP*/
/*'cOperator' cOperator SKIP  */
/*'cWhere' cWhere             */
/*VIEW-AS ALERT-BOX.          */
         
    oBrwArtBas:setFilter(pcFieldLst,cOperator,cWhere).
          
  END.
  oBrwArtBas:OpenQuery().
  APPLY 'ENTRY' TO fiBeskr IN FRAME DEFAULT-FRAME.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKampanjeId C-Win 
PROCEDURE setKampanjeId :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piKampanjeId AS INTEGER NO-UNDO.
  
  ASSIGN 
    iKampanjeId = piKampanjeId
    .
  otbArtBas:disabledTools = "".
  RUN MoveToTop.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settModellFilter C-Win 
PROCEDURE settModellFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER icTekst AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF icTekst BEGINS 'FILTER:' THEN 
    DO:
      ASSIGN 
        icTekst     = ENTRY(2,icTekst,':')
        cLevKod     = ENTRY(1,icTekst)
        cLevFargKod = ENTRY(2,icTekst)
        cLevKod     = ENTRY(2,cLevKod,'=')
        cLevFargKod = ENTRY(2,cLevFargKod,'=')
        fiLevKod:SCREEN-VALUE     = cLevKod
        fiLevFargKod:SCREEN-VALUE = cLevFargKod
        cbLagerkoder              = ''
        cbLagerkoder:SCREEN-VALUE = ''
        cbLagant                  = 0
        cbLagAnt:SCREEN-VALUE     = '0'
        .
      RUN setFilter.
      RUN refreshrecord.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModellRecord C-Win 
PROCEDURE SettModellRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DO:
    IF oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
    DO:    
      oBrwArtBas:processRowsNoMessage("artbas_setmodell.p", STRING(iKampanjeId)).
/*      oBrwArtBas:refreshSelectedRows().*/
      oBrwArtBas:openQuery().  
      JBoxSession:Instance:ViewMessage("Markerte rader er slått sammen til en modell.").
    END.  
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage("Marker to eller flere rader som skal slås sammen til modell.").
      RETURN.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettPubliserRecord C-Win 
PROCEDURE SettPubliserRecord :
IF oBrwArtBas:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Publiser artikler for valgt(e) post(er) (Toggler Ja/Nei)?") THEN 
      RETURN.
    IF NOT oBrwArtBas:processRowsNoMessage("artlag_Publiser.p", "") THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    oBrwArtBas:refreshSelectedRows().  
  END.
  ELSE 
  DO:  
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Publiser artikler for alle valgte poster (Toggler Ja/Nei)?") THEN 
      RETURN.
    oBrwArtBas:processSet("artlag_Publiser.p","").
    oBrwArtBas:OpenQuery().
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

