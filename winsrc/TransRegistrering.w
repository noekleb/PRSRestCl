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

DEFINE VARIABLE bOk               AS LOG           NO-UNDO.
DEFINE VARIABLE ix                AS INTEGER       NO-UNDO.
DEFINE VARIABLE hBrowse           AS HANDLE        NO-UNDO.
DEFINE VARIABLE hQuery            AS HANDLE        NO-UNDO.
DEFINE VARIABLE hToolbar          AS HANDLE        NO-UNDO.
DEFINE VARIABLE hFieldMap         AS HANDLE        NO-UNDO.
DEFINE VARIABLE oContainer        AS JBoxContainer NO-UNDO.
DEFINE VARIABLE hMerknadColumn    AS HANDLE        NO-UNDO.
DEFINE VARIABLE cVareFelt         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cVareVerdier      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lArtikkelNr       AS DECIMAL       NO-UNDO.
DEFINE VARIABLE iButNr            AS INTEGER       NO-UNDO.
DEFINE VARIABLE iTransNr          AS INTEGER       NO-UNDO.
DEFINE VARIABLE iSeqNr            AS INTEGER       NO-UNDO.
DEFINE VARIABLE hOppdStatusColumn AS HANDLE        NO-UNDO.
DEFINE VARIABLE hRefTekstColumn   AS HANDLE        NO-UNDO.
DEFINE VARIABLE opopupTranslogg AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwBatchLogg ***/
DEFINE VARIABLE oBrwBatchLogg     AS JBoxBrowse    NO-UNDO.
DEFINE TEMP-TABLE BatchLogg
  FIELD BatchNr                    AS INTEGER
  FIELD Beskrivelse                AS CHARACTER
  FIELD Merknad                    AS CHARACTER
  FIELD OppdStatus                 AS INTEGER
  FIELD BatchLogg_DatoTidOppdatert AS CHARACTER
  FIELD OppdatertAv                AS CHARACTER
  FIELD BatchLogg_DatoTidEndret    AS CHARACTER
  FIELD BrukerID                   AS CHARACTER
  FIELD BatchLogg_DatoTidOpprettet AS CHARACTER
  FIELD RegistrertAv               AS CHARACTER
  FIELD Opphav                     AS CHARACTER
  FIELD RowIdent1                  AS CHARACTER 
  FIELD RowCount                   AS INTEGER
  FIELD jbCountDistinct            AS INTEGER   FORMAT '>>>,>>>,>>9' INIT 1
  FIELD jbAverage                  AS DECIMAL   FORMAT '->>>,>>>,>>9.99'
  INDEX idxRowids RowIdent1
  .
DEFINE BUFFER v_BatchLogg FOR TEMP-TABLE BatchLogg.


FUNCTION getBuffersAndFieldsBrwBatchLogg RETURNS CHARACTER():
  RETURN
    'BatchLogg'
    + ';BatchNr'
    + ';Beskrivelse'
    + ';Merknad'
    + ';OppdStatus'
    + ';OppdatertAv'
    + ';BrukerID'
    + ';RegistrertAv'
    + ';Opphav'
    + ';+BatchLogg_DatoTidOppdatert|CHARACTER||BatchLogg_DatoTidOppdatert(ROWID)|Oppdatert'
    + ';+BatchLogg_DatoTidEndret|CHARACTER||BatchLogg_DatoTidEndret(ROWID)|Endret'
    + ';+BatchLogg_DatoTidOpprettet|CHARACTER||BatchLogg_DatoTidOpprettet(ROWID)|Opprettet'
    .
END FUNCTION.
FUNCTION getQueryJoinBrwBatchLogg RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwBatchLogg RETURNS CHARACTER():
  RETURN 
    'server/batchlogg_brwcalc.p' /* BatchLogg_DatoTidOppdatert(ROWID) */
    + ',server/batchlogg_brwcalc.p' /* BatchLogg_DatoTidEndret(ROWID) */
    + ',server/batchlogg_brwcalc.p' /* BatchLogg_DatoTidOpprettet(ROWID) */
    .
END FUNCTION.


DEFINE VARIABLE otbBatchLogg  AS JBoxToolbar NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwTransLogg ***/
DEFINE VARIABLE oBrwTransLogg AS JBoxBrowse  NO-UNDO.
DEF TEMP-TABLE TransLogg
    FIELD Butik AS INTEGER
    FIELD TransNr AS INTEGER
    FIELD SeqNr AS INTEGER
    FIELD TTId AS INTEGER
    FIELD TBId AS INTEGER
    FIELD BongTekst AS CHARACTER
    FIELD translogg_LevKod AS CHARACTER
    FIELD translogg_LevFargKod AS CHARACTER
    FIELD Kode AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD Storl AS CHARACTER
    FIELD Varekost AS DECIMAL
    FIELD Pris AS DECIMAL
    FIELD Postert AS LOGICAL
    FIELD RefTekst AS CHARACTER
    FIELD RegistrertAv AS CHARACTER
    FIELD ArtikkelNr AS DECIMAL
    FIELD BatchNr AS INTEGER
    FIELD KassaNr AS INTEGER
    FIELD BongId AS INTEGER
    FIELD BongLinjeNr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
  .
DEFINE BUFFER v_TransLogg FOR TEMP-TABLE TransLogg.


FUNCTION getBuffersAndFieldsBrwTransLogg RETURNS CHARACTER():
  RETURN
    'TransLogg'
     + ';Butik'
     + ';TransNr'
     + ';SeqNr'
     + ';TTId'
     + ';TBId'
     + ';BongTekst'
     + ';Kode'
     + ';Antall'
     + ';Storl'
     + ';Varekost'
     + ';Pris'
     + ';Postert'
     + ';RefTekst'
     + ';RegistrertAv'
     + ';ArtikkelNr'
     + ';BatchNr'
     + ';KassaNr'
     + ';BongId'
     + ';BongLinjeNr'
     + ';+translogg_LevKod|CHARACTER||translogg_LevKod(ROWID)|Lev.art.nr'
     + ';+translogg_LevFargKod|CHARACTER||translogg_LevFargKod(ROWID)|Lev.fargekode'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwTransLogg RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwTransLogg RETURNS CHARACTER():
  RETURN 
     'server/translogg_brwcalc.p' /* translogg_LevKod(ROWID) */
   + ',server/translogg_brwcalc.p' /* translogg_LevFargKod(ROWID) */
     .
END FUNCTION.


DEFINE VARIABLE otbTranslogg AS JBoxToolbar   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwBatchLogg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BatchLogg TransLogg

/* Definitions for BROWSE BrwBatchLogg                                  */
&Scoped-define FIELDS-IN-QUERY-BrwBatchLogg BatchLogg.BatchNr ~
BatchLogg.Beskrivelse BatchLogg.Merknad BatchLogg.OppdStatus ~
BatchLogg.BatchLogg_DatoTidOppdatert BatchLogg.OppdatertAv ~
BatchLogg.BatchLogg_DatoTidEndret BatchLogg.BrukerID ~
BatchLogg.BatchLogg_DatoTidOpprettet BatchLogg.RegistrertAv ~
BatchLogg.Opphav 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwBatchLogg BatchLogg.BatchNr 
&Scoped-define QUERY-STRING-BrwBatchLogg FOR EACH BatchLogg NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwBatchLogg OPEN QUERY BrwBatchLogg FOR EACH BatchLogg NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwBatchLogg BatchLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BrwBatchLogg BatchLogg


/* Definitions for BROWSE BrwTransLogg                                  */
&Scoped-define FIELDS-IN-QUERY-BrwTransLogg TransLogg.Butik ~
TransLogg.TransNr TransLogg.SeqNr TransLogg.TTId TransLogg.TBId ~
TransLogg.BongTekst TransLogg.translogg_LevKod ~
TransLogg.translogg_LevFargKod TransLogg.Kode TransLogg.Antall ~
TransLogg.Storl TransLogg.Varekost TransLogg.Pris TransLogg.Postert ~
TransLogg.RefTekst TransLogg.RegistrertAv TransLogg.ArtikkelNr ~
TransLogg.BatchNr TransLogg.KassaNr TransLogg.BongId TransLogg.BongLinjeNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwTransLogg TransLogg.Butik ~
TransLogg.BatchNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwTransLogg TransLogg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwTransLogg TransLogg
&Scoped-define QUERY-STRING-BrwTransLogg FOR EACH TransLogg NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwTransLogg OPEN QUERY BrwTransLogg FOR EACH TransLogg NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwTransLogg TransLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BrwTransLogg TransLogg


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbBatchLogg btnSplitBarY searchBatchLogg ~
tbTranslogg first_tbBatchLogg prev_tbBatchLogg next_tbBatchLogg ~
last_tbBatchLogg new_tbBatchLogg delete_tbBatchLogg refresh_tbBatchLogg ~
filter_tbBatchLogg excel_tbBatchLogg OppdaterBatch_tbBatchLogg BrwBatchLogg ~
first_tbTranslogg prev_tbTranslogg next_tbTranslogg last_tbTranslogg ~
new_tbTranslogg delete_tbTranslogg refresh_tbTranslogg filter_tbTranslogg ~
excel_tbTranslogg cbJusteringsType fcStrekkode BrwTransLogg 
&Scoped-Define DISPLAYED-OBJECTS cbTTId cbJusteringsType fcStrekkode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 171 BY .43.

DEFINE BUTTON delete_tbBatchLogg 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON delete_tbTranslogg 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbBatchLogg 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON excel_tbTranslogg 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbBatchLogg 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON filter_tbTranslogg 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbBatchLogg 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON first_tbTranslogg 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbBatchLogg 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON last_tbTranslogg 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON new_tbBatchLogg 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON new_tbTranslogg 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbBatchLogg 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON next_tbTranslogg 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON OppdaterBatch_tbBatchLogg 
     LABEL "Oppdater batch" 
     SIZE 17.8 BY 1.1 TOOLTIP "Oppdatereer transaksjonene mot lager og statistikker.".

DEFINE BUTTON prev_tbBatchLogg 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON prev_tbTranslogg 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbBatchLogg 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON refresh_tbTranslogg 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbJusteringsType AS INTEGER FORMAT "9":U INITIAL 2 
     LABEL "Justeringstype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Økning",1,
                     "Reduksjon",2
     DROP-DOWN-LIST
     SIZE 23 BY 1 TOOLTIP "Angir om lagerjustering skal justere lagerteller opp eller ned." NO-UNDO.

DEFINE VARIABLE cbTTId AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Transaksjonstype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 30.6 BY 1 NO-UNDO.

DEFINE VARIABLE fcStrekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 26 BY 1 TOOLTIP "Skann strekkode på varer det skla opprettes transaksjon for." NO-UNDO.

DEFINE RECTANGLE searchBatchLogg
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29.6 BY .91.

DEFINE RECTANGLE tbBatchLogg
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 171.4 BY 1.29.

DEFINE RECTANGLE tbTranslogg
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 171 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwBatchLogg FOR 
      BatchLogg SCROLLING.

DEFINE QUERY BrwTransLogg FOR 
      TransLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwBatchLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwBatchLogg C-Win _STRUCTURED
  QUERY BrwBatchLogg NO-LOCK DISPLAY
      BatchLogg.BatchNr COLUMN-LABEL "Batch" FORMAT "zzzzzzzz9":U
            WIDTH 9.2
      BatchLogg.Beskrivelse COLUMN-LABEL "Beskrivelse" FORMAT "x(50)":U
      BatchLogg.Merknad COLUMN-LABEL "Merknad" FORMAT "x(50)":U
      BatchLogg.OppdStatus COLUMN-LABEL "S" FORMAT "9":U
      BatchLogg.BatchLogg_DatoTidOppdatert COLUMN-LABEL "Oppdatert" FORMAT "X(21)":U
            WIDTH 20.6
      BatchLogg.OppdatertAv COLUMN-LABEL "Oppdatert av" FORMAT "X(12)":U
      BatchLogg.BatchLogg_DatoTidEndret COLUMN-LABEL "Endret" FORMAT "X(21)":U
            WIDTH 20.6
      BatchLogg.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
            WIDTH 10.4
      BatchLogg.BatchLogg_DatoTidOpprettet COLUMN-LABEL "Opprettet" FORMAT "X(21)":U
            WIDTH 20.6
      BatchLogg.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      BatchLogg.Opphav COLUMN-LABEL "Opphav" FORMAT "X(20)":U
  ENABLE
      BatchLogg.BatchNr HELP "Batch nummer som holder sammen transaksjoner"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 171.6 BY 6.71 FIT-LAST-COLUMN.

DEFINE BROWSE BrwTransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwTransLogg C-Win _STRUCTURED
  QUERY BrwTransLogg NO-LOCK DISPLAY
      TransLogg.Butik COLUMN-LABEL "ButNr" FORMAT ">>>>>9":U
      TransLogg.TransNr COLUMN-LABEL "Transnr" FORMAT "zzzzzzz9":U
            WIDTH 8.2
      TransLogg.SeqNr COLUMN-LABEL "SeqNr" FORMAT "9":U
      TransLogg.TTId COLUMN-LABEL "TTId" FORMAT ">>9":U
      TransLogg.TBId COLUMN-LABEL "TBId" FORMAT ">>9":U
      TransLogg.BongTekst COLUMN-LABEL "Bongtekst" FORMAT "X(30)":U
      TransLogg.translogg_LevKod COLUMN-LABEL "Lev.art.nr" FORMAT "X(20)":U
      TransLogg.translogg_LevFargKod COLUMN-LABEL "Lev.fargekode" FORMAT "X(20)":U
      TransLogg.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      TransLogg.Antall COLUMN-LABEL "Antall" FORMAT "-zz,zzz,zz9":U
            WIDTH 10
      TransLogg.Storl COLUMN-LABEL "Størrelse" FORMAT "x(10)":U
      TransLogg.Varekost COLUMN-LABEL "Varekost" FORMAT "->>>,>>9.99":U
      TransLogg.Pris COLUMN-LABEL "Pris" FORMAT "-zz,zzz,zz9.99":U
            WIDTH 13
      TransLogg.Postert COLUMN-LABEL "Postert" FORMAT "Ja/Nei":U
      TransLogg.RefTekst COLUMN-LABEL "Ref.tekst" FORMAT "X(40)":U
      TransLogg.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      TransLogg.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      TransLogg.BatchNr COLUMN-LABEL "Batch" FORMAT "zzzzzzzz9":U
            WIDTH 9.2
      TransLogg.KassaNr COLUMN-LABEL "KasseNr" FORMAT "zzz9":U
      TransLogg.BongId COLUMN-LABEL "BongID" FORMAT "zz,zzz,zz9":U
            WIDTH 9.4
      TransLogg.BongLinjeNr COLUMN-LABEL "LinjeNr" FORMAT "zzzzz9":U
            WIDTH 6.6
  ENABLE
      TransLogg.Butik HELP "Butikknummer"
      TransLogg.BatchNr HELP "Batch nummer som holder sammen transaksjoner"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 171 BY 11.24 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 10.33 COL 2 WIDGET-ID 44
     first_tbBatchLogg AT ROW 1.24 COL 1.8 WIDGET-ID 4
     prev_tbBatchLogg AT ROW 1.24 COL 6.6 WIDGET-ID 6
     next_tbBatchLogg AT ROW 1.24 COL 11.2 WIDGET-ID 8
     last_tbBatchLogg AT ROW 1.24 COL 15.8 WIDGET-ID 10
     new_tbBatchLogg AT ROW 1.24 COL 20.4 WIDGET-ID 12
     delete_tbBatchLogg AT ROW 1.24 COL 25 WIDGET-ID 14
     refresh_tbBatchLogg AT ROW 1.24 COL 29.6 WIDGET-ID 16
     filter_tbBatchLogg AT ROW 1.24 COL 34.2 WIDGET-ID 18
     excel_tbBatchLogg AT ROW 1.24 COL 38.8 WIDGET-ID 20
     OppdaterBatch_tbBatchLogg AT ROW 1.24 COL 43.4 WIDGET-ID 50
     BrwBatchLogg AT ROW 3.57 COL 1.4 WIDGET-ID 200
     first_tbTranslogg AT ROW 10.95 COL 2.2 WIDGET-ID 56
     prev_tbTranslogg AT ROW 10.95 COL 7 WIDGET-ID 58
     next_tbTranslogg AT ROW 10.95 COL 11.6 WIDGET-ID 60
     last_tbTranslogg AT ROW 10.95 COL 16.2 WIDGET-ID 62
     new_tbTranslogg AT ROW 10.95 COL 20.8 WIDGET-ID 54
     delete_tbTranslogg AT ROW 10.95 COL 25.4 WIDGET-ID 64
     refresh_tbTranslogg AT ROW 10.95 COL 30 WIDGET-ID 66
     filter_tbTranslogg AT ROW 10.95 COL 34.6 WIDGET-ID 70
     excel_tbTranslogg AT ROW 10.95 COL 39.2 WIDGET-ID 68
     cbTTId AT ROW 11 COL 61 COLON-ALIGNED WIDGET-ID 48
     cbJusteringsType AT ROW 11 COL 108 COLON-ALIGNED WIDGET-ID 72
     fcStrekkode AT ROW 11 COL 143.8 COLON-ALIGNED
     BrwTransLogg AT ROW 12.19 COL 2 WIDGET-ID 300
     tbBatchLogg AT ROW 1.14 COL 1.6 WIDGET-ID 2
     searchBatchLogg AT ROW 2.48 COL 1.8 WIDGET-ID 22
     tbTranslogg AT ROW 10.86 COL 2 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 173 BY 22.67 WIDGET-ID 100.


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
         HEIGHT             = 22.67
         WIDTH              = 173
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 173
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 173
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
/* BROWSE-TAB BrwBatchLogg OppdaterBatch_tbBatchLogg DEFAULT-FRAME */
/* BROWSE-TAB BrwTransLogg fcStrekkode DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 22.67
       FRAME DEFAULT-FRAME:WIDTH            = 173.

ASSIGN 
       TransLogg.BongId:VISIBLE IN BROWSE BrwTransLogg = FALSE
       TransLogg.BongLinjeNr:VISIBLE IN BROWSE BrwTransLogg = FALSE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       cbJusteringsType:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "2".

/* SETTINGS FOR COMBO-BOX cbTTId IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbBatchLogg:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,delete;Slett,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,OppdaterBatch;Oppdater batchmaxborder".

ASSIGN 
       tbTranslogg:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,delete;Slett,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwBatchLogg
/* Query rebuild information for BROWSE BrwBatchLogg
     _TblList          = "SkoTex.BatchLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"BatchLogg.BatchNr" "Batch" "zzzzzzzz9" "INTEGER" ? ? ? ? ? ? yes "Batch nummer som holder sammen transaksjoner" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"BatchLogg.Beskrivelse" "Beskrivelse" "x(50)" "CHARACTER" ? ? ? ? ? ? no "Beskrivelse/notat om ordren" no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"BatchLogg.Merknad" "Merknad" "x(50)" "CHARACTER" ? ? ? ? ? ? no "Kort merknad til bestillingen" no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"BatchLogg.OppdStatus" "S" "9" "INTEGER" ? ? ? ? ? ? no "Oppdateringsstatus" no no "1.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"BatchLogg.BatchLogg_DatoTidOppdatert" "Oppdatert" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "20.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"BatchLogg.OppdatertAv" "Oppdatert av" "X(12)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som utførte oppdateringen" no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"BatchLogg.BatchLogg_DatoTidEndret" "Endret" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "20.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"BatchLogg.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"BatchLogg.BatchLogg_DatoTidOpprettet" "Opprettet" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "20.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"BatchLogg.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte bilde" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"BatchLogg.Opphav" "Opphav" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Programnavn på programmet som opprettet posten" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwBatchLogg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwTransLogg
/* Query rebuild information for BROWSE BrwTransLogg
     _TblList          = "SkoTex.TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > TransLogg.Butik
"TransLogg.Butik" "ButNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikknummer" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > TransLogg.TransNr
"TransLogg.TransNr" "Transnr" "zzzzzzz9" "INTEGER" ? ? ? ? ? ? no "Transaksjonsnummer" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > TransLogg.SeqNr
"TransLogg.SeqNr" "SeqNr" "9" "INTEGER" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > TransLogg.TTId
"TransLogg.TTId" "TTId" ">>9" "INTEGER" ? ? ? ? ? ? no "TransaksjonstypensID" no no "4.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > TransLogg.TBId
"TransLogg.TBId" "TBId" ">>9" "INTEGER" ? ? ? ? ? ? no "Transaksjonstype beskrivelse" no no "4.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > TransLogg.BongTekst
"TransLogg.BongTekst" "Bongtekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Bongtekst - Tekst som vises på kundens kvittering" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"TransLogg.translogg_LevKod" "Lev.art.nr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"TransLogg.translogg_LevFargKod" "Lev.fargekode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > TransLogg.Kode
"TransLogg.Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > TransLogg.Antall
"TransLogg.Antall" "Antall" "-zz,zzz,zz9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > TransLogg.Storl
"TransLogg.Storl" "Størrelse" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > TransLogg.Varekost
"TransLogg.Varekost" "Varekost" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "KAlkulert varekost (Hentet fra gjeldende kalkyle)." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > TransLogg.Pris
"TransLogg.Pris" "Pris" "-zz,zzz,zz9.99" "DECIMAL" ? ? ? ? ? ? no "Pris" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > TransLogg.Postert
"TransLogg.Postert" "Postert" "Ja/Nei" "LOGICAL" ? ? ? ? ? ? no "Postert i lager og statistikker" no no "6.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > TransLogg.RefTekst
"TransLogg.RefTekst" "Ref.tekst" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Referansetekst fra kassen" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > TransLogg.RegistrertAv
"TransLogg.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > TransLogg.ArtikkelNr
"TransLogg.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > TransLogg.BatchNr
"TransLogg.BatchNr" "Batch" "zzzzzzzz9" "INTEGER" ? ? ? ? ? ? yes "Batch nummer som holder sammen transaksjoner" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > TransLogg.KassaNr
"TransLogg.KassaNr" "KasseNr" "zzz9" "INTEGER" ? ? ? ? ? ? no "Kassenummer" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > TransLogg.BongId
"TransLogg.BongId" "BongID" "zz,zzz,zz9" "INTEGER" ? ? ? ? ? ? no "BongId" no no "9.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > TransLogg.BongLinjeNr
"TransLogg.BongLinjeNr" "LinjeNr" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "Linjenummer" no no "6.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwTransLogg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
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
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
  /*  apply "window-resized" to {&window-name}.*/
  /*   RUN MoveToTop. */
  /*   DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,OrderFolder:HANDLE). */
  /*   RUN MoveToTop IN hCurrTabProc NO-ERROR.                            */
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTTId C-Win
ON VALUE-CHANGED OF cbTTId IN FRAME DEFAULT-FRAME /* Transaksjonstype */
DO:
    DO WITH FRAME DEFAULT-FRAME:
      ASSIGN 
        cbTTId = INT(cbTTId:SCREEN-VALUE)
        .
    END.  
  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fcStrekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcStrekkode C-Win
ON LEAVE OF fcStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
    IF AVAILABLE BatchLogg AND BatchLogg.OppdStatus <> 0 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Batch'en er allerede oppdatert. Nye linjer kan ikke registreres.").
      RETURN.
    END.
    IF fcStrekkode:SCREEN-VALUE <> '' AND AVAILABLE BatchLogg THEN 
    DO:
      IF NOT JBoxServerAPI:Instance:Find("Strekkode", "WHERE Kode = '"+ fcStrekkode:SCREEN-VALUE + "'") THEN
      DO:
        JBoxSession:Instance:ViewMessage("Ukjent strekkode!").
        RETURN.
      END.  
      ELSE 
      DO:
        lArtikkelNr = DEC(JBoxServerAPI:Instance:FieldValue("Strekkode.ArtikkelNr")).
        IF NOT JBoxServerAPI:Instance:Find("ArtBas", "WHERE ArtikkelNr = '" + STRING(lArtikkelNr) + "'") THEN
        DO:
          JBoxSession:Instance:ViewMessage("Ukjent artikkel knyttet til strekkoden!").
          RETURN.
        END.
      END.
      IF NOT JBoxServerApi:Instance:CallServerProc("translogg_NyFraStrekkode.p",  
        STRING(BatchLogg.BatchNr) + "|" + 
        fcStrekkode:SCREEN-VALUE +  
        '|1|16|' +
        cbTTId:SCREEN-VALUE  + 
        '|' + (IF INT(cbJusteringsType:SCREEN-VALUE) = 1 THEN '-1' 
      ELSE '1')
        ) THEN 
      DO: 
        JBoxSession:Instance:ViewMessage("Feil ved opprettelse av linje: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
      IF NUM-ENTRIES(JBoxServerApi:Instance:getCallReturnParam(),'|') >= 3 THEN 
      DO:
        ASSIGN 
          iButNr   = INT(ENTRY(1,JBoxServerApi:Instance:getCallReturnParam(),'|'))
          iTransNr = INT(ENTRY(2,JBoxServerApi:Instance:getCallReturnParam(),'|'))        
          iSeqNr   = INT(ENTRY(3,JBoxServerApi:Instance:getCallReturnParam(),'|'))
          .
        /* Akkumulerer hvis varen finnes på an varelinje fra før. */
        FIND FIRST v_Translogg NO-LOCK WHERE 
          v_Translogg.Butik   = iButNr AND
          v_TransLogg.TransNr = iTransNr AND  
          v_TransLogg.SeqNr   = iSeqNr NO-ERROR.
        /* Akkumulerer */  
        IF AVAILABLE v_TransLogg THEN
        DO:
          oBrwTransLogg:BROWSE-HANDLE:QUERY:REPOSITION-TO-ROWID(ROWID(v_Translogg)).        
          oBrwTranslogg:refreshRow(). /* for å friske opp raden. */
          oBrwTranslogg:displayRecord(). /* For å friske opp update feltet hvis dette er aktivt på raden. */
        END.
        /* Viser ny linje. */
        ELSE 
        DO:
          oBrwTranslogg:OpenQuery().
        END.
      END.
      ELSE
        oBrwTranslogg:OpenQuery().
      ASSIGN 
        fcStrekkode:SCREEN-VALUE = ''
        fcStrekkode              = ''
        .
      APPLY 'ENTRY' TO fcStrekkode IN FRAME DEFAULT-FRAME.
      RETURN NO-APPLY. /* NB: Dette er nødvendig for at ikke 'ENTRY' ovenfor skal overstyres av progress. */
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcStrekkode C-Win
ON RETURN OF fcStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
    APPLY 'LEAVE' TO fcStrekkode.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwBatchLogg
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
{incl/conttrigg.i oBrwBatchLogg:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE BatchLogg THEN 
    DO:
      IF BatchLogg.OppdStatus <> 0 THEN 
        RETURN.
    END.    
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbBatchLogg:isCurrent THEN
    DO:
      IF AVAILABLE BatchLogg THEN 
      DO:
        IF BatchLogg.OppdStatus <> 0 THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Batchen er oppdatert. Kan ikke slettes.").
          RETURN.        
        END.
        ELSE 
        DO:
          JBoxServerApi:Instance:Update("BatchLogg",
            BatchLogg.RowIdent1,
            'eTime',
            STRING(TIME),
            FALSE,
            "batchlogg_post_delete.p",
            TRUE).
        END.      
      END.
    END.
    IF otbTranslogg:isCurrent THEN
    DO:
      IF AVAILABLE TransLogg THEN 
      DO:
        IF TransLogg.Postert = TRUE THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Batchen er oppdatert. Linjer på batchen kan ikke slettes.").
          RETURN.
        END.
      END.
    END.    
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
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
IF oBrwBatchLogg:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  IF oBrwBatchLogg:isCurrent THEN  
  DO WITH FRAME {&FRAME-NAME}:
  END.
  IF AVAILABLE BatchLogg THEN 
  DO:
    IF BatchLogg.OppdStatus = 0 THEN
      ASSIGN 
        otbBatchLogg:disabledTools = ""
        fcStrekkode:SENSITIVE      = TRUE 
        .
    ELSE
      ASSIGN 
        otbBatchLogg:disabledTools = "Delete,OppdaterBatch"
        fcStrekkode:SENSITIVE      = FALSE 
        .
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
  DISPLAY cbTTId cbJusteringsType fcStrekkode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbBatchLogg btnSplitBarY searchBatchLogg tbTranslogg first_tbBatchLogg 
         prev_tbBatchLogg next_tbBatchLogg last_tbBatchLogg new_tbBatchLogg 
         delete_tbBatchLogg refresh_tbBatchLogg filter_tbBatchLogg 
         excel_tbBatchLogg OppdaterBatch_tbBatchLogg BrwBatchLogg 
         first_tbTranslogg prev_tbTranslogg next_tbTranslogg last_tbTranslogg 
         new_tbTranslogg delete_tbTranslogg refresh_tbTranslogg 
         filter_tbTranslogg excel_tbTranslogg cbJusteringsType fcStrekkode 
         BrwTransLogg 
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
  DEFINE VARIABLE oBrwFillInMerknad  AS JBoxBrowseFillIn NO-UNDO.
  DEFINE VARIABLE oBrwFillInRefTekst AS JBoxBrowseFillIn NO-UNDO.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

  DO WITH FRAME {&FRAME-NAME}:
 
    SUBSCRIBE 'msgFraLagerListeButArtStr' ANYWHERE.
    
    oBrwBatchLogg = NEW JBoxBrowse(brwBatchLogg:HANDLE).
    otbBatchLogg = NEW JBoxToolbar(tbBatchLogg:HANDLE).

    oBrwBatchLogg:setNoResizeY().
    oBrwBatchLogg:TOOLBAR-OBJECT = otbBatchLogg.
    oBrwBatchLogg:setSearchField(searchBatchLogg:HANDLE,"").
    oBrwBatchLogg:postUpdateProc = "batchlogg_post_update.p".
    oBrwBatchLogg:baseQuery  = "WHERE Opphav = 'eComLager' ".
    oBrwBatchLogg:setQuerySort('Opphav,BatchNr;DESC').
    oBrwBatchLogg:customDeleteValProc = "ignore". /* Fjerner vlaidering på om det ligger Ovbuffer poster under PkSdlHode. */

    oBrwTransLogg = NEW JBoxBrowse(brwTransLogg:HANDLE).
 
    oBrwTranslogg:setParentBrowseObject(oBrwBatchLogg,"BatchNr").
    oBrwTranslogg:postUpdateProc = "Translogg_post_update.p".
    oBrwTranslogg:customCreateProc = "create_translogg.p". /* Fjerner vlaidering på om det ligger Ovbuffer poster under PkSdlHode. */
    /*    oBrwTranslogg:dynamicUpdateValidation = FALSE.*/
    DYNAMIC-FUNCTION("setAttribute",BrwTranslogg:HANDLE,"customUpdateValProc","ignore").
    otbTranslogg = NEW JBoxToolbar(tbTranslogg:HANDLE).

    otbTranslogg:BROWSE-OBJECT = oBrwTranslogg.

    oBrwFillInRefTekst = NEW JBoxBrowseFillIn(oBrwTranslogg,"RefTekst",TRUE).
    hRefTekstColumn = oBrwTranslogg:getColumnHandle("RefTekst").
    hOppdStatusColumn = oBrwBatchLogg:getColumnHandle("OppdStatus").

    oContainer:setSplitBarY(btnSplitBarY:HANDLE).
    oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,300). /* 200 pixels from the top, 300 pixels from the bottom */
    oContainer:setFollowSplitBarY(STRING(BrwBatchLogg:HANDLE) + ',' + 
      STRING(BrwTranslogg:HANDLE) + ',' +
      STRING(first_tbTranslogg:HANDLE) + ',' +
      STRING(prev_tbTranslogg:HANDLE) + ',' +
      STRING(next_tbTranslogg:HANDLE) + ',' +
      STRING(last_tbTranslogg:HANDLE) + ',' +
      STRING(new_tbTranslogg:HANDLE) + ',' +
      STRING(filter_tbTranslogg:HANDLE) + ',' +
      STRING(refresh_tbTranslogg:HANDLE) + ',' +
      STRING(excel_tbTranslogg:HANDLE) + ',' +
      STRING(delete_tbTranslogg:HANDLE) + ',' +
      STRING(cbTTId:HANDLE) + ',' +
      STRING(cbJusteringsType:HANDLE) + ',' +      
      STRING(fcStrekkode:HANDLE)
      ).
    oContainer:setNoResizeY("BrwBatchLogg").
  
    cbTTId:DELIMITER = '|'.
    cbTTId:LIST-ITEM-PAIRS = "0/|0|" + DYNAMIC-FUNCTION("getFieldList","TransType;TTId|Beskrivelse;TTId","WHERE TRUE").
    cbTTId:SCREEN-VALUE = '7'.
  
    oBrwBatchLogg:setReadOnlyOnReturn = YES.
    oBrwBatchLogg:enableOnDblClick = YES.
    oBrwTranslogg:setReadOnlyOnReturn = YES.
    oBrwTranslogg:enableOnDblClick = YES.
    
    hMerknadColumn = oBrwBatchLogg:getColumnHandle("Merknad").
    oBrwFillInMerknad = NEW JBoxBrowseFillIn(oBrwBatchLogg,"Merknad",TRUE).
    oBrwBatchLogg:enabledColumns = "Merknad".
    
    opopupTranslogg = NEW JBoxPopupMenu().
    opopupTranslogg:AddToolGroup('OppslagModell;Vis i modell liste'  
                             ).
    oBrwTranslogg:POPUP-MENU-OBJECT = opopupTranslogg.
    
  END.
  oBrwBatchLogg:OpenQuery().

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  oContainer:initResize().
&ELSE
  DYNAMIC-FUNCTION("setASlibBehaviour",
    "QueryLogFile|ServerQuery.log," +   
    "TransLogFile|ServerTrans.log").
&ENDIF
  APPLY 'ENTRY' TO fcStrekkode IN FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msgFraLagerListeButArtStr C-Win 
PROCEDURE msgFraLagerListeButArtStr :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER icVareFelt AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER icVareVerdier AS CHARACTER NO-UNDO.
  
  ASSIGN 
    cVareFelt    = icVareFelt
    cVareVerdier = icVareVerdier
    . 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
DEFINE INPUT  PARAMETER ihFillIn AS HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
  DEFINE OUTPUT PARAMETER obOk     AS LOG    NO-UNDO.

  IF AVAILABLE BatchLogg THEN 
  DO:
    JBoxServerApi:Instance:Update("BatchLogg",
      BatchLogg.RowIdent1,
      DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
      ihFillIn:SCREEN-VALUE,
      FALSE,
      "",
      TRUE).
    oBrwBatchLogg:refreshRow().
  END.
  IF AVAILABLE Translogg THEN 
  DO:
    JBoxServerApi:Instance:Update("Translogg",
      Translogg.RowIdent1,
      DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
      ihFillIn:SCREEN-VALUE,
      FALSE,
      "",
      TRUE).
    oBrwTranslogg:refreshRow().
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbBatchLogg:isCurrent THEN 
    DO:
    END.
  
    IF otbTranslogg:isCurrent THEN
    DO:
      IF AVAILABLE BatchLogg AND BatchLogg.OppdStatus <> 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Batch er oppdatert. Ny rad kan ikke legges til.").
        RETURN.
      END.
    END.
  END.
  
  RUN SUPER.

  DO WITH FRAME {&FRAME-NAME}:
    IF otbBatchLogg:isCurrent THEN 
    DO:
      IF AVAILABLE BatchLogg THEN 
      DO:
        /* Nå er det enkelt å bygge ut denne rutinen .... */
        CASE cbTTId:SCREEN-VALUE:
          WHEN '7' THEN 
            JBoxServerApi:Instance:Update("BatchLogg",
              BatchLogg.RowIdent1,
              "Beskrivelse,Opphav",
              "Lagerjustering fra lager eCom." + "|eComLager",
              FALSE,
              "",
              TRUE).
        END CASE.   
        oBrwBatchLogg:refreshRow().
        APPLY 'ENTRY' TO fcStrekkode IN FRAME DEFAULT-FRAME.
        RETURN NO-APPLY.
      END.   
    END.
  
    IF otbTranslogg:isCurrent THEN
    DO:
      IF AVAILABLE Translogg THEN
      DO:
        CURRENT-WINDOW:SENSITIVE = FALSE.
        ASSIGN
          cVareFelt    = ''
          cVareVerdier = ''
          .
        RUN LagerListeButArtStr.w ('16|30|').
        CURRENT-WINDOW:SENSITIVE = TRUE.
        IF cVareFelt = '' OR cVarefelt = ? THEN
        DO:
          JBoxServerApi:Instance:Delete("Translogg", Translogg.RowIdent1).
          oBrwTranslogg:OpenQuery().
        END.
        ELSE
        DO:
          IF NOT JBoxServerApi:Instance:Update("Translogg",
            Translogg.RowIdent1,
            'Postert,TTId,TBId,Antall,Butik,Kode,' + cVareFelt,
            'FALSE|7|1|' + (IF INT(cbJusteringsType:SCREEN-VALUE) = 1 THEN '-1' 
          ELSE '1') + '|16||' + cVareVerdier,
            FALSE,
            "translogg_post_update.p",
            TRUE) THEN
          DO:
            JBoxSession:Instance:ViewMessage("Feil ved opprettelse av rad pga " + JBoxServerAPI:Instance:getCallMessage()).
            RETURN.
          END.
          oBrwTranslogg:refreshRow().
          APPLY 'ENTRY' TO fcStrekkode IN FRAME DEFAULT-FRAME.
          RETURN NO-APPLY.
        END.
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterBatchRecord C-Win 
PROCEDURE OppdaterBatchRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbBatchLogg:isCurrent THEN 
    DO:
      IF AVAILABLE BatchLogg THEN
      DO: 
        IF BatchLogg.OppdStatus <> 0 THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Batchen er allerede oppdatert. ").
          RETURN.
        END.
        JBoxServerApi:Instance:Update("BatchLogg",
          BatchLogg.RowIdent1,
          "OppdStatus,StatusOppdatert,tidOppdatet,OppdatertAv",
          "2|" + STRING(TODAY ) + "|" + STRING(TIME) + "|" + JBoxSession:Instance:UserName,
          FALSE,
          "",
          TRUE).   
        oBrwBatchLogg:refreshRow().
      END.   
    END.

  /*    ELSE IF otbTranslogg:isCurrent THEN*/
  /*      DO:                              */
  /*      END.                             */
  
  END.
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
    IF AVAILABLE Translogg THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',Translogg.Translogg_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',Translogg.Translogg_LevFargKod)
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



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  DO WITH FRAME {&FRAME-NAME}: 
    IF AVAILABLE BatchLogg THEN
    DO:
      IF BatchLogg.OppdStatus = 3 THEN
        hOppdStatusColumn:BGCOLOR = 12. /* Rød */
      ELSE IF BatchLogg.OppdStatus = 2 THEN
          hOppdStatusColumn:BGCOLOR = 14. /* Gul */
        ELSE IF BatchLogg.OppdStatus = 0 THEN
            hOppdStatusColumn:BGCOLOR = 10. /* Grønn */
          ELSE 
            hOppdStatusColumn:BGCOLOR = ?.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

