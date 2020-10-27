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
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.
rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner( ) NO-ERROR.

/*** Start instance property definitions for JBoxBrowse object oBrwTransLogg ***/
DEF VAR oBrwTransLogg AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE TransLogg
    FIELD BatchNr AS INTEGER
    FIELD Butik AS INTEGER
    FIELD OvButik AS INTEGER
    FIELD translogg_TransType AS CHARACTER
    FIELD TBId AS INTEGER
    FIELD Dato AS DATE
    FIELD translogg_Tid AS CHARACTER
    FIELD Kode AS CHARACTER
    FIELD Storl AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD Pris AS DECIMAL
    FIELD RabKr AS DECIMAL
    FIELD translogg_PBruttoPris AS DECIMAL
    FIELD Mva AS DECIMAL
    FIELD VVarekost AS DECIMAL
    FIELD PostertDato AS DATE
    FIELD translogg_PostertTid AS CHARACTER
    FIELD RefNr AS INTEGER
    FIELD RefTekst AS CHARACTER
    FIELD KalkylePris AS DECIMAL
    FIELD KortNr AS CHARACTER
    FIELD Plukket AS LOGICAL
    FIELD KassaNr AS INTEGER
    FIELD BongId AS INTEGER
    FIELD BongLinjeNr AS INTEGER
    FIELD SeqNr AS INTEGER
    FIELD BongTekst AS CHARACTER
    FIELD Varekost AS DECIMAL
    FIELD Mva% AS DECIMAL
    FIELD LinjeRab AS DECIMAL
    FIELD KundNr AS DECIMAL
    FIELD ForsNr AS DECIMAL
    FIELD TTId AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD Postert AS LOGICAL
    FIELD Tid AS INTEGER
    FIELD TransNr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_TransLogg FOR TEMP-TABLE TransLogg.


FUNCTION getBuffersAndFieldsBrwTransLogg RETURNS CHARACTER():
  RETURN
    'TransLogg'
     + ';BatchNr'
     + ';Butik'
     + ';OvButik'
     + ';TBId'
     + ';Dato'
     + ';Kode'
     + ';Storl'
     + ';Antall'
     + ';Pris'
     + ';RabKr'
     + ';Mva'
     + ';VVarekost'
     + ';PostertDato'
     + ';RefNr'
     + ';RefTekst'
     + ';KalkylePris'
     + ';KortNr'
     + ';Plukket'
     + ';KassaNr'
     + ';BongId'
     + ';BongLinjeNr'
     + ';SeqNr'
     + ';BongTekst'
     + ';Varekost'
     + ';Mva%'
     + ';LinjeRab'
     + ';KundNr'
     + ';ForsNr'
     + ';TTId'
     + ';ArtikkelNr'
     + ';Postert'
     + ';Tid'
     + ';TransNr'
     + ';+translogg_TransType|CHARACTER||translogg_TransType(ROWID)|TTId'
     + ';+translogg_Tid|CHARACTER||translogg_Tid(ROWID)|Tid'
     + ';+translogg_PBruttoPris|DECIMAL||translogg_BruttoPris(ROWID)|Nettopris'
     + ';+translogg_PostertTid|CHARACTER||translogg_PostertTid(ROWID)|Tid'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwTransLogg RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwTransLogg RETURNS CHARACTER():
  RETURN 
     'server/translogg_brwcalc.p' /* translogg_TransType(ROWID) */
   + ',server/translogg_brwcalc.p' /* translogg_Tid(ROWID) */
   + ',server/translogg_brwcalc.p' /* translogg_BruttoPris(ROWID) */
   + ',server/translogg_brwcalc.p' /* translogg_PostertTid(ROWID) */
     .
END FUNCTION.
DEF VAR otbTransLogg AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwTransLogg

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TransLogg

/* Definitions for BROWSE BrwTransLogg                                  */
&Scoped-define FIELDS-IN-QUERY-BrwTransLogg TransLogg.BatchNr ~
TransLogg.Butik TransLogg.OvButik TransLogg.translogg_TransType ~
TransLogg.TBId TransLogg.Dato TransLogg.translogg_Tid TransLogg.Kode ~
TransLogg.Storl TransLogg.Antall TransLogg.Pris TransLogg.RabKr ~
TransLogg.translogg_PBruttoPris TransLogg.Mva TransLogg.VVarekost ~
TransLogg.PostertDato TransLogg.translogg_PostertTid TransLogg.RefNr ~
TransLogg.RefTekst TransLogg.KalkylePris TransLogg.KortNr TransLogg.Plukket ~
TransLogg.KassaNr TransLogg.BongId TransLogg.BongLinjeNr TransLogg.SeqNr ~
TransLogg.BongTekst TransLogg.Varekost TransLogg.Mva% TransLogg.LinjeRab ~
TransLogg.KundNr TransLogg.ForsNr TransLogg.TTId TransLogg.ArtikkelNr ~
TransLogg.Postert TransLogg.Tid TransLogg.TransNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwTransLogg TransLogg.BatchNr ~
TransLogg.ArtikkelNr 
&Scoped-define QUERY-STRING-BrwTransLogg FOR EACH TransLogg NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwTransLogg OPEN QUERY BrwTransLogg FOR EACH TransLogg NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwTransLogg TransLogg
&Scoped-define FIRST-TABLE-IN-QUERY-BrwTransLogg TransLogg


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbTransLogg cbTransType btBlank ~
first_tbTransLogg prev_tbTransLogg next_tbTransLogg last_tbTransLogg ~
filter_tbTransLogg refresh_tbTransLogg excel_tbTransLogg fiButNr fiOvButNr ~
fiStorl fiFraDato fiTilDato BrwTransLogg 
&Scoped-Define DISPLAYED-OBJECTS cbTransType fiButNr fiOvButNr fiStorl ~
fiFraDato fiTilDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btBlank 
     LABEL "<Blank filterr>" 
     SIZE 20 BY 1.1 TOOLTIP "Blanker filteret og åpner spørringen igjen.".

DEFINE BUTTON excel_tbTransLogg 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbTransLogg 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbTransLogg 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbTransLogg 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbTransLogg 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbTransLogg 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbTransLogg 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbTransType AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Transaksjonstype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE fiButNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Butikk transaksjonen er registrert i." NO-UNDO.

DEFINE VARIABLE fiFraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra/til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiOvButNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Ov.butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Overført til butikk" NO-UNDO.

DEFINE VARIABLE fiStorl AS CHARACTER FORMAT "X(256)":U 
     LABEL "Str." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Størrelse" NO-UNDO.

DEFINE VARIABLE fiTilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE tbTransLogg
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 110.6 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwTransLogg FOR 
      TransLogg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwTransLogg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwTransLogg C-Win _STRUCTURED
  QUERY BrwTransLogg NO-LOCK DISPLAY
      TransLogg.BatchNr COLUMN-LABEL "Batch" FORMAT "zzzzzzzz9":U
            WIDTH 9.2
      TransLogg.Butik COLUMN-LABEL "ButNr" FORMAT ">>>>>9":U
      TransLogg.OvButik COLUMN-LABEL "ButNr" FORMAT ">>>>>9":U
      TransLogg.translogg_TransType COLUMN-LABEL "TTId" FORMAT "X(8)":U
            WIDTH 5
      TransLogg.TBId COLUMN-LABEL "TBId" FORMAT ">>9":U WIDTH 5
      TransLogg.Dato COLUMN-LABEL "Dato" FORMAT "99/99/9999":U
      TransLogg.translogg_Tid COLUMN-LABEL "Tid" FORMAT "X(10)":U
      TransLogg.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      TransLogg.Storl COLUMN-LABEL "Størrelse" FORMAT "x(10)":U
      TransLogg.Antall COLUMN-LABEL "Antall" FORMAT "-zz,zzz,zz9":U
            WIDTH 10
      TransLogg.Pris COLUMN-LABEL "Pris" FORMAT "-zz,zzz,zz9.99":U
            WIDTH 13
      TransLogg.RabKr COLUMN-LABEL "Rabatt" FORMAT "->,>>>,>>9.99":U
      TransLogg.translogg_PBruttoPris COLUMN-LABEL "Nettopris" FORMAT "->>>,>>9.99":U
            WIDTH 10
      TransLogg.Mva COLUMN-LABEL "Mva" FORMAT "->,>>>,>>9.99":U
      TransLogg.VVarekost COLUMN-LABEL "VVarekost" FORMAT "-z,zzz,zz9.99":U
            WIDTH 12
      TransLogg.PostertDato COLUMN-LABEL "PostertDato" FORMAT "99/99/9999":U
      TransLogg.translogg_PostertTid COLUMN-LABEL "Tid" FORMAT "X(10)":U
      TransLogg.RefNr COLUMN-LABEL "RefNr" FORMAT "->,>>>,>>9":U
      TransLogg.RefTekst COLUMN-LABEL "Ref.tekst" FORMAT "X(40)":U
      TransLogg.KalkylePris COLUMN-LABEL "Kalkylepris" FORMAT "->>,>>>,>>9.99":U
      TransLogg.KortNr COLUMN-LABEL "Kortnummer" FORMAT "X(22)":U
      TransLogg.Plukket COLUMN-LABEL "Plukket" FORMAT "Ja/Nei":U
      TransLogg.KassaNr COLUMN-LABEL "KasseNr" FORMAT "zzz9":U
      TransLogg.BongId COLUMN-LABEL "BongID" FORMAT "zz,zzz,zz9":U
            WIDTH 9.4
      TransLogg.BongLinjeNr COLUMN-LABEL "LinjeNr" FORMAT "zzzzz9":U
            WIDTH 6.6
      TransLogg.SeqNr COLUMN-LABEL "SeqNr" FORMAT "9":U
      TransLogg.BongTekst COLUMN-LABEL "Bongtekst" FORMAT "X(30)":U
      TransLogg.Varekost COLUMN-LABEL "Kalkulert varekost" FORMAT "->>,>>9.99":U
      TransLogg.Mva% COLUMN-LABEL "Mva%" FORMAT "->>9.99":U
      TransLogg.LinjeRab COLUMN-LABEL "Linjerabatt" FORMAT "->,>>>,>>9.99":U
      TransLogg.KundNr COLUMN-LABEL "KundeNr" FORMAT ">>>>>>>>>>>>9":U
      TransLogg.ForsNr COLUMN-LABEL "Kasserer" FORMAT ">>>>>9":U
      TransLogg.TTId COLUMN-LABEL "TransTypeId" FORMAT "zz9":U
      TransLogg.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      TransLogg.Postert COLUMN-LABEL "Postert" FORMAT "Ja/Nei":U
      TransLogg.Tid COLUMN-LABEL "Tid" FORMAT "->,>>>,>>9":U
      TransLogg.TransNr COLUMN-LABEL "TransaksjonsNr" FORMAT "zz,zzz,zz9":U
  ENABLE
      TransLogg.BatchNr HELP "Batch nummer som holder sammen transaksjoner"
      TransLogg.ArtikkelNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110.6 BY 16.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cbTransType AT ROW 1.24 COL 81.8 COLON-ALIGNED WIDGET-ID 26
     btBlank AT ROW 1.33 COL 35.6 WIDGET-ID 28
     first_tbTransLogg AT ROW 1.38 COL 2.6 WIDGET-ID 4
     prev_tbTransLogg AT ROW 1.38 COL 7.2 WIDGET-ID 6
     next_tbTransLogg AT ROW 1.38 COL 12 WIDGET-ID 8
     last_tbTransLogg AT ROW 1.38 COL 16.6 WIDGET-ID 10
     filter_tbTransLogg AT ROW 1.38 COL 21.4 WIDGET-ID 12
     refresh_tbTransLogg AT ROW 1.38 COL 26 WIDGET-ID 14
     excel_tbTransLogg AT ROW 1.38 COL 30.8 WIDGET-ID 16
     fiButNr AT ROW 2.91 COL 8.6 COLON-ALIGNED
     fiOvButNr AT ROW 2.91 COL 34.6 COLON-ALIGNED
     fiStorl AT ROW 2.91 COL 55.2 COLON-ALIGNED
     fiFraDato AT ROW 2.91 COL 81.8 COLON-ALIGNED
     fiTilDato AT ROW 2.91 COL 96.4 COLON-ALIGNED NO-LABEL
     BrwTransLogg AT ROW 4 COL 2.4 WIDGET-ID 200
     tbTransLogg AT ROW 1.29 COL 2.4 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 112.4 BY 19.24 WIDGET-ID 100.


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
         HEIGHT             = 19.24
         WIDTH              = 112.4
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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
/* BROWSE-TAB BrwTransLogg fiTilDato DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.24
       FRAME DEFAULT-FRAME:WIDTH            = 112.4.

ASSIGN 
       tbTransLogg:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,filter;Filter,refresh;Refresh,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwTransLogg
/* Query rebuild information for BROWSE BrwTransLogg
     _TblList          = "SkoTex.TransLogg"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"TransLogg.BatchNr" "Batch" "zzzzzzzz9" "INTEGER" ? ? ? ? ? ? yes "Batch nummer som holder sammen transaksjoner" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"TransLogg.Butik" "ButNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikknummer" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"TransLogg.OvButik" "ButNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikknummer" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"TransLogg.translogg_TransType" "TTId" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"TransLogg.TBId" "TBId" ">>9" "INTEGER" ? ? ? ? ? ? no "." no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"TransLogg.Dato" "Dato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Transaksjonsdato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"TransLogg.translogg_Tid" "Tid" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"TransLogg.Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"TransLogg.Storl" "Størrelse" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"TransLogg.Antall" "Antall" "-zz,zzz,zz9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"TransLogg.Pris" "Pris" "-zz,zzz,zz9.99" "DECIMAL" ? ? ? ? ? ? no "Pris" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"TransLogg.RabKr" "Rabatt" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Rabatt" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"TransLogg.translogg_PBruttoPris" "Nettopris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"TransLogg.Mva" "Mva" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Mva" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"TransLogg.VVarekost" "VVarekost" "-z,zzz,zz9.99" "DECIMAL" ? ? ? ? ? ? no "Vektet varekost" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"TransLogg.PostertDato" "PostertDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Postert dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"TransLogg.translogg_PostertTid" "Tid" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"TransLogg.RefNr" "RefNr" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Referansenummer fra kassen" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"TransLogg.RefTekst" "Ref.tekst" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Referansetekst fra kassen" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"TransLogg.KalkylePris" "Kalkylepris" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris hentet fra artikkelens aktive kalkyle" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"TransLogg.KortNr" "Kortnummer" "X(22)" "CHARACTER" ? ? ? ? ? ? no "Kortnummer (Kunde eller medlemskort)" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"TransLogg.Plukket" "Plukket" "Ja/Nei" "LOGICAL" ? ? ? ? ? ? no "Plukket" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"TransLogg.KassaNr" "KasseNr" "zzz9" "INTEGER" ? ? ? ? ? ? no "Kassenummer" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"TransLogg.BongId" "BongID" "zz,zzz,zz9" "INTEGER" ? ? ? ? ? ? no "BongId" no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"TransLogg.BongLinjeNr" "LinjeNr" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "Linjenummer" no no "6.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"TransLogg.SeqNr" "SeqNr" "9" "INTEGER" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"TransLogg.BongTekst" "Bongtekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Bongtekst - Tekst som vises på kundens kvittering" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"TransLogg.Varekost" "Kalkulert varekost" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "KAlkulert varekost (Hentet fra gjeldende kalkyle)." no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"TransLogg.Mva%" "Mva%" "->>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"TransLogg.LinjeRab" "Linjerabatt" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjerabatt" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"TransLogg.KundNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"TransLogg.ForsNr" "Kasserer" ">>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kasserernummer" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"TransLogg.TTId" "TransTypeId" "zz9" "INTEGER" ? ? ? ? ? ? no "TransaksjonstypensID" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"TransLogg.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? yes "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > "_<CALC>"
"TransLogg.Postert" "Postert" "Ja/Nei" "LOGICAL" ? ? ? ? ? ? no "Postert i lager og statistikker" no no "6.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > "_<CALC>"
"TransLogg.Tid" "Tid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tid" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > "_<CALC>"
"TransLogg.TransNr" "TransaksjonsNr" "zz,zzz,zz9" "INTEGER" ? ? ? ? ? ? no "Transaksjonsnummer" no no "14.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwTransLogg */
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


&Scoped-define SELF-NAME btBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btBlank C-Win
ON CHOOSE OF btBlank IN FRAME DEFAULT-FRAME /* <Blank filterr> */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        cbTransType:SCREEN-VALUE = ''
        fiButNr:SCREEN-VALUE     = ''
        fiOvButNr:SCREEN-VALUE   = ''
        fiStorl:SCREEN-VALUE     = ''
        fiFraDato:SCREEN-VALUE   = ?
        fiTilDato:SCREEN-VALUE   = ?
        .
    RUN setFilter.  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME cbTransType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTransType C-Win
ON VALUE-CHANGED OF cbTransType IN FRAME DEFAULT-FRAME /* Transaksjonstype */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiButNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiButNr C-Win
ON RETURN OF fiButNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  RUN setFilter.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiButNr C-Win
ON TAB OF fiButNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato C-Win
ON RETURN OF fiFraDato IN FRAME DEFAULT-FRAME /* Fra/til dato */
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato C-Win
ON TAB OF fiFraDato IN FRAME DEFAULT-FRAME /* Fra/til dato */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiOvButNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOvButNr C-Win
ON RETURN OF fiOvButNr IN FRAME DEFAULT-FRAME /* Ov.butikk */
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiOvButNr C-Win
ON TAB OF fiOvButNr IN FRAME DEFAULT-FRAME /* Ov.butikk */
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStorl C-Win
ON RETURN OF fiStorl IN FRAME DEFAULT-FRAME /* Str. */
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStorl C-Win
ON TAB OF fiStorl IN FRAME DEFAULT-FRAME /* Str. */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDato C-Win
ON RETURN OF fiTilDato IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiTilDato C-Win
ON TAB OF fiTilDato IN FRAME DEFAULT-FRAME
DO:
  RUN setFilter.    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwTransLogg
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
{incl/conttrigg.i oBrwTransLogg:BROWSE-HANDLE}
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
  DISPLAY cbTransType fiButNr fiOvButNr fiStorl fiFraDato fiTilDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbTransLogg cbTransType btBlank first_tbTransLogg prev_tbTransLogg 
         next_tbTransLogg last_tbTransLogg filter_tbTransLogg 
         refresh_tbTransLogg excel_tbTransLogg fiButNr fiOvButNr fiStorl 
         fiFraDato fiTilDato BrwTransLogg 
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

  oBrwTransLogg = NEW JBoxBrowse(brwTransLogg:HANDLE).

  otbTransLogg = NEW JBoxToolbar(tbTransLogg:HANDLE).

  oBrwTransLogg:TOOLBAR-OBJECT = otbTransLogg.
  oBrwTransLogg:setQuerySort("ArtikkelNr,Dato,Tid,butik,TTId").
  
  cTekst = '<Alle>,0,' + REPLACE(JBoxServerAPI:Instance:FieldList("Transtype;TTId|Beskrivelse;TTId", "WHERE Aktiv = TRUE"),'|',',').
  cbTransType:LIST-ITEM-PAIRS = cTekst.
  ASSIGN 
    cbTranstype = 0
    cbTranstype:SCREEN-VALUE = '0'
    .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilter C-Win 
PROCEDURE setFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
   DEF TEMP-TABLE TransLogg
    FIELD Butik AS INTEGER
    FIELD OvButik AS INTEGER
    FIELD translogg_TransType AS CHARACTER
    FIELD Dato AS DATE
    FIELD Storl AS CHARACTER
   
------------------------------------------------------------------------------*/

  DEFINE VARIABLE pcFieldLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcOperator AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcWhere    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pctekst    AS CHARACTER NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      pcFieldLst  = ''
      pcWhere     = ''
      pcTekst     = ''
      pcOperator  = ''
      .
    DO:
      IF cbTransType:SCREEN-VALUE <> '0' THEN 
        ASSIGN   
          pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'TTId'
          pcOperator  = pcOperator  + (IF pcOperator  = '' THEN '' ELSE ',') + '='
          pcWhere = pcWhere + (IF pcWhere = '' THEN '' ELSE '|') + cbTransType:SCREEN-VALUE
          .
  
      IF INT(fiButNr:SCREEN-VALUE) > 0 THEN 
        ASSIGN 
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Butik'
        pcOperator  = pcOperator  + (IF pcOperator  = '' THEN '' ELSE ',') + '='
        pcWhere = pcWhere + (IF pcWhere = '' THEN '' ELSE '|') + fiButNr:SCREEN-VALUE
        .
        
      IF INT(fiOvButNr:SCREEN-VALUE) > 0 THEN 
        ASSIGN 
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'OvButik'
        pcOperator  = pcOperator  + (IF pcOperator  = '' THEN '' ELSE ',') + '='
        pcWhere = pcWhere + (IF pcWhere = '' THEN '' ELSE '|') + fiOvButNr:SCREEN-VALUE
        .
  
      IF fiStorl:SCREEN-VALUE <> '' THEN 
      DO:
        pcTekst = rclStandardFunksjoner:FixStorl(TRIM(fiStorl:SCREEN-VALUE)).
        ASSIGN   
          pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Storl'
          pcOperator  = pcOperator  + (IF pcOperator  = '' THEN '' ELSE ',') + '='
          pcWhere = pcWhere + (IF pcWhere = '' THEN '' ELSE '|') + pcTekst
          .
      END.
  
      IF DATE(fiFraDato:SCREEN-VALUE) <> ? THEN 
        ASSIGN 
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Dato'
        pcOperator  = pcOperator  + (IF pcOperator  = '' THEN '' ELSE ',') + '>='
        pcWhere = pcWhere + (IF pcWhere = '' THEN '' ELSE '|') + fiFraDato:SCREEN-VALUE
        .
  
      IF DATE(fiTilDato:SCREEN-VALUE) <> ? THEN 
        ASSIGN 
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Dato'
        pcOperator  = pcOperator  + (IF pcOperator  = '' THEN '' ELSE ',') + '=<'
        pcWhere = pcWhere + (IF pcWhere = '' THEN '' ELSE '|') + fiTilDato:SCREEN-VALUE
        .
    END.
    oBrwTransLogg:setFilter(pcFieldLst,pcOperator,pcWhere).
  END.
  
  oBrwTransLogg:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

