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
&SCOPED-DEFINE AdvGuiWin 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE iKasseNr AS INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE pcFieldLst  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cWhere      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cOperator   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE pcTekst     AS CHARACTER     NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE hOmsetningsDatoColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hBokforingsbilag_BokforingsNrColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hBokforingsbilag_KasseDiffColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE iCurrTab AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
  
DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

DEFINE TEMP-TABLE ttBokforingsbilag NO-UNDO
  FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  .



/*** Start instance property definitions for JBoxBrowse object oBrwBokforingsBilag ***/
DEF VAR oBrwBokforingsBilag AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE BokforingsBilag
    FIELD ButikkNr AS INTEGER
    FIELD OmsetningsDato AS DATE
    FIELD bokforingsbilag_Kassediff AS DECIMAL
    FIELD bokforingsbilag_BruttoOmsetning AS DECIMAL
    FIELD BokforingsNr AS INTEGER
    FIELD EODDato AS DATE
    FIELD EODMottatt AS LOGICAL
    FIELD EODDatoTidMottatt AS DATETIME
    FIELD GodkjentFlagg AS LOGICAL
    FIELD bokforingsbilag_GodkjentDatoTid AS CHARACTER
    FIELD SendtRegnskap AS LOGICAL
    FIELD bokforingsbilag_SendtDatoTid AS CHARACTER
    FIELD SendAv AS CHARACTER
    FIELD bokforingsbilag_RegistrertTid AS CHARACTER
    FIELD RegistrertAv AS CHARACTER
    FIELD bokforingsbilag_EndretDatoTid AS CHARACTER
    FIELD BrukerID AS CHARACTER
    FIELD GodkjentDato AS DATE
    FIELD GodkjentTid AS INTEGER
    FIELD GodkjentAv AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD SendtDato AS DATE
    FIELD SendtTid AS INTEGER
    FIELD EDato AS DATE
    FIELD ETid AS INTEGER
    FIELD BokforingsID AS DECIMAL
    FIELD Aar AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .

FUNCTION getBuffersAndFieldsBrwBokforingsBilag RETURNS CHARACTER():
  RETURN
    'BokforingsBilag'
     + ';ButikkNr'
     + ';OmsetningsDato'
     + ';BokforingsNr'
     + ';EODDato'
     + ';EODMottatt'
     + ';EODDatoTidMottatt'
     + ';GodkjentFlagg'
     + ';SendtRegnskap'
     + ';SendAv'
     + ';RegistrertAv'
     + ';BrukerID'
     + ';GodkjentDato'
     + ';GodkjentTid'
     + ';GodkjentAv'
     + ';RegistrertDato'
     + ';RegistrertTid'
     + ';SendtDato'
     + ';SendtTid'
     + ';EDato'
     + ';ETid'
     + ';BokforingsID'
     + ';Aar'
     + ';+bokforingsbilag_Kassediff|DECIMAL||bokforingsbilag_Kassediff|Diff'
     + ';+bokforingsbilag_BruttoOmsetning|DECIMAL||bokforingsbilag_BruttoOmsetning|Br.Oms.'
     + ';+bokforingsbilag_GodkjentDatoTid|CHARACTER||bokforingsbilag_GodkjentDatoTid|Godkjent'
     + ';+bokforingsbilag_SendtDatoTid|CHARACTER||bokforingsbilag_SendtDatoTid|Sendt'
     + ';+bokforingsbilag_RegistrertTid|CHARACTER||bokforingsbilag_RegistrertTid|Registrer'
     + ';+bokforingsbilag_EndretDatoTid|CHARACTER||bokforingsbilag_EndretDatoTid|Endret'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwBokforingsBilag RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwBokforingsBilag RETURNS CHARACTER():
  RETURN 
     'bokforingsbilag_brwcalc.p' /* bokforingsbilag_Kassediff */
   + ',bokforingsbilag_brwcalc.p' /* bokforingsbilag_BruttoOmsetning */
   + ',bokforingsbilag_brwcalc.p' /* bokforingsbilag_GodkjentDatoTid */
   + ',bokforingsbilag_brwcalc.p' /* bokforingsbilag_SendtDatoTid */
   + ',bokforingsbilag_brwcalc.p' /* bokforingsbilag_RegistrertTid */
   + ',bokforingsbilag_brwcalc.p' /* bokforingsbilag_EndretDatoTid */
     .
END FUNCTION.

DEF VAR otbBokforingsBilag AS JBoxToolbar NO-UNDO.

DEF VAR oTabBokforingsBilag AS JBoxMsTabs NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwBokforingsBilag

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BokforingsBilag

/* Definitions for BROWSE BrwBokforingsBilag                            */
&Scoped-define FIELDS-IN-QUERY-BrwBokforingsBilag BokforingsBilag.ButikkNr ~
BokforingsBilag.OmsetningsDato BokforingsBilag.bokforingsbilag_Kassediff ~
BokforingsBilag.bokforingsbilag_BruttoOmsetning ~
BokforingsBilag.BokforingsNr BokforingsBilag.EODDato ~
BokforingsBilag.EODMottatt BokforingsBilag.EODDatoTidMottatt ~
BokforingsBilag.GodkjentFlagg ~
BokforingsBilag.bokforingsbilag_GodkjentDatoTid ~
BokforingsBilag.SendtRegnskap BokforingsBilag.bokforingsbilag_SendtDatoTid ~
BokforingsBilag.SendAv BokforingsBilag.bokforingsbilag_RegistrertTid ~
BokforingsBilag.RegistrertAv BokforingsBilag.bokforingsbilag_EndretDatoTid ~
BokforingsBilag.BrukerID BokforingsBilag.GodkjentDato ~
BokforingsBilag.GodkjentTid BokforingsBilag.GodkjentAv ~
BokforingsBilag.RegistrertDato BokforingsBilag.RegistrertTid ~
BokforingsBilag.SendtDato BokforingsBilag.SendtTid BokforingsBilag.EDato ~
BokforingsBilag.ETid BokforingsBilag.BokforingsID BokforingsBilag.Aar 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwBokforingsBilag ~
BokforingsBilag.ButikkNr BokforingsBilag.BokforingsID 
&Scoped-define QUERY-STRING-BrwBokforingsBilag FOR EACH BokforingsBilag NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwBokforingsBilag OPEN QUERY BrwBokforingsBilag FOR EACH BokforingsBilag NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwBokforingsBilag BokforingsBilag
&Scoped-define FIRST-TABLE-IN-QUERY-BrwBokforingsBilag BokforingsBilag


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbBokforingsBilag recFilter ~
TabBokforingsBilag first_tbBokforingsBilag prev_tbBokforingsBilag ~
next_tbBokforingsBilag last_tbBokforingsBilag refresh_tbBokforingsBilag ~
filter_tbBokforingsBilag excel_tbBokforingsBilag print_tbBokforingsBilag ~
EkspOppgjor_tbBokforingsBilag cbButikk fiFraDato tgVisAlle tgVisTomme ~
tgDiff BrwBokforingsBilag 
&Scoped-Define DISPLAYED-OBJECTS cbButikk fiFraDato tgVisAlle tgVisTomme ~
tgDiff 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON EkspOppgjor_tbBokforingsBilag 
     LABEL "Eksporter oppgjør" 
     SIZE 18 BY 1.1.

DEFINE BUTTON excel_tbBokforingsBilag 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbBokforingsBilag 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbBokforingsBilag 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbBokforingsBilag 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbBokforingsBilag 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbBokforingsBilag 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON print_tbBokforingsBilag 
     IMAGE-UP FILE "bmp/print16e.bmp":U
     LABEL "Print" 
     SIZE 4.6 BY 1.1 TOOLTIP "Print (ALT-P)".

DEFINE BUTTON refresh_tbBokforingsBilag 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbButikk AS INT64 FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "<Alle>",0,
                     "15",15
     DROP-DOWN-LIST
     SIZE 45 BY 1 TOOLTIP "Filter på butikknr." NO-UNDO.

DEFINE VARIABLE fiFraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1 TOOLTIP "Viser oppgjør fra og med dato" NO-UNDO.

DEFINE RECTANGLE recFilter
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 160.6 BY 1.52.

DEFINE RECTANGLE TabBokforingsBilag
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 126.2 BY 16.14.

DEFINE RECTANGLE tbBokforingsBilag
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 162 BY 1.29.

DEFINE VARIABLE tgDiff AS LOGICAL INITIAL NO 
     LABEL "Bare med diff" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Vise bare oppgjør med diff." NO-UNDO.

DEFINE VARIABLE tgVisAlle AS LOGICAL INITIAL NO 
     LABEL "Vis alle (Også godkjente og sendt regnskap)" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 TOOLTIP "Viser også bokføringsbilag som er sendt regnskap." NO-UNDO.

DEFINE VARIABLE tgVisTomme AS LOGICAL INITIAL NO 
     LABEL "Vis tomme" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 TOOLTIP "Vis også oppgjør med 0 i omsetning" NO-UNDO.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 11.43
     BGCOLOR 12 FGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwBokforingsBilag FOR 
      BokforingsBilag SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwBokforingsBilag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwBokforingsBilag C-Win _STRUCTURED
  QUERY BrwBokforingsBilag NO-LOCK DISPLAY
      BokforingsBilag.ButikkNr COLUMN-LABEL "Butnr" FORMAT ">>>>>9":U
            WIDTH 5
      BokforingsBilag.OmsetningsDato COLUMN-LABEL "ODato" FORMAT "99/99/99":U
      BokforingsBilag.bokforingsbilag_Kassediff COLUMN-LABEL "Diff" FORMAT "->,>>>,>>9.99":U
            WIDTH 9
      BokforingsBilag.bokforingsbilag_BruttoOmsetning COLUMN-LABEL "Br.Oms." FORMAT "->,>>>,>>9":U
      BokforingsBilag.BokforingsNr COLUMN-LABEL "Bokf.nr" FORMAT ">>>>>9":U
            WIDTH 9
      BokforingsBilag.EODDato COLUMN-LABEL "EODDato" FORMAT "99/99/99":U
      BokforingsBilag.EODMottatt COLUMN-LABEL "EOD" FORMAT "*/":U
            WIDTH 5
      BokforingsBilag.EODDatoTidMottatt COLUMN-LABEL "Dato/tid mottatt EOD" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 23
      BokforingsBilag.GodkjentFlagg COLUMN-LABEL "Godkj." FORMAT "*/":U
            WIDTH 8.6
      BokforingsBilag.bokforingsbilag_GodkjentDatoTid COLUMN-LABEL "Godkjent" FORMAT "X(21)":U
      BokforingsBilag.SendtRegnskap COLUMN-LABEL "Sendt" FORMAT "*/":U
      BokforingsBilag.bokforingsbilag_SendtDatoTid COLUMN-LABEL "Sendt" FORMAT "X(21)":U
      BokforingsBilag.SendAv COLUMN-LABEL "BrukerId" FORMAT "X(15)":U
      BokforingsBilag.bokforingsbilag_RegistrertTid COLUMN-LABEL "Registrer" FORMAT "X(21)":U
      BokforingsBilag.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      BokforingsBilag.bokforingsbilag_EndretDatoTid COLUMN-LABEL "Endret" FORMAT "X(21)":U
      BokforingsBilag.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      BokforingsBilag.GodkjentDato COLUMN-LABEL "Godkjent" FORMAT "99/99/99":U
      BokforingsBilag.GodkjentTid COLUMN-LABEL "_GodkjentTid" FORMAT "->,>>>,>>9":U
      BokforingsBilag.GodkjentAv COLUMN-LABEL "Bruker" FORMAT "X(15)":U
            WIDTH 9
      BokforingsBilag.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      BokforingsBilag.RegistrertTid COLUMN-LABEL "Registreringstidspunkt" FORMAT "->,>>>,>>9":U
      BokforingsBilag.SendtDato COLUMN-LABEL "Sendt" FORMAT "99/99/99":U
      BokforingsBilag.SendtTid COLUMN-LABEL "Tid" FORMAT "->,>>>,>>9":U
      BokforingsBilag.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      BokforingsBilag.ETid COLUMN-LABEL "ETid" FORMAT "->,>>>,>>9":U
      BokforingsBilag.BokforingsID COLUMN-LABEL "Bokføringsbilagets unike ID" FORMAT ">>>>>>>>>>>>>>>9":U
      BokforingsBilag.Aar COLUMN-LABEL "År" FORMAT ">>>9":U
  ENABLE
      BokforingsBilag.ButikkNr HELP "Butikknummer"
      BokforingsBilag.BokforingsID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 33 BY 16.19 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbBokforingsBilag AT ROW 1.33 COL 2.4 WIDGET-ID 4
     prev_tbBokforingsBilag AT ROW 1.33 COL 7.2 WIDGET-ID 6
     next_tbBokforingsBilag AT ROW 1.33 COL 11.8 WIDGET-ID 8
     last_tbBokforingsBilag AT ROW 1.33 COL 16.4 WIDGET-ID 10
     refresh_tbBokforingsBilag AT ROW 1.33 COL 21 WIDGET-ID 12
     filter_tbBokforingsBilag AT ROW 1.33 COL 25.6 WIDGET-ID 14
     excel_tbBokforingsBilag AT ROW 1.33 COL 30.2 WIDGET-ID 16
     print_tbBokforingsBilag AT ROW 1.33 COL 34.8 WIDGET-ID 18
     EkspOppgjor_tbBokforingsBilag AT ROW 1.33 COL 39.4 WIDGET-ID 36
     cbButikk AT ROW 3.29 COL 8.2 COLON-ALIGNED WIDGET-ID 22
     fiFraDato AT ROW 3.29 COL 144 COLON-ALIGNED
     tgVisAlle AT ROW 3.38 COL 55.8 WIDGET-ID 32
     tgVisTomme AT ROW 3.38 COL 102.8 WIDGET-ID 34
     tgDiff AT ROW 3.38 COL 118.6 WIDGET-ID 38
     BrwBokforingsBilag AT ROW 4.81 COL 2 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.48 COL 3.6 WIDGET-ID 28
          FONT 6
     tbBokforingsBilag AT ROW 1.24 COL 1 WIDGET-ID 2
     recFilter AT ROW 3.05 COL 2.4 WIDGET-ID 26
     TabBokforingsBilag AT ROW 4.86 COL 36.4 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 162.2 BY 20.19 WIDGET-ID 100.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1.48 COL 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 10.4 ROW 6.71
         SIZE 47 BY 12.62 WIDGET-ID 300.


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
         TITLE              = "Dagsoppgjør"
         HEIGHT             = 20.05
         WIDTH              = 162.6
         MAX-HEIGHT         = 24.1
         MAX-WIDTH          = 179.8
         VIRTUAL-HEIGHT     = 24.1
         VIRTUAL-WIDTH      = 179.8
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
/* BROWSE-TAB BrwBokforingsBilag tgDiff DEFAULT-FRAME */
ASSIGN 
       BokforingsBilag.GodkjentDato:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.GodkjentTid:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.RegistrertDato:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.RegistrertTid:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.SendtDato:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.SendtTid:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.EDato:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE
       BokforingsBilag.ETid:VISIBLE IN BROWSE BrwBokforingsBilag = FALSE.

ASSIGN 
       tbBokforingsBilag:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,print;Print,EkspOppgjor;Eksporter oppgjørmaxborder".

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwBokforingsBilag
/* Query rebuild information for BROWSE BrwBokforingsBilag
     _TblList          = "SkoTex.BokforingsBilag"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"BokforingsBilag.ButikkNr" "Butnr" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikknummer" no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"BokforingsBilag.OmsetningsDato" "ODato" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato inneholder omsetningstall for." no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"BokforingsBilag.bokforingsbilag_Kassediff" "Diff" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"BokforingsBilag.bokforingsbilag_BruttoOmsetning" "Br.Oms." "->,>>>,>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"BokforingsBilag.BokforingsNr" "Bokf.nr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Bokføringsnummer" no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"BokforingsBilag.EODDato" "EODDato" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato da EOD er mottatt" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"BokforingsBilag.EODMottatt" "EOD" "*~~/" "LOGICAL" ? ? ? ? ? ? no "EOD mottatt" no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"BokforingsBilag.EODDatoTidMottatt" "Dato/tid mottatt EOD" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "Dato og tidspunkt da bilaget ble stemplet med EOD." no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"BokforingsBilag.GodkjentFlagg" "Godkj." "*~~/" "LOGICAL" ? ? ? ? ? ? no "Godkjent" no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"BokforingsBilag.bokforingsbilag_GodkjentDatoTid" "Godkjent" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"BokforingsBilag.SendtRegnskap" "Sendt" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Sendt til regnskap" no no "5.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"BokforingsBilag.bokforingsbilag_SendtDatoTid" "Sendt" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"BokforingsBilag.SendAv" "BrukerId" "X(15)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"BokforingsBilag.bokforingsbilag_RegistrertTid" "Registrer" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"BokforingsBilag.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"BokforingsBilag.bokforingsbilag_EndretDatoTid" "Endret" "X(21)" "CHARACTER" ? ? ? ? ? ? no "" no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"BokforingsBilag.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"BokforingsBilag.GodkjentDato" "Godkjent" "99/99/99" "DATE" ? ? ? ? ? ? no "Godkjent dato" no no "9.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"BokforingsBilag.GodkjentTid" "_GodkjentTid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Godkjent tid" no no "12.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"BokforingsBilag.GodkjentAv" "Bruker" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Godkjent av" no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"BokforingsBilag.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"BokforingsBilag.RegistrertTid" "Registreringstidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt for registrering av posten" no no "20.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"BokforingsBilag.SendtDato" "Sendt" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato sendt til regnskapssystem" no no "9.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"BokforingsBilag.SendtTid" "Tid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt den ble sendt." no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"BokforingsBilag.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"BokforingsBilag.ETid" "ETid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Endret tidspunkt" no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"BokforingsBilag.BokforingsID" "Bokføringsbilagets unike ID" ">>>>>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? yes "" no no "25.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"BokforingsBilag.Aar" "År" ">>>9" "INTEGER" ? ? ? ? ? ? no "År" no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwBokforingsBilag */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frSplitBarX
/* Query rebuild information for FRAME frSplitBarX
     _Query            is NOT OPENED
*/  /* FRAME frSplitBarX */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dagsoppgjør */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dagsoppgjør */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
&Scoped-define SELF-NAME cbButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbButikk C-Win
ON VALUE-CHANGED OF cbButikk IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato C-Win
ON RETURN OF fiFraDato IN FRAME DEFAULT-FRAME /* Fra dato */
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato C-Win
ON TAB OF fiFraDato IN FRAME DEFAULT-FRAME /* Fra dato */
DO:
  RUN setFilter.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgDiff
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgDiff C-Win
ON VALUE-CHANGED OF tgDiff IN FRAME DEFAULT-FRAME /* Bare med diff */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgVisAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgVisAlle C-Win
ON VALUE-CHANGED OF tgVisAlle IN FRAME DEFAULT-FRAME /* Vis alle (Også godkjente og sendt regnskap) */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgVisTomme
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgVisTomme C-Win
ON VALUE-CHANGED OF tgVisTomme IN FRAME DEFAULT-FRAME /* Vis tomme */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwBokforingsBilag
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
{incl/conttrigg.i oBrwBokforingsBilag:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE closeRecord C-Win 
PROCEDURE closeRecord :
RUN SUPER.


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
IF oBrwBokforingsBilag:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  IF oBrwBokforingsBilag:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
    IF oTabBokforingsBilag:CurrTabNum = 2 AND AVAILABLE Bokforingsbilag THEN
      RUN 'VisBokfBilagRecord' IN oTabBokforingsBilag:getPageHandle(2). 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EkspOppgjorRecord C-Win 
PROCEDURE EkspOppgjorRecord :
DEFINE VARIABLE piSvar AS INTEGER NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:
  
  IF AVAILABLE Bokforingsbilag AND BokforingsBilag.GodkjentFlagg = TRUE THEN 
  DO:
    piSvar = JBoxSession:Instance:ViewQuestionYesNoCancel("Eksportere oppgjør til NavisionNei)?"). 
    IF piSvar = 1 THEN
    EKSPORT: 
    DO:
      JBoxServerAPI:Instance:CallServerProc("EksportEtDagsoppgjorNavision.p",STRING(Bokforingsbilag.BokforingsId) + '|' + JBoxSession:Instance:UserId,hBuffer).
    END. /* EKSPORT */
  END.
  ELSE JBoxSession:Instance:ViewMessage("Bokføringsbilaget må være godkjent før det kan eksporteres.").

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
  DISPLAY cbButikk fiFraDato tgVisAlle tgVisTomme tgDiff 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbBokforingsBilag recFilter TabBokforingsBilag first_tbBokforingsBilag 
         prev_tbBokforingsBilag next_tbBokforingsBilag last_tbBokforingsBilag 
         refresh_tbBokforingsBilag filter_tbBokforingsBilag 
         excel_tbBokforingsBilag print_tbBokforingsBilag 
         EkspOppgjor_tbBokforingsBilag cbButikk fiFraDato tgVisAlle tgVisTomme 
         tgDiff BrwBokforingsBilag 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE btnSplitBarX 
      WITH FRAME frSplitBarX IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frSplitBarX}
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
  rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.
  
  ASSIGN
    iButNr   = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr")) 
    iKasseNr = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"kassenr")) 
    .
  oBrwBokforingsBilag = NEW JBoxBrowse(brwBokforingsBilag:HANDLE).
  
  dDato = ?.
  IF JBoxServerAPI:Instance:Find("Syspara", "WHERE SysHId = 210 and SysGr = 260 and ParaNr = 10") THEN
    ASSIGN
      dDato = DATE(JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1")) NO-ERROR.
      . 
/*  IF iKasseNr > 0 THEN*/
/*  DO:                                                                            */
/*    IF dDato <> ? THEN                                                           */
/*      oBrwBokforingsBilag:baseQuery = "WHERE Omsetningsdato >= " + STRING(dDato).*/
/*    ELSE                                                                         */
/*      oBrwBokforingsBilag:baseQuery = "WHERE Omsetningsdato >= 01/04/2020".      */
/*  END.                                                                           */
  hBokforingsbilag_BokforingsNrColumn = oBrwBokforingsBilag:getColumnHandle("Bokforingsnr").
  hOmsetningsdatoColumn = oBrwBokforingsBilag:getColumnHandle("Omsetningsdato").
  hBokforingsbilag_KasseDiffColumn = oBrwBokforingsBilag:getColumnHandle("Bokforingsbilag_KasseDiff").

  otbBokforingsBilag = NEW JBoxToolbar(tbBokforingsBilag:HANDLE).

  oBrwBokforingsBilag:TOOLBAR-OBJECT = otbBokforingsBilag.
/*  oBrwBokforingsBilag:baseQuery = "WHERE ButikkNr = '" + STRING(iButNr) + "'".*/
  oBrwBokforingsBilag:setQuerySort("ButikkNr,Aar;DESC,BokforingsNr;DESC").
  
  oContainer:setNoResizeY("recFilter").
   
  ASSIGN 
    cbButikk:DELIMITER = "|"
    .
  
/*  cbButikk:LIST-ITEM-PAIRS = '<Alle>|0|' + DYNAMIC-FUNCTION("getFieldList",                                                                                                */
/*                                                            "Butiker;Butik|ButNamn;Butik",                                                                                 */
/*                                                            "WHERE Butiker.HarButikkSystem = 'TRUE' AND Butiker.Butik < 800 AND (Butiker.ApningsDato <= " + STRING(TODAY) +*/
/*                                                            ") AND (Butiker.NedlagtDato = '?' OR Butiker.NedlagtDato >= '" + STRING(TODAY) + "') "                         */
/*                                                            ).                                                                                                             */
  cbButikk:LIST-ITEM-PAIRS = '<Alle>|0|' + DYNAMIC-FUNCTION("getFieldList",
                                                            "Butiker;Butik|ButNamn;Butik",
                                                            "WHERE Butiker.HarButikkSystem = 'TRUE' AND (Butiker.ApningsDato <= " + STRING(TODAY) + 
                                                            ") AND (Butiker.NedlagtDato = '?' OR Butiker.NedlagtDato >= '" + STRING(TODAY) + "') "
                                                            ).    
  ASSIGN 
    cbButikk = iButNr
    cbbutikk:SCREEN-VALUE = STRING(iButNr)
    cbButikk:SENSITIVE = IF iKasseNr = 0 THEN TRUE ELSE FALSE 
    fiFraDato:SCREEN-VALUE = STRING(dDato)
    .

  oTabBokforingsBilag = NEW JBoxMsTabs(TabBokforingsBilag:HANDLE,oBrwBokforingsBilag).
  oTabBokforingsBilag:pageOneType = "oneToOne".

  oTabBokforingsBilag:setLinkFields("BokforingsId").
  oTabBokforingsBilag:AddPage("Godkjenning","DagsoppgjorGodk.w").
  oTabBokforingsBilag:AddPage("Korreksjon","DagsoppgjorKorr.w").  
  
  oContainer:setSplitBarX(btnSplitBarX:HANDLE IN FRAME frSplitBarX).
  oContainer:setFollowSplitBarX(STRING(BrwBokforingsbilag:HANDLE) + ',' + STRING(TabBokforingsBilag:HANDLE)).
  oContainer:setNoResizeX("BrwBokforingsbilag").
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostTabChanged C-Win 
PROCEDURE PostTabChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piCurrTabNum AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
   IF piCurrTabNum = 2 AND AVAILABLE Bokforingsbilag THEN  
     /* Viktig at man her bruker metoden på objektet, og ikke kjører display record direkte. */
     /* Når metoden benyttes, vet den hvor displayrecord skal kjøres.                        */           
     oBrwBokforingsBilag:displayRecord().

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PreTabChanged C-Win 
PROCEDURE PreTabChanged :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER piCurrTabNum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.

  pbOk = TRUE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  RUN visDagsoppgjor.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  IF AVAILABLE Bokforingsbilag THEN 
  DO:
    IF Bokforingsbilag.Bokforingsbilag_KasseDiff <> 0 THEN
      hBokforingsbilag_KasseDiffColumn:BGCOLOR = 12.
    
    IF Bokforingsbilag.EODMottatt = FALSE THEN
      hOmsetningsdatoColumn:BGCOLOR = 14.
    ELSE IF Bokforingsbilag.EODMottatt = TRUE AND Bokforingsbilag.GodkjentFlagg = FALSE THEN
      hOmsetningsdatoColumn:BGCOLOR = 12.
    ELSE IF Bokforingsbilag.EODMottatt = TRUE AND Bokforingsbilag.GodkjentFlagg = TRUE AND Bokforingsbilag.SendtRegnskap = FALSE THEN
      hOmsetningsdatoColumn:BGCOLOR = 10.
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
  
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN
      pcFieldLst = ''
      cWhere     = ''
      pcTekst    = ''
      cOperator  = ''
      .
      
    IF cbButikk:SCREEN-VALUE <> '0' THEN 
      ASSIGN   
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'ButikkNr'
        cOperator  = cOperator  + (IF cOperator <> '' THEN ',' ELSE '') + '='
        cWhere     = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + cbButikk:SCREEN-VALUE
        .
    IF tgVisAlle:SCREEN-VALUE = 'no' THEN 
      ASSIGN   
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'SendtRegnskap'
        cOperator  = cOperator  + (IF cOperator <> '' THEN ',' ELSE '') + '='
        cWhere     = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + tgVisAlle:SCREEN-VALUE
        .

    IF tgVisTomme:SCREEN-VALUE = 'no' THEN
      ASSIGN
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'bokforingsbilag_BruttoOmsetning'
        cOperator  = cOperator  + (IF cOperator <> '' THEN ',' ELSE '') + '>'
        cWhere     = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '0'
        .

    IF tgDiff:SCREEN-VALUE = 'yes' THEN 
      ASSIGN   
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'bokforingsbilag_Kassediff'
        cOperator  = cOperator  + (IF cOperator <> '' THEN ',' ELSE '') + '<>'
        cWhere     = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '0'
        .
    IF fiFraDato:SCREEN-VALUE <> ? THEN 
      ASSIGN   
        pcFieldLst = pcFieldLst + (IF pcFieldLst = '' THEN '' ELSE ',') + 'Omsetningsdato'
        cOperator  = cOperator  + (IF cOperator <> '' THEN ',' ELSE '') + '>='
        cWhere     = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiFraDato:SCREEN-VALUE 
        .


/*MESSAGE                      */
/*'pcFieldLst:' pcFieldLst SKIP*/
/*'cOperator:' cOperator SKIP  */
/*'cWhere:' cWhere             */
/*VIEW-AS ALERT-BOX.           */

    oBrwBokforingsBilag:setFilter(pcFieldLst,cOperator,cWhere).
          
  END.
  oBrwBokforingsBilag:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visDagsoppgjor C-Win 
PROCEDURE visDagsoppgjor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piSvar AS INTEGER NO-UNDO.

  hBuffer = BUFFER ttBokforingsbilag:HANDLE.
  
  IF AVAILABLE Bokforingsbilag THEN 
  DO:
    piSvar =  JBoxSession:Instance:ViewQuestionYesNoCancel("Utskrift til skriver(Ja) eller skjerm(Nei)?"). 
    
    IF piSvar = 1 THEN
    TILSKRIVER: 
    DO:
      CREATE ttBokforingsbilag. /* En record å sende med. */
      IF JBoxServerAPI:Instance:CallServerProc("Bokforingsbilag_getBlob.p",STRING(Bokforingsbilag.BokforingsId) + '|' + JBoxSession:Instance:UserId + '|SKRIVER',hBuffer) THEN
         JBoxSession:Instance:ViewMessage("Bokføringsbilaget er sendt til skriver.").
      DELETE ttBokforingsbilag.    
    END. /* TILSKRIVER */
    
    IF piSvar = 2 THEN
    TILSKJERM: 
    DO:
      CREATE ttBokforingsbilag. /* En record å sende med. */
      IF JBoxServerAPI:Instance:CallServerProc("Bokforingsbilag_getBlob.p",STRING(Bokforingsbilag.BokforingsId) + '|' + JBoxSession:Instance:UserId,hBuffer) THEN
      DO: 
        hBuffer = JBoxServerAPI:Instance:getCallReturnTable().
        pcFilNavn = REPLACE(rclStandardFunksjoner:getTmpFileName (),'.tmp','.pdf').
  
        COPY-LOB FROM ttBokforingsbilag.PdfFil TO FILE pcFilNavn NO-ERROR. 
        IF SEARCH(pcFilNavn) <> ? THEN  
          OS-COMMAND SILENT VALUE('foxitreader.exe ' + pcFilNavn).
      END.
      ELSE 
        JBoxSession:Instance:ViewMessage("Fant ikke PDF.").
    END. /* TILSKJERM */
        
    IF piSvar = 3 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Utskrift avbrutt.").
    END.    
        
  END.
  
  EMPTY TEMP-TABLE ttBokforingsbilag.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

