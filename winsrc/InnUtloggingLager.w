&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
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

DEFINE VARIABLE bOk         AS LOG    NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBrowse     AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
DEFINE VARIABLE hToolbar    AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE hLoggColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE oBrwFillInLogg AS HANDLE NO-UNDO.
DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOperator AS CHARACTER NO-UNDO.

DEFINE VARIABLE cBaseFields   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBaseOperator AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFields       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBaseWhere    AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwBongHode ***/
DEFINE VARIABLE oBrwBongHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE BongHode
    FIELD ButikkNr AS INTEGER
    FIELD bongHode_IntTTId AS INTEGER
    FIELD bongHode_TTId AS CHARACTER
    FIELD bongHode_DatoTid AS CHARACTER
    FIELD KasseNr AS INTEGER
    FIELD BongNr AS INTEGER
    FIELD bongHode_SelgerId AS INTEGER
    FIELD SelgerNr AS INTEGER
    FIELD bongHode_AnsattNr AS CHARACTER
    FIELD bongHode_AnsNavn AS CHARACTER
    FIELD Logg AS CHARACTER
    FIELD bongHode_EDatoTid AS CHARACTER
    FIELD bongHode_eAv AS CHARACTER
    FIELD GruppeNr AS INTEGER
    FIELD Dato AS DATE
    FIELD Tid AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_BongHode FOR TEMP-TABLE BongHode.


FUNCTION getBuffersAndFieldsBrwBongHode RETURNS CHARACTER():
  RETURN
    'BongHode'
     + ';ButikkNr'
     + ';KasseNr'
     + ';BongNr'
     + ';SelgerNr'
     + ';Logg'
     + ';GruppeNr'
     + ';Dato'
     + ';Tid'
     + ';+bongHode_IntTTId|INTEGER||bongHode_IntTTId|TTId'
     + ';+bongHode_TTId|CHARACTER||bongHode_TTId|Type'
     + ';+bongHode_DatoTid|CHARACTER||bongHode_DatoTid|Dato/Tid'
     + ';+bongHode_SelgerId|INTEGER||bongHode_SelgerId|SelgerId'
     + ';+bongHode_AnsattNr|CHARACTER||bongHode_AnsattNr|AnsattNr'
     + ';+bongHode_AnsNavn|CHARACTER||bongHode_AnsNavn|Navn'
     + ';+bongHode_EDatoTid|CHARACTER||bongHode_EDatoTid|Endret dato/tid'
     + ';+bongHode_eAv|CHARACTER||bongHode_eAv|Endret av'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwBongHode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwBongHode RETURNS CHARACTER():
  RETURN 
     'server/bonghode_brwcalc.p' /* bongHode_IntTTId */
   + ',server/bonghode_brwcalc.p' /* bongHode_TTId */
   + ',server/bonghode_brwcalc.p' /* bongHode_DatoTid */
   + ',server/bonghode_brwcalc.p' /* bongHode_SelgerId */
   + ',server/bonghode_brwcalc.p' /* bongHode_AnsattNr */
   + ',server/bonghode_brwcalc.p' /* bongHode_AnsNavn */
   + ',server/bonghode_brwcalc.p' /* bongHode_EDatoTid */
   + ',server/bonghode_brwcalc.p' /* bongHode_eAv */
     .
END FUNCTION.


DEFINE VARIABLE otbBongHode AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwBongHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BongHode

/* Definitions for BROWSE BrwBongHode                                   */
&Scoped-define FIELDS-IN-QUERY-BrwBongHode BongHode.ButikkNr ~
BongHode.bongHode_IntTTId BongHode.bongHode_TTId BongHode.bongHode_DatoTid ~
BongHode.KasseNr BongHode.BongNr BongHode.bongHode_SelgerId ~
BongHode.SelgerNr BongHode.bongHode_AnsattNr BongHode.bongHode_AnsNavn ~
BongHode.Logg BongHode.bongHode_EDatoTid BongHode.bongHode_eAv ~
BongHode.GruppeNr BongHode.Dato BongHode.Tid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwBongHode BongHode.ButikkNr ~
BongHode.BongNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwBongHode BongHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwBongHode BongHode
&Scoped-define QUERY-STRING-BrwBongHode FOR EACH BongHode NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwBongHode OPEN QUERY BrwBongHode FOR EACH BongHode NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwBongHode BongHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwBongHode BongHode


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbBongHode RECT-4 first_tbBongHode ~
prev_tbBongHode next_tbBongHode last_tbBongHode delete_tbBongHode ~
refresh_tbBongHode filter_tbBongHode excel_tbBongHode btnBlank fiSelgerId ~
fiAnsattNr fiNavn fiMerknad BrwBongHode 
&Scoped-Define DISPLAYED-OBJECTS fiSelgerId fiAnsattNr fiNavn fiMerknad 

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
     SIZE 15 BY 1.14 TOOLTIP "Blanker filter.".

DEFINE BUTTON delete_tbBongHode 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbBongHode 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbBongHode 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbBongHode 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbBongHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbBongHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbBongHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbBongHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE fiAnsattNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ansattnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Filer på ansattnr. Begins..." NO-UNDO.

DEFINE VARIABLE fiMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 TOOLTIP "Filter på merknad. Matches..." NO-UNDO.

DEFINE VARIABLE fiNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 TOOLTIP "Filter på navn. Matches ..." NO-UNDO.

DEFINE VARIABLE fiSelgerId AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Selgerid" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 TOOLTIP "Selgerid som benyttes ved innlogging på kasse i butikk" NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 161 BY 1.43.

DEFINE RECTANGLE tbBongHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 181.2 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwBongHode FOR 
      BongHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwBongHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwBongHode C-Win _STRUCTURED
  QUERY BrwBongHode NO-LOCK DISPLAY
      BongHode.ButikkNr COLUMN-LABEL "ButNr" FORMAT ">>>>>9":U
      BongHode.bongHode_IntTTId COLUMN-LABEL "TTId" FORMAT ">>9":U
      BongHode.bongHode_TTId COLUMN-LABEL "Type" FORMAT "X(8)":U
            WIDTH 12
      BongHode.bongHode_DatoTid COLUMN-LABEL "Dato/Tid" FORMAT "X(20)":U
            WIDTH 22
      BongHode.KasseNr COLUMN-LABEL "KasseNr" FORMAT ">>9":U
      BongHode.BongNr COLUMN-LABEL "BongNr" FORMAT ">>>>>>>9":U
      BongHode.bongHode_SelgerId COLUMN-LABEL "SelgerId" FORMAT ">>>9":U
            WIDTH 8.2
      BongHode.SelgerNr COLUMN-LABEL "Selger" FORMAT ">>>>>9":U
      BongHode.bongHode_AnsattNr COLUMN-LABEL "AnsattNr" FORMAT "X(8)":U
      BongHode.bongHode_AnsNavn COLUMN-LABEL "Navn" FORMAT "X(40)":U
            WIDTH 31.6
      BongHode.Logg COLUMN-LABEL "Merknad" FORMAT "X(60)":U
      BongHode.bongHode_EDatoTid COLUMN-LABEL "Endret dato/tid" FORMAT "X(22)":U
      BongHode.bongHode_eAv COLUMN-LABEL "Endret av" FORMAT "X(15)":U
      BongHode.GruppeNr COLUMN-LABEL "GrNr" FORMAT ">9":U
      BongHode.Dato COLUMN-LABEL "Dato" FORMAT "99/99/99":U
      BongHode.Tid COLUMN-LABEL "Tid" FORMAT "->,>>>,>>9":U
  ENABLE
      BongHode.ButikkNr HELP "Butikknummer."
      BongHode.BongNr HELP "Bongens nummer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 182 BY 17.38 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbBongHode AT ROW 1.33 COL 2.2 WIDGET-ID 4
     prev_tbBongHode AT ROW 1.33 COL 7 WIDGET-ID 6
     next_tbBongHode AT ROW 1.33 COL 11.6 WIDGET-ID 8
     last_tbBongHode AT ROW 1.33 COL 16.2 WIDGET-ID 10
     delete_tbBongHode AT ROW 1.33 COL 20.8 WIDGET-ID 18
     refresh_tbBongHode AT ROW 1.33 COL 25.4 WIDGET-ID 12
     filter_tbBongHode AT ROW 1.33 COL 30 WIDGET-ID 14
     excel_tbBongHode AT ROW 1.33 COL 34.6 WIDGET-ID 16
     btnBlank AT ROW 3.14 COL 165 WIDGET-ID 24
     fiSelgerId AT ROW 3.24 COL 35.8 COLON-ALIGNED
     fiAnsattNr AT ROW 3.24 COL 56.2 COLON-ALIGNED
     fiNavn AT ROW 3.24 COL 77.8 COLON-ALIGNED
     fiMerknad AT ROW 3.24 COL 118.2 COLON-ALIGNED
     BrwBongHode AT ROW 4.57 COL 2 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.52 COL 23.2 WIDGET-ID 22
          FONT 6
     tbBongHode AT ROW 1.24 COL 2 WIDGET-ID 2
     RECT-4 AT ROW 3 COL 22 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 183.2 BY 21.1 WIDGET-ID 100.


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
         HEIGHT             = 21.1
         WIDTH              = 183.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 183.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 183.2
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
/* BROWSE-TAB BrwBongHode fiMerknad DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21.1
       FRAME DEFAULT-FRAME:WIDTH            = 183.2.

ASSIGN 
       BongHode.GruppeNr:VISIBLE IN BROWSE BrwBongHode = FALSE
       BongHode.Dato:VISIBLE IN BROWSE BrwBongHode = FALSE
       BongHode.Tid:VISIBLE IN BROWSE BrwBongHode = FALSE.

ASSIGN 
       tbBongHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,delete;Slett,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwBongHode
/* Query rebuild information for BROWSE BrwBongHode
     _TblList          = "Data.BongHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > BongHode.ButikkNr
"BongHode.ButikkNr" "ButNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikknummer." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"BongHode.bongHode_IntTTId" "TTId" ">>9" "INTEGER" ? ? ? ? ? ? no "" no no "4.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"BongHode.bongHode_TTId" "Type" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"BongHode.bongHode_DatoTid" "Dato/Tid" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > BongHode.KasseNr
"BongHode.KasseNr" "KasseNr" ">>9" "INTEGER" ? ? ? ? ? ? no "Kassenummer" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > BongHode.BongNr
"BongHode.BongNr" "BongNr" ">>>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Bongens nummer" no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"BongHode.bongHode_SelgerId" "SelgerId" ">>>9" "INTEGER" ? ? ? ? ? ? no "" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > BongHode.SelgerNr
"BongHode.SelgerNr" "Selger" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Selgerens nummer." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"BongHode.bongHode_AnsattNr" "AnsattNr" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"BongHode.bongHode_AnsNavn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "31.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > BongHode.Logg
"BongHode.Logg" "Merknad" "X(60)" "CHARACTER" ? ? ? ? ? ? no "Kort merknad til loggpost." no no "60" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"BongHode.bongHode_EDatoTid" "Endret dato/tid" "X(22)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"BongHode.bongHode_eAv" "Endret av" "X(15)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > BongHode.GruppeNr
"BongHode.GruppeNr" "GrNr" ">9" "INTEGER" ? ? ? ? ? ? no "Gruppenummer." no no "4.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > BongHode.Dato
"BongHode.Dato" "Dato" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato for utstedelse av bongen" no no "9.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > BongHode.Tid
"BongHode.Tid" "Tid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt for utstedelse av bongen" no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwBongHode */
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
  RUN blankFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiAnsattNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAnsattNr C-Win
ON LEAVE OF fiAnsattNr IN FRAME DEFAULT-FRAME /* Ansattnr */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiAnsattNr C-Win
ON RETURN OF fiAnsattNr IN FRAME DEFAULT-FRAME /* Ansattnr */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMerknad C-Win
ON LEAVE OF fiMerknad IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMerknad C-Win
ON RETURN OF fiMerknad IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNavn C-Win
ON LEAVE OF fiNavn IN FRAME DEFAULT-FRAME /* Navn */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiNavn C-Win
ON RETURN OF fiNavn IN FRAME DEFAULT-FRAME /* Navn */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSelgerId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSelgerId C-Win
ON LEAVE OF fiSelgerId IN FRAME DEFAULT-FRAME /* Selgerid */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSelgerId C-Win
ON RETURN OF fiSelgerId IN FRAME DEFAULT-FRAME /* Selgerid */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwBongHode
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
{incl/conttrigg.i oBrwBongHode:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE blankFilter C-Win 
PROCEDURE blankFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    DO:
      ASSIGN
        fiAnsattNr:SCREEN-VALUE      = ''
        fiNavn:SCREEN-VALUE     = ''
        fiMerknad:SCREEN-VALUE = ''
        .
/*      oBrwBongHode:setFilter('BongHode_AnsattNr,BongHode_AnsNavn,Logg',                                      */
/*                           ',,',                                                                             */
/*                           fiAnsattNr:SCREEN-VALUE + "|" + fiNavn:SCREEN-VALUE + "|" + fiMerknad:SCREEN-VALUE*/
/*                           ).                                                                                */
      oBrwBongHode:setFilter(cBaseFields,cBaseOperator,cBaseWhere).
      oBrwBongHode:OpenQuery().
                           
      oBrwBongHode:OpenQuery().
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
IF oBrwBongHode:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  
  RUN SUPER.
  
  IF oBrwBongHode:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
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
  DISPLAY fiSelgerId fiAnsattNr fiNavn fiMerknad 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbBongHode RECT-4 first_tbBongHode prev_tbBongHode next_tbBongHode 
         last_tbBongHode delete_tbBongHode refresh_tbBongHode filter_tbBongHode 
         excel_tbBongHode btnBlank fiSelgerId fiAnsattNr fiNavn fiMerknad 
         BrwBongHode 
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
DEF VAR oBrwFillInLogg AS JBoxBrowseFillIn NO-UNDO.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

DO WITH FRAME {&FRAME-NAME}:

  oBrwBongHode = NEW JBoxBrowse(brwBongHode:HANDLE).

  oBrwBongHode:baseQuery  = "WHERE ButikkNr = 16 AND GruppeNr = 1 AND KasseNr >= 0 AND Dato >= '" + STRING(TODAY - 7) + "' AND BongNR >= 0 ".
  oBrwBongHode:setQuerySort("ButikkNr,GruppeNr,Dato;DESC,Tid;Desc,BongNr;DESC").

  otbBongHode = NEW JBoxToolbar(tbBongHode:HANDLE).
  otbBongHode:addToolGroup("Innlogging;Innlogging¤enable,Utlogging;Utlogging¤enable").
  otbBongHode:disabledTools = 'Filter'.
  hLoggColumn = oBrwBongHode:getColumnHandle("Logg").
  oBrwFillInLogg = NEW JBoxBrowseFillIn(oBrwBongHode,"Logg",TRUE).

  oBrwBongHode:TOOLBAR-OBJECT = otbBongHode.
  oContainer:setNoResizeY("RECT-4").
  
  ASSIGN 
    cBaseFields   = 'bongHode_IntTTId,bongHode_IntTTId'
    cBaseOperator = '>=,<='
    cFields       = 'bongHode_IntTTId,bongHode_IntTTId,BongHode_SelgerId,BongHode_AnsattNr,BongHode_AnsNavn,Logg'
    cBaseWhere    = '96|97'
    . 
  
  
END.
RUN setFilter.
oBrwBongHode:OpenQuery().

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  oContainer:initResize().
&ELSE
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|ServerQuery.log," +   
                    "TransLogFile|ServerTrans.log").
&ENDIF
  APPLY 'ENTRY' TO fiAnsattNr IN FRAME DEFAULT-FRAME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnloggingRecord C-Win 
PROCEDURE InnloggingRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  DO WITH FRAME DEFAULT-FRAME:
    RUN loggInnUt ('96').
      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loggInnUt C-Win 
PROCEDURE loggInnUt :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcTTId AS CHARACTER NO-UNDO.
  
  CURRENT-WINDOW:SENSITIVE = FALSE.    
  RUN LoggInnUtSelger.w (INPUT pcTTId, OUTPUT obOk, OUTPUT ocReturn).
  CURRENT-WINDOW:SENSITIVE = TRUE.

  IF obOk AND ocReturn <> '' THEN
  DO: 
    IF NOT DYNAMIC-FUNCTION("runproc",
                            "bonghode_OpprettInnUtLogging.p",
                            ocReturn,
                            ?) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    ELSE DO:
      oBrwBongHode:OpenQuery().
    END.
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

DO WITH FRAME DEFAULT-FRAME:
  DYNAMIC-FUNCTION("setPostUpdProc","bonghode_post_update.p").
  obOK = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,"",
              "",
              ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
              DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
              ihFillIn:SCREEN-VALUE,
              TRUE).
  oBrwBongHode:refreshRow(). 
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
    /* Gjør bare noe hvis filter skal blankes eller nytt filter settes. */
    IF (fiAnsattNr:SCREEN-VALUE + fiNavn:SCREEN-VALUE + fiMerknad:SCREEN-VALUE <> '') OR
        (cWhere + cOperator <> '') OR INT(fiSelgerId:SCREEN-VALUE) > 0 THEN
    DO:
      cWhere = '96|97|' + 
               TRIM(fiSelgerId:SCREEN-VALUE) + '|' +
               TRIM(fiAnsattNr:SCREEN-VALUE,'*') + '|*' + 
               TRIM(fiNavn:SCREEN-VALUE,'*') + "*|*" + 
               TRIM(fiMerknad:SCREEN-VALUE,'*') + '*'.  
      cOperator = '>=,<=,' +
                  (IF fiSelgerId:SCREEN-VALUE = '0' THEN '>=' ELSE '=') + ',' +
                  (IF fiAnsattNr:SCREEN-VALUE <> '' THEN 'BEGINS' ELSE '') + ',' +
                  (IF fiNavn:SCREEN-VALUE <> '' THEN 'MATCHES' ELSE '') + ',' +
                  (IF fiMerknad:SCREEN-VALUE <> '' THEN 'MATCHES' ELSE '')
                  .

      oBrwBongHode:setFilter(cFields,cOperator,cWhere).
      oBrwBongHode:OpenQuery().
      APPLY 'ENTRY' TO fiAnsattNr IN FRAME DEFAULT-FRAME.
    END.
    ELSE DO:
      oBrwBongHode:setFilter(cBaseFields,cBaseOperator,cBaseWhere).
      oBrwBongHode:OpenQuery().
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtloggingRecord C-Win 
PROCEDURE UtloggingRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
    RUN loggInnUt ('97').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

