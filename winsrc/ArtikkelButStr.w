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
DEFINE VARIABLE hLager AS HANDLE NO-UNDO.
DEFINE VARIABLE hColLblStr AS HANDLE EXTENT 50 NO-UNDO.
DEFINE VARIABLE cAlfaFordeling AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStrBrukFordeling AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOffSeth AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE ipMaks AS INTEGER NO-UNDO.

/* Standard funksjoner for logging */
DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.
cLogg  = 'ArtikkelButStr' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','').
rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner( cLogg ) NO-ERROR.

ASSIGN 
  ipMaks = 50
  .

/* Code for Definitions: */

/*** Start instance property definitions for JBoxBrowse object oBrwLager ***/
DEF VAR oBrwLager AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Lager
    FIELD ArtikkelNr AS DECIMAL
    FIELD Butik AS INTEGER
    FIELD Lager_ButNr AS CHARACTER
    FIELD LagAnt AS DECIMAL
    FIELD VVarekost AS DECIMAL
    FIELD Str1 AS CHARACTER
    FIELD Str2 AS CHARACTER
    FIELD Str3 AS CHARACTER
    FIELD Str4 AS CHARACTER
    FIELD Str5 AS CHARACTER
    FIELD Str6 AS CHARACTER
    FIELD Str7 AS CHARACTER
    FIELD Str8 AS CHARACTER
    FIELD Str9 AS CHARACTER
    FIELD Str10 AS CHARACTER
    FIELD Str11 AS CHARACTER
    FIELD Str12 AS CHARACTER
    FIELD Str13 AS CHARACTER
    FIELD Str14 AS CHARACTER
    FIELD Str15 AS CHARACTER
    FIELD Str16 AS CHARACTER
    FIELD Str17 AS CHARACTER
    FIELD Str18 AS CHARACTER
    FIELD Str19 AS CHARACTER
    FIELD Str20 AS CHARACTER
    FIELD Str21 AS CHARACTER
    FIELD Str22 AS CHARACTER
    FIELD Str23 AS CHARACTER
    FIELD Str24 AS CHARACTER
    FIELD Str25 AS CHARACTER
    FIELD Str26 AS CHARACTER
    FIELD Str27 AS CHARACTER
    FIELD Str28 AS CHARACTER
    FIELD Str29 AS CHARACTER
    FIELD Str30 AS CHARACTER
    FIELD Str31 AS CHARACTER
    FIELD Str32 AS CHARACTER
    FIELD Str33 AS CHARACTER
    FIELD Str34 AS CHARACTER
    FIELD Str35 AS CHARACTER
    FIELD Str36 AS CHARACTER
    FIELD Str37 AS CHARACTER
    FIELD Str38 AS CHARACTER
    FIELD Str39 AS CHARACTER
    FIELD Str40 AS CHARACTER
    FIELD Str41 AS CHARACTER
    FIELD Str42 AS CHARACTER
    FIELD Str43 AS CHARACTER
    FIELD Str44 AS CHARACTER
    FIELD Str45 AS CHARACTER
    FIELD Str46 AS CHARACTER
    FIELD Str47 AS CHARACTER
    FIELD Str48 AS CHARACTER
    FIELD Str49 AS CHARACTER
    FIELD Str50 AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Lager FOR TEMP-TABLE Lager.


FUNCTION getBuffersAndFieldsBrwLager RETURNS CHARACTER():
  RETURN
    'Lager'
     + ';ArtikkelNr'
     + ';Butik'
     + ';LagAnt'
     + ';VVarekost'
     + ';+Lager_ButNr|CHARACTER||Lager_ButNr|Butnr'
     + ';+Str1|CHARACTER||jb_void(N/A)|Str1'
     + ';+Str2|CHARACTER||jb_void(N/A)|Str2'
     + ';+Str3|CHARACTER||jb_void(N/A)|Str3'
     + ';+Str4|CHARACTER||jb_void(N/A)|Str4'
     + ';+Str5|CHARACTER||jb_void(N/A)|Str5'
     + ';+Str6|CHARACTER||jb_void(N/A)|Str6'
     + ';+Str7|CHARACTER||jb_void(N/A)|Str7'
     + ';+Str8|CHARACTER||jb_void(N/A)|Str8'
     + ';+Str9|CHARACTER||jb_void(N/A)|Str9'
     + ';+Str10|CHARACTER||jb_void(N/A)|Str10'
     + ';+Str11|CHARACTER||jb_void(N/A)|Str11'
     + ';+Str12|CHARACTER||jb_void(N/A)|Str12'
     + ';+Str13|CHARACTER||jb_void(N/A)|Str13'
     + ';+Str14|CHARACTER||jb_void(N/A)|Str14'
     + ';+Str15|CHARACTER||jb_void(N/A)|Str15'
     + ';+Str16|CHARACTER||jb_void(N/A)|Str16'
     + ';+Str17|CHARACTER||jb_void(N/A)|Str17'
     + ';+Str18|CHARACTER||jb_void(N/A)|Str18'
     + ';+Str19|CHARACTER||jb_void(N/A)|Str19'
     + ';+Str20|CHARACTER||jb_void(N/A)|Str20'
     + ';+Str21|CHARACTER||jb_void(N/A)|Str21'
     + ';+Str22|CHARACTER||jb_void(N/A)|Str22'
     + ';+Str23|CHARACTER||jb_void(N/A)|Str23'
     + ';+Str24|CHARACTER||jb_void(N/A)|Str24'
     + ';+Str25|CHARACTER||jb_void(N/A)|Str25'
     + ';+Str26|CHARACTER||jb_void(N/A)|Str26'
     + ';+Str27|CHARACTER||jb_void(N/A)|Str27'
     + ';+Str28|CHARACTER||jb_void(N/A)|Str28'
     + ';+Str29|CHARACTER||jb_void(N/A)|Str29'
     + ';+Str30|CHARACTER||jb_void(N/A)|Str30'
     + ';+Str31|CHARACTER||jb_void(N/A)|Str31'
     + ';+Str32|CHARACTER||jb_void(N/A)|Str32'
     + ';+Str33|CHARACTER||jb_void(N/A)|Str33'
     + ';+Str34|CHARACTER||jb_void(N/A)|Str34'
     + ';+Str35|CHARACTER||jb_void(N/A)|Str35'
     + ';+Str36|CHARACTER||jb_void(N/A)|Str36'
     + ';+Str37|CHARACTER||jb_void(N/A)|Str37'
     + ';+Str38|CHARACTER||jb_void(N/A)|Str38'
     + ';+Str39|CHARACTER||jb_void(N/A)|Str39'
     + ';+Str40|CHARACTER||jb_void(N/A)|Str40'
     + ';+Str41|CHARACTER||jb_void(N/A)|Str41'
     + ';+Str42|CHARACTER||jb_void(N/A)|Str42'
     + ';+Str43|CHARACTER||jb_void(N/A)|Str43'
     + ';+Str44|CHARACTER||jb_void(N/A)|Str44'
     + ';+Str45|CHARACTER||jb_void(N/A)|Str45'
     + ';+Str46|CHARACTER||jb_void(N/A)|Str46'
     + ';+Str47|CHARACTER||jb_void(N/A)|Str47'
     + ';+Str48|CHARACTER||jb_void(N/A)|Str48'
     + ';+Str49|CHARACTER||jb_void(N/A)|Str49'
     + ';+Str50|CHARACTER||jb_void(N/A)|Str50'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwLager RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwLager RETURNS CHARACTER():
  RETURN 
     'server/lager_brwcalc.p' /* Lager_ButNr */
     .
END FUNCTION.


DEF VAR oTabArtBas AS JBoxMsTabs NO-UNDO.

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
&Scoped-define FIELDS-IN-QUERY-BrwLager Lager.ArtikkelNr Lager.Butik ~
Lager.Lager_ButNr Lager.LagAnt Lager.VVarekost Lager.Str1 Lager.Str2 ~
Lager.Str3 Lager.Str4 Lager.Str5 Lager.Str6 Lager.Str7 Lager.Str8 ~
Lager.Str9 Lager.Str10 Lager.Str11 Lager.Str12 Lager.Str13 Lager.Str14 ~
Lager.Str15 Lager.Str16 Lager.Str17 Lager.Str18 Lager.Str19 Lager.Str20 ~
Lager.Str21 Lager.Str22 Lager.Str23 Lager.Str24 Lager.Str25 Lager.Str26 ~
Lager.Str27 Lager.Str28 Lager.Str29 Lager.Str30 Lager.Str31 Lager.Str32 ~
Lager.Str33 Lager.Str34 Lager.Str35 Lager.Str36 Lager.Str37 Lager.Str38 ~
Lager.Str39 Lager.Str40 Lager.Str41 Lager.Str42 Lager.Str43 Lager.Str44 ~
Lager.Str45 Lager.Str46 Lager.Str47 Lager.Str48 Lager.Str49 Lager.Str50 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwLager Lager.ArtikkelNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwLager Lager
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwLager Lager
&Scoped-define QUERY-STRING-BrwLager FOR EACH Lager NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwLager OPEN QUERY BrwLager FOR EACH Lager NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwLager Lager
&Scoped-define FIRST-TABLE-IN-QUERY-BrwLager Lager


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwLager 

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
DEFINE QUERY BrwLager FOR 
      Lager SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwLager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwLager C-Win _STRUCTURED
  QUERY BrwLager NO-LOCK DISPLAY
      Lager.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      Lager.Butik COLUMN-LABEL "ButNr" FORMAT ">>>>>9":U
      Lager.Lager_ButNr COLUMN-LABEL "Butnr" FORMAT "X(12)":U WIDTH 10
      Lager.LagAnt COLUMN-LABEL "Antall" FORMAT "->>,>>>,>>9":U
      Lager.VVarekost COLUMN-LABEL "V.varekost" FORMAT "->,>>>,>>9.99":U
      Lager.Str1 COLUMN-LABEL "Str1" FORMAT "X(10)":U WIDTH 8
      Lager.Str2 COLUMN-LABEL "Str2" FORMAT "X(8)":U
      Lager.Str3 COLUMN-LABEL "Str3" FORMAT "X(8)":U
      Lager.Str4 COLUMN-LABEL "Str4" FORMAT "X(8)":U
      Lager.Str5 COLUMN-LABEL "Str5" FORMAT "X(8)":U
      Lager.Str6 COLUMN-LABEL "Str6" FORMAT "X(8)":U
      Lager.Str7 COLUMN-LABEL "Str7" FORMAT "X(8)":U
      Lager.Str8 COLUMN-LABEL "Str8" FORMAT "X(8)":U
      Lager.Str9 COLUMN-LABEL "Str9" FORMAT "X(8)":U
      Lager.Str10 COLUMN-LABEL "Str10" FORMAT "X(8)":U
      Lager.Str11 COLUMN-LABEL "Str11" FORMAT "X(8)":U
      Lager.Str12 COLUMN-LABEL "Str12" FORMAT "X(8)":U
      Lager.Str13 COLUMN-LABEL "Str13" FORMAT "X(8)":U
      Lager.Str14 COLUMN-LABEL "Str14" FORMAT "X(8)":U
      Lager.Str15 COLUMN-LABEL "Str15" FORMAT "X(8)":U
      Lager.Str16 COLUMN-LABEL "Str16" FORMAT "X(8)":U
      Lager.Str17 COLUMN-LABEL "Str17" FORMAT "X(8)":U
      Lager.Str18 COLUMN-LABEL "Str18" FORMAT "X(8)":U
      Lager.Str19 COLUMN-LABEL "Str19" FORMAT "X(8)":U
      Lager.Str20 COLUMN-LABEL "Str20" FORMAT "X(8)":U
      Lager.Str21 COLUMN-LABEL "Str21" FORMAT "X(8)":U
      Lager.Str22 COLUMN-LABEL "Str22" FORMAT "X(8)":U
      Lager.Str23 COLUMN-LABEL "Str23" FORMAT "X(8)":U
      Lager.Str24 COLUMN-LABEL "Str24" FORMAT "X(8)":U
      Lager.Str25 COLUMN-LABEL "Str25" FORMAT "X(8)":U
      Lager.Str26 COLUMN-LABEL "Str26" FORMAT "X(8)":U
      Lager.Str27 COLUMN-LABEL "Str27" FORMAT "X(8)":U
      Lager.Str28 COLUMN-LABEL "Str28" FORMAT "X(8)":U
      Lager.Str29 COLUMN-LABEL "Str29" FORMAT "X(8)":U
      Lager.Str30 COLUMN-LABEL "Str30" FORMAT "X(8)":U
      Lager.Str31 COLUMN-LABEL "Str31" FORMAT "X(10)":U
      Lager.Str32 COLUMN-LABEL "Str32" FORMAT "X(10)":U
      Lager.Str33 COLUMN-LABEL "Str33" FORMAT "X(10)":U
      Lager.Str34 COLUMN-LABEL "Str34" FORMAT "X(10)":U
      Lager.Str35 COLUMN-LABEL "Str35" FORMAT "X(10)":U
      Lager.Str36 COLUMN-LABEL "Str36" FORMAT "X(10)":U
      Lager.Str37 COLUMN-LABEL "Str37" FORMAT "X(10)":U
      Lager.Str38 COLUMN-LABEL "Str38" FORMAT "X(10)":U
      Lager.Str39 COLUMN-LABEL "Str39" FORMAT "X(10)":U
      Lager.Str40 COLUMN-LABEL "Str40" FORMAT "X(10)":U
      Lager.Str41 COLUMN-LABEL "Str41" FORMAT "X(10)":U
      Lager.Str42 COLUMN-LABEL "Str42" FORMAT "X(10)":U
      Lager.Str43 COLUMN-LABEL "Str43" FORMAT "X(10)":U
      Lager.Str44 COLUMN-LABEL "Str44" FORMAT "X(10)":U
      Lager.Str45 COLUMN-LABEL "Str45" FORMAT "X(10)":U
      Lager.Str46 COLUMN-LABEL "Str46" FORMAT "X(10)":U
      Lager.Str47 COLUMN-LABEL "Str47" FORMAT "X(10)":U
      Lager.Str48 COLUMN-LABEL "Str48" FORMAT "X(10)":U
      Lager.Str49 COLUMN-LABEL "Str49" FORMAT "X(10)":U
      Lager.Str50 COLUMN-LABEL "Str50" FORMAT "X(10)":U
  ENABLE
      Lager.ArtikkelNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 104 BY 17.86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwLager AT ROW 1.48 COL 3.2 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 107.6 BY 18.76 WIDGET-ID 100.


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
         HEIGHT             = 18.76
         WIDTH              = 107.6
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
/* BROWSE-TAB BrwLager 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 18.76
       FRAME DEFAULT-FRAME:WIDTH            = 107.6.

ASSIGN 
       BrwLager:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 3.

ASSIGN 
       Lager.ArtikkelNr:VISIBLE IN BROWSE BrwLager = FALSE
       Lager.Butik:VISIBLE IN BROWSE BrwLager = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwLager
/* Query rebuild information for BROWSE BrwLager
     _TblList          = "SkoTex.Lager"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Lager.ArtikkelNr
"Lager.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? yes "" no no "14.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Lager.Butik
"Lager.Butik" "ButNr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikknummer" no no "7.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Lager.Lager_ButNr" "Butnr" "X(12)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Lager.LagAnt
"Lager.LagAnt" "Antall" "->>,>>>,>>9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Lager.VVarekost
"Lager.VVarekost" "V.varekost" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Vektet varekost" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Lager.Str1" "Str1" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Lager.Str2" "Str2" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Lager.Str3" "Str3" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Lager.Str4" "Str4" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"Lager.Str5" "Str5" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Lager.Str6" "Str6" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"Lager.Str7" "Str7" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"Lager.Str8" "Str8" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"Lager.Str9" "Str9" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"Lager.Str10" "Str10" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"Lager.Str11" "Str11" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"Lager.Str12" "Str12" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"Lager.Str13" "Str13" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"Lager.Str14" "Str14" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"Lager.Str15" "Str15" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"Lager.Str16" "Str16" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"Lager.Str17" "Str17" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"Lager.Str18" "Str18" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"Lager.Str19" "Str19" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"Lager.Str20" "Str20" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"Lager.Str21" "Str21" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"Lager.Str22" "Str22" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"Lager.Str23" "Str23" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"Lager.Str24" "Str24" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"Lager.Str25" "Str25" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"Lager.Str26" "Str26" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"Lager.Str27" "Str27" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"Lager.Str28" "Str28" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"Lager.Str29" "Str29" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > "_<CALC>"
"Lager.Str30" "Str30" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > "_<CALC>"
"Lager.Str31" "Str31" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > "_<CALC>"
"Lager.Str32" "Str32" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[38]   > "_<CALC>"
"Lager.Str33" "Str33" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[39]   > "_<CALC>"
"Lager.Str34" "Str34" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[40]   > "_<CALC>"
"Lager.Str35" "Str35" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[41]   > "_<CALC>"
"Lager.Str36" "Str36" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[42]   > "_<CALC>"
"Lager.Str37" "Str37" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[43]   > "_<CALC>"
"Lager.Str38" "Str38" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[44]   > "_<CALC>"
"Lager.Str39" "Str39" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[45]   > "_<CALC>"
"Lager.Str40" "Str40" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[46]   > "_<CALC>"
"Lager.Str41" "Str41" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[47]   > "_<CALC>"
"Lager.Str42" "Str42" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[48]   > "_<CALC>"
"Lager.Str43" "Str43" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[49]   > "_<CALC>"
"Lager.Str44" "Str44" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[50]   > "_<CALC>"
"Lager.Str45" "Str45" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[51]   > "_<CALC>"
"Lager.Str46" "Str46" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[52]   > "_<CALC>"
"Lager.Str47" "Str47" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[53]   > "_<CALC>"
"Lager.Str48" "Str48" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[54]   > "_<CALC>"
"Lager.Str49" "Str49" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[55]   > "_<CALC>"
"Lager.Str50" "Str50" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwLager */
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
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  
  IF oBrwLager:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}:
    IF (cAlfaFordeling > '' AND cStrBrukFordeling > '') THEN
    DO:
      /* Blanker overskriftene. */
      DO piLoop = 1 TO ipMaks:
          hColLblStr[piLoop]:LABEL = ''.
      END.
      piLoop = 0. 
      /* Legger opp størrelser der hvor det ligger noe lager. */
      STRLOOP: 
      DO iLoop = (1 + iOffSeth) TO NUM-ENTRIES(cAlfaFordeling):
          IF iLoop > ipMaks THEN 
            LEAVE STRLOOP.
          piLoop = piLoop + 1. /* Kollonneteller */
          IF iLoop > NUM-ENTRIES(cStrBrukFordeling) THEN 
            LEAVE STRLOOP.
          IF TRIM(ENTRY(iLoop,cAlfaFordeling)) <> '' THEN  
            hColLblStr[piLoop]:LABEL = FILL(' ',9 - LENGTH(ENTRY(iLoop,cAlfaFordeling))) + ENTRY(iLoop,cAlfaFordeling).
      END. /* STRLOOP */
    END.
    /* Legger opp størrelse på artikler som ikke har noe på lager. */
    ELSE IF cAlfaFordeling > '' THEN 
      BLANKLOOP: 
      DO iLoop = 1 TO ipMaks:
        IF iLoop <= NUM-ENTRIES(cAlfaFordeling) THEN
          hColLblStr[iLoop]:LABEL = IF TRIM(ENTRY(iLoop,cAlfaFordeling)) <> '' THEN FILL(' ',9 - LENGTH(ENTRY(iLoop,cAlfaFordeling))) + ENTRY(iLoop,cAlfaFordeling) ELSE ''.
        ELSE
          hColLblStr[iLoop]:LABEL = ''.
      END. /* BLANKLOOP */
  END.
  RUN SUPER.
  IF oBrwLager:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
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
  ENABLE BrwLager 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLagerForArtBas C-Win 
PROCEDURE getLagerForArtBas :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER plArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
  DEFINE VARIABLE pcRetParam AS CHARACTER NO-UNDO.

  hLager = BUFFER Lager:HANDLE.

    ASSIGN 
      cAlfaFordeling    = '' 
      cStrBrukFordeling = '' 
      iOffSeth          = 0
    .
    
  IF JBoxServerAPI:Instance:CallServerProc("lager_butikkstr.p",STRING(plArtikkelNr),hLager) THEN
  DO:
    hLager = JBoxServerAPI:Instance:getCallReturnTable().
    pcRetParam = JBoxServerApi:Instance:getCallReturnParam().

    IF NUM-ENTRIES(pcRetParam,'|') >= 3 THEN 
    DO:
      ASSIGN 
        cAlfaFordeling    = TRIM(ENTRY(1,pcRetParam,'|'),',') 
        iOffSeth          = INT(ENTRY(3,pcRetParam,'|'))
        cStrBrukFordeling = TRIM(ENTRY(2,pcRetParam,'|'),',')
        cStrBrukFordeling = (IF iOffSeth > 0 THEN FILL(',',iOffSeth) + cStrBrukFordeling ELSE cStrBrukFordeling) 
      .
    END.
  END.

  IF bTest THEN 
  DO:
/*    TEMP-TABLE Lager:WRITE-JSON('file', 'konv\LagertTabell' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json', TRUE).*/
  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start getLagerForArtBas.' 
        ).
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
        ' cAlfaFordeling: ' + cAlfaFordeling 
        ).
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
        ' cStrBrukFordeling: ' + cStrBrukFordeling 
        ).
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
        ' iOffSeth: ' + STRING(iOffSeth) 
        ).
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
        'Slutt.' 
        ).
  END.

//  oBrwLager:OpenQuery(). TN 17/2-20 Ikke gjør dette her. Det gjøres fra hovedprorammet.
  
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
  IF SEARCH('tnc.txt') <> ? THEN 
    bTest = TRUE.
      
  oBrwLager = NEW JBoxBrowse(brwLager:HANDLE).
  oBrwLager:useLocalData = YES.
  oBrwLager:setQuerySort("Butik").
  
  DO iLoop = 1 TO ipMaks:
    hColLblStr[iLoop] = oBrwLager:getColumnHandle('Str' + STRING(iLoop)).
  END.
  
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

