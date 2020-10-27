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

  Author:            tomn@nsoft.no

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
DEFINE VARIABLE hbfKOrdre_Id AS HANDLE NO-UNDO.
DEFINE VARIABLE hbcKOrdre_Id AS HANDLE NO-UNDO.
DEFINE VARIABLE otbKOrdreHode AS JBoxToolbar NO-UNDO.
DEFINE VARIABLE hKordre_IdColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreHode ***/
DEFINE VARIABLE oBrwKOrdreHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreHode
    FIELD DatoTidOpprettet AS DATETIME
    FIELD Navn AS CHARACTER
    FIELD KOrdre_Id AS DECIMAL
    FIELD kordre_LevStatus AS CHARACTER
    FIELD LevFormBeskrivelse AS CHARACTER
    FIELD EkstOrdreNr AS CHARACTER
    FIELD Totalt AS DECIMAL
    FIELD AntApnet AS INTEGER
    FIELD AntPPEti AS INTEGER
    FIELD Leveringsdato AS DATE
    FIELD SendingsNr AS CHARACTER
    FIELD ReturNr AS CHARACTER
    FIELD ShipmentSendt AS CHARACTER
    FIELD MobilTlf AS CHARACTER
    FIELD kordre_Butikk AS CHARACTER
    FIELD LevAdresse1 AS CHARACTER
    FIELD LevPostNr AS CHARACTER
    FIELD LevPostSted AS CHARACTER
    FIELD LevLand AS CHARACTER
    FIELD LevStatus AS CHARACTER
    FIELD cOpt1 AS CHARACTER
    FIELD LevFNr AS INTEGER
    FIELD ePostAdresse AS CHARACTER
    FIELD Opphav AS INTEGER
    FIELD KundeNr AS DECIMAL
    FIELD Faktura_Id AS DECIMAL
    FIELD kordre_FakturaNr AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3
    .
DEFINE BUFFER v_KOrdreHode FOR TEMP-TABLE KOrdreHode.



/* ************************  Function Implementations ***************** */



FUNCTION getBuffersAndFieldsBrwKOrdreHode RETURNS CHARACTER():
  RETURN
    'KOrdreHode'
     + ';DatoTidOpprettet'
     + ';KOrdre_Id'
     + ';EkstOrdreNr'
     + ';Totalt'
     + ';AntApnet'
     + ';AntPPEti'
     + ';Leveringsdato'
     + ';SendingsNr'
     + ';ReturNr'
     + ';MobilTlf'
     + ';LevAdresse1'
     + ';LevPostNr'
     + ';LevPostSted'
     + ';LevLand'
     + ';LevStatus'
     + ';cOpt1'
     + ';LevFNr'
     + ';ePostAdresse'
     + ';Opphav'
     + ';KundeNr'
     + ';Faktura_Id'
     + ';+kordre_LevStatus|CHARACTER||kordre_LevStatus|Leveringsstatus'
     + ';+ShipmentSendt|CHARACTER||kordre_ShipmentSendt|Shipment sendt'
     + ';+kordre_Butikk|CHARACTER||kordre_Butikk|Butikk'
     + ';+kordre_FakturaNr|CHARACTER||kordre_FakturaNr|FakturaNr'
  + ',Kunde'
     + ';Navn'
  + ',LeveringsForm'
     + ';LevFormBeskrivelse'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreHode RETURNS CHARACTER():
  RETURN 'EACH Kunde OF KOrdreHode NO-LOCK,EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.DatoTidEndret DESCENDING'.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreHode RETURNS CHARACTER():
  RETURN 
     'kordre_brwcalc.p' /* kordre_LevStatus */
   + ',kordre_brwcalc.p' /* kordre_ShipmentSendt */
   + ',kordre_brwcalc.p' /* kordre_Butikk */
   + ',kordre_brwcalc.p' /* kordre_FakturaNr */
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
&Scoped-define BROWSE-NAME BrwKOrdreHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KOrdreHode

/* Definitions for BROWSE BrwKOrdreHode                                 */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.DatoTidOpprettet ~
KOrdreHode.Navn KOrdreHode.KOrdre_Id KOrdreHode.kordre_LevStatus ~
KOrdreHode.LevFormBeskrivelse KOrdreHode.EkstOrdreNr KOrdreHode.Totalt ~
KOrdreHode.AntApnet KOrdreHode.AntPPEti KOrdreHode.Leveringsdato ~
KOrdreHode.SendingsNr KOrdreHode.ReturNr KOrdreHode.ShipmentSendt ~
KOrdreHode.MobilTlf KOrdreHode.kordre_Butikk KOrdreHode.LevAdresse1 ~
KOrdreHode.LevPostNr KOrdreHode.LevPostSted KOrdreHode.LevLand ~
KOrdreHode.LevStatus KOrdreHode.cOpt1 KOrdreHode.LevFNr ~
KOrdreHode.ePostAdresse KOrdreHode.Opphav KOrdreHode.KundeNr ~
KOrdreHode.Faktura_Id KOrdreHode.kordre_FakturaNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreHode ~
KOrdreHode.DatoTidOpprettet KOrdreHode.Navn 
&Scoped-define QUERY-STRING-BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK ~
    BY KOrdreHode.DatoTidEndret DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreHode OPEN QUERY BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK ~
    BY KOrdreHode.DatoTidEndret DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbKOrdreHode first_tbKOrdreHode ~
prev_tbKOrdreHode next_tbKOrdreHode last_tbKOrdreHode refresh_tbKOrdreHode ~
filter_tbKOrdreHode excel_tbKOrdreHode multiSortBrowse_tbKOrdreHode ~
browseconfig_tbKOrdreHode ficPPKode BrwKOrdreHode 
&Scoped-Define DISPLAYED-OBJECTS ficPPKode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbKOrdreHode 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Column setup" 
     SIZE 4.6 BY 1.1 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON excel_tbKOrdreHode 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbKOrdreHode 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbKOrdreHode 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbKOrdreHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON multiSortBrowse_tbKOrdreHode 
     IMAGE-UP FILE "bmp/bullet_triangle_green.bmp":U
     LABEL "Sorter på flere kolonner" 
     SIZE 4.6 BY 1.1 TOOLTIP "Sorter på flere kolonner (ALT-S)".

DEFINE BUTTON next_tbKOrdreHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKOrdreHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKOrdreHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE ficPPKode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Skann postpakke kode" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Skann postpakke etikettens strekkode for å sette den klar for utlevering."
     BGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 8  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 163 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwKOrdreHode FOR 
      KOrdreHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwKOrdreHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreHode C-Win _STRUCTURED
  QUERY BrwKOrdreHode NO-LOCK DISPLAY
      KOrdreHode.DatoTidOpprettet COLUMN-LABEL "DatoTidOpprettet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      KOrdreHode.Navn COLUMN-LABEL "Navn" FORMAT "X(40)":U
      KOrdreHode.KOrdre_Id COLUMN-LABEL "Ordrenummer" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.kordre_LevStatus COLUMN-LABEL "Leveringsstatus" FORMAT "X(30)":U
      KOrdreHode.LevFormBeskrivelse COLUMN-LABEL "Leveringsform" FORMAT "X(30)":U
      KOrdreHode.EkstOrdreNr COLUMN-LABEL "Ekst.ordrenr" FORMAT "X(15)":U
      KOrdreHode.Totalt COLUMN-LABEL "Totalt" FORMAT "->>>,>>>,>>9.99":U
      KOrdreHode.AntApnet COLUMN-LABEL "Ant. pksdl" FORMAT ">9":U
            WIDTH 9.8
      KOrdreHode.AntPPEti COLUMN-LABEL "Antall etiketter" FORMAT ">9":U
      KOrdreHode.Leveringsdato COLUMN-LABEL "Leveringsdato" FORMAT "99/99/99":U
      KOrdreHode.SendingsNr COLUMN-LABEL "Sendingsnummer" FORMAT "X(30)":U
      KOrdreHode.ReturNr COLUMN-LABEL "Retur nr." FORMAT "x(30)":U
      KOrdreHode.ShipmentSendt COLUMN-LABEL "Shipment sendt" FORMAT "X(20)":U
            WIDTH 18
      KOrdreHode.MobilTlf COLUMN-LABEL "Mobiltelefon" FORMAT "X(15)":U
      KOrdreHode.kordre_Butikk COLUMN-LABEL "Butikk" FORMAT "X(30)":U
      KOrdreHode.LevAdresse1 COLUMN-LABEL "Leveringsadresse" FORMAT "X(40)":U
      KOrdreHode.LevPostNr COLUMN-LABEL "Lev. PostNr" FORMAT "X(10)":U
      KOrdreHode.LevPostSted COLUMN-LABEL "Poststed" FORMAT "X(30)":U
      KOrdreHode.LevLand COLUMN-LABEL "Lev. Land" FORMAT "X(30)":U
      KOrdreHode.LevStatus COLUMN-LABEL "Lev.status" FORMAT "x(2)":U
      KOrdreHode.cOpt1 COLUMN-LABEL "cOpt1" FORMAT "x(8)":U
      KOrdreHode.LevFNr COLUMN-LABEL "Leveringsform" FORMAT ">9":U
      KOrdreHode.ePostAdresse COLUMN-LABEL "E-Post" FORMAT "X(40)":U
      KOrdreHode.Opphav COLUMN-LABEL "Opphav" FORMAT ">9":U
      KOrdreHode.KundeNr COLUMN-LABEL "KundeNr" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.Faktura_Id COLUMN-LABEL "FakturaId" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.kordre_FakturaNr COLUMN-LABEL "FakturaNr" FORMAT "X(13)":U
  ENABLE
      KOrdreHode.DatoTidOpprettet HELP "Dato og klokkeslett for opprettelse av ordre."
      KOrdreHode.Navn HELP "Navn eller firmanavn"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 163 BY 18.14 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbKOrdreHode AT ROW 1.33 COL 2 WIDGET-ID 10
     prev_tbKOrdreHode AT ROW 1.33 COL 6.8 WIDGET-ID 12
     next_tbKOrdreHode AT ROW 1.33 COL 11.4 WIDGET-ID 14
     last_tbKOrdreHode AT ROW 1.33 COL 16 WIDGET-ID 16
     refresh_tbKOrdreHode AT ROW 1.33 COL 20.6 WIDGET-ID 6
     filter_tbKOrdreHode AT ROW 1.33 COL 25.2 WIDGET-ID 22
     excel_tbKOrdreHode AT ROW 1.33 COL 29.8 WIDGET-ID 8
     multiSortBrowse_tbKOrdreHode AT ROW 1.33 COL 34.4 WIDGET-ID 20
     browseconfig_tbKOrdreHode AT ROW 1.33 COL 39 WIDGET-ID 18
     ficPPKode AT ROW 3.14 COL 27 COLON-ALIGNED
     BrwKOrdreHode AT ROW 5.05 COL 2 WIDGET-ID 200
     tbKOrdreHode AT ROW 1.24 COL 1 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 165 BY 22.67 WIDGET-ID 100.


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
         HEIGHT             = 22.62
         WIDTH              = 165.6
         MAX-HEIGHT         = 32.71
         MAX-WIDTH          = 202.8
         VIRTUAL-HEIGHT     = 32.71
         VIRTUAL-WIDTH      = 202.8
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
/* BROWSE-TAB BrwKOrdreHode ficPPKode DEFAULT-FRAME */
ASSIGN 
       BrwKOrdreHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1
       BrwKOrdreHode:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500.

ASSIGN 
       tbKOrdreHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh¤enable,filter;Filter,excel;Eksporter til E&xcel,multiSortBrowse;Sorter på flere kolonner,browseconfig;Column setupmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreHode
/* Query rebuild information for BROWSE BrwKOrdreHode
     _TblList          = "skotex.KOrdreHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.KOrdreHode.DatoTidEndret|no"
     _FldNameList[1]   > "_<CALC>"
"KOrdreHode.DatoTidOpprettet" "DatoTidOpprettet" "99/99/9999 HH:MM:SS.SSS" "datetime" ? ? ? ? ? ? yes "Dato og klokkeslett for opprettelse av ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreHode.Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "Navn eller firmanavn" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreHode.KOrdre_Id" "Ordrenummer" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreHode.kordre_LevStatus" "Leveringsstatus" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreHode.LevFormBeskrivelse" "Leveringsform" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Kort beskrivelse av leveringsform." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreHode.EkstOrdreNr" "Ekst.ordrenr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Ordrenummer fra eksternt system (Importert ordre)" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreHode.Totalt" "Totalt" "->>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Totalt beløp på faktura" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreHode.AntApnet" "Ant. pksdl" ">9" "INTEGER" ? ? ? ? ? ? no "Antall ganger ordren er åpnet etter første utlevering." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreHode.AntPPEti" "Antall etiketter" ">9" "INTEGER" ? ? ? ? ? ? no "Antall postpakke etiketter utskrivet." no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreHode.Leveringsdato" "Leveringsdato" "99/99/99" "DATE" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreHode.SendingsNr" "Sendingsnummer" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Sendingsnummer - for sporing." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreHode.ReturNr" "Retur nr." "x(30)" "CHARACTER" ? ? ? ? ? ? no "Returnr for sporing. Påført returetikett." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreHode.ShipmentSendt" "Shipment sendt" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreHode.MobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KOrdreHode.kordre_Butikk" "Butikk" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KOrdreHode.LevAdresse1" "Leveringsadresse" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Kundens adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreHode.LevPostNr" "Lev. PostNr" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Postnummer" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KOrdreHode.LevPostSted" "Poststed" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Poststed" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KOrdreHode.LevLand" "Lev. Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreHode.LevStatus" "Lev.status" "x(2)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KOrdreHode.cOpt1" "cOpt1" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreHode.LevFNr" "Leveringsform" ">9" "INTEGER" ? ? ? ? ? ? no "Leveringsvorm" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KOrdreHode.ePostAdresse" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"KOrdreHode.Opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Ordre opphav. F.eks. 1-Manuell reg. 2-Nettbutikk." no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"KOrdreHode.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"KOrdreHode.Faktura_Id" "FakturaId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kobling mellom faktura og kundeordre" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"KOrdreHode.kordre_FakturaNr" "FakturaNr" "X(13)" "CHARACTER" ? ? ? ? ? ? no "" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreHode */
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


&Scoped-define SELF-NAME ficPPKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficPPKode C-Win
ON LEAVE OF ficPPKode IN FRAME DEFAULT-FRAME /* Skann postpakke kode */
DO:
    IF TRIM(ficPPKode:SCREEN-VALUE) <> '' THEN 
    DO:
        ASSIGN 
            ficPPKode = ficPPKode:SCREEN-VALUE.
        RUN getKOrdre(ficPPKode).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficPPKode C-Win
ON RETURN OF ficPPKode IN FRAME DEFAULT-FRAME /* Skann postpakke kode */
DO:
    IF TRIM(ficPPKode:SCREEN-VALUE) <> '' THEN 
        RUN getKOrdre(ficPPKode:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwKOrdreHode
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
{incl/conttrigg.i oBrwKOrdreHode:BROWSE-HANDLE}
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
/*IF oBrwKOrdreHode:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:*/
/*  END.                                                       */

  RUN SUPER.
  
  IF oBrwKOrdreHode:isCurrent THEN  
  DO WITH FRAME {&FRAME-NAME}:
      IF AVAILABLE KOrdreHode AND (KOrdreHode.cOpt1 <> '' OR KOrdreHode.LevFNr = 8) THEN
      DO:
          IF (KOrdreHode.cOpt1 <> '' OR KOrdreHode.LevFNr = 8) THEN       
              oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = IF (KOrdreHode.LevFNr = 8 AND KOrdreHode.cOpt1 <> "") THEN
                                                         'Pakken er en gave og skal utleveres i butikk.' /* Rød */
                                                     ELSE IF KOrdreHode.cOpt1 <> "" THEN
                                                         'Pakken skal være en gave.' /* GUL */
                                                     ELSE IF KOrdreHode.LevFNr = 8 THEN
                                                         'Utlevering i butikk.'  /* Grøn */
                                                     ELSE ''.
          ELSE 
              oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = ''.
          
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
  DISPLAY ficPPKode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbKOrdreHode first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode 
         last_tbKOrdreHode refresh_tbKOrdreHode filter_tbKOrdreHode 
         excel_tbKOrdreHode multiSortBrowse_tbKOrdreHode 
         browseconfig_tbKOrdreHode ficPPKode BrwKOrdreHode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKOrdre C-Win 
PROCEDURE getKOrdre :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcSporingsnr AS CHARACTER NO-UNDO.
    
    IF TRIM(pcSporingsNr) <> '' THEN
    DO:
        IF NOT JBoxServerAPI:Instance:CallServerProc("kordrehode_setLevertSpeditor.p", TRIM(pcSporingsnr)) THEN 
            JBoxSession:Instance:ViewMessage("Feil ved start av server prodedure pga. " + JBoxServerAPI:Instance:getCallMessage()).
        ELSE   
            PUBLISH 'OrdrebehandlingOpenQuery'.
    END.
    ficPPKode:SCREEN-VALUE IN FRAME Default-Frame = ''.
    APPLY 'ENTRY' TO ficPPKode IN FRAME Default-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IkkeLevertRecord C-Win 
PROCEDURE IkkeLevertRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for valgt(e) post(er) som 'Ikke levert'?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_IkkeLevert.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for alle poster som levert til speditør?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_IkkeLevert.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:



  oBrwKOrdreHode:baseQuery = "WHERE LevStatus = '47' and Opphav = '10'".
  oBrwKOrdreHode:calcFieldProc = "kordre_brwcalc.p".
  
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
  /* Gjøres her for at også de andre browserne skal oppdateres. Ikke bare i dette programmet. */
  SUBSCRIBE TO 'OrdrebehandlingOpenQuery' ANYWHERE.

  oBrwKOrdreHode = NEW JBoxBrowse(brwKOrdreHode:HANDLE).

  otbKOrdreHode = NEW JBoxToolbar(tbKOrdreHode:HANDLE).

  oBrwKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.
  hKOrdre_IdColumn = oBrwKOrdreHode:getColumnHandle("KOrdre_Id").
  
  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20") THEN
    cNettButikkType = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1").
  
  RUN InitializeComponents.
  
END.
oBrwKOrdreHode:OpenQuery().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE multiSortBrowseRecord C-Win 
PROCEDURE multiSortBrowseRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrdrebehandlingOpenQuery C-Win
PROCEDURE OrdrebehandlingOpenQuery:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    oBrwKOrdreHode:OpenQuery().
    
END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE piLevFnr AS INTEGER     NO-UNDO.
    
    RUN SUPER.
    
    IF AVAILABLE KOrdreHode THEN 
    DO:
        piLevFnr = INT(DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(KOrdreHode.KOrdre_Id),"LevFnr")). 
        
        IF KOrdreHode.cOpt1 <> '' OR KOrdreHode.LevFNr = 8 THEN 
        DO: 
            hKOrdre_IdColumn:BGCOLOR = IF KOrdreHode.cOpt1 = "" THEN 10 /* GUL */ 
                                 ELSE IF KOrdreHode.LevFNr <> 8 THEN 14  /* Grøn */
                                 ELSE 12. /* Rød */
        END.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

