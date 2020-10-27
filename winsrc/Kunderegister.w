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
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cWhere      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cOperator   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE cFeltLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwKunde ***/
DEF VAR oBrwKunde AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Kunde
    FIELD Navn AS CHARACTER
    FIELD KundeNr AS DECIMAL
    FIELD EksterntKundeNr AS CHARACTER
    FIELD MobilTlf AS CHARACTER
    FIELD ePostAdresse AS CHARACTER
    FIELD Kunde_Adresse1 AS CHARACTER
    FIELD Kunde_Adresse2 AS CHARACTER
    FIELD PostNr AS CHARACTER
    FIELD Kunde_PostSted AS CHARACTER
    FIELD Kunde_FaktAdresse1 AS CHARACTER
    FIELD Kunde_FaktAdresse2 AS CHARACTER
    FIELD FaktPostNr AS CHARACTER
    FIELD Kunde_FaktPostSted AS CHARACTER
    FIELD KundeSaldo AS DECIMAL
    FIELD ForsteKjop AS DATE
    FIELD SisteKjop AS DATE
    FIELD Telefon AS CHARACTER
    FIELD ButikkNr AS INTEGER
    FIELD Aktiv AS LOGICAL
    FIELD GruppeId AS INTEGER
    FIELD TypeId AS INTEGER
    FIELD Kunde_HarOrdre AS INTEGER
    FIELD Kilde AS CHARACTER
    FIELD TilgKilde AS CHARACTER
    FIELD OrgNr AS CHARACTER
    FIELD KontNavn AS CHARACTER
    FIELD KontTelefon AS CHARACTER
    FIELD KontMobilTlf AS CHARACTER
    FIELD KontE-Post AS CHARACTER
    FIELD Privat AS LOGICAL
    FIELD RegistrertDato AS DATE
    FIELD EDato AS DATE
    FIELD Land AS CHARACTER
    FIELD LevLand AS CHARACTER
    FIELD FaktLand AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Kunde FOR TEMP-TABLE Kunde.


FUNCTION getBuffersAndFieldsBrwKunde RETURNS CHARACTER():
  RETURN
    'Kunde'
     + ';Navn'
     + ';KundeNr'
     + ';EksterntKundeNr'
     + ';MobilTlf'
     + ';ePostAdresse'
     + ';PostNr'
     + ';FaktPostNr'
     + ';KundeSaldo'
     + ';ForsteKjop'
     + ';SisteKjop'
     + ';Telefon'
     + ';ButikkNr'
     + ';Aktiv'
     + ';GruppeId'
     + ';TypeId'
     + ';Kilde'
     + ';TilgKilde'
     + ';OrgNr'
     + ';KontNavn'
     + ';KontTelefon'
     + ';KontMobilTlf'
     + ';KontE-Post'
     + ';Privat'
     + ';RegistrertDato'
     + ';EDato'
     + ';Land'
     + ';LevLand'
     + ';FaktLand'
     + ';+Kunde_Adresse1|CHARACTER||Kunde_Adresse1(ROWID)|Adresse1'
     + ';+Kunde_Adresse2|CHARACTER||Kunde_Adresse2(ROWID)|Adresse2'
     + ';+Kunde_PostSted|CHARACTER||Kunde_PostSted(ROWID)|PostSted'
     + ';+Kunde_FaktAdresse1|CHARACTER||Kunde_FaktAdresse1(ROWID)|FaktAdresse1'
     + ';+Kunde_FaktAdresse2|CHARACTER||Kunde_FaktAdresse2(ROWID)|FaktAdresse2'
     + ';+Kunde_FaktPostSted|CHARACTER||Kunde_FaktPostSted(ROWID)|FaktPostSted'
     + ';+Kunde_HarOrdre|INTEGER||Kunde_HarOrdre(ROWID)|Har ordre'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKunde RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKunde RETURNS CHARACTER():
  RETURN 
     'kunde_brwcalc.p' /* Kunde_Adresse1(ROWID) */
   + ',kunde_brwcalc.p' /* Kunde_Adresse2(ROWID) */
   + ',kunde_brwcalc.p' /* Kunde_PostSted(ROWID) */
   + ',kunde_brwcalc.p' /* Kunde_FaktAdresse1(ROWID) */
   + ',kunde_brwcalc.p' /* Kunde_FaktAdresse2(ROWID) */
   + ',kunde_brwcalc.p' /* Kunde_FaktPostSted(ROWID) */
   + ',kunde_brwcalc.p' /* Kunde_HarOrdre(ROWID) */
     .
END FUNCTION.
DEF VAR otbKunde AS JBoxToolbar NO-UNDO.


DEF VAR oTabKunde AS JBoxMsTabs NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwKunde

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Kunde

/* Definitions for BROWSE BrwKunde                                      */
&Scoped-define FIELDS-IN-QUERY-BrwKunde Kunde.Navn Kunde.KundeNr ~
Kunde.EksterntKundeNr Kunde.MobilTlf Kunde.ePostAdresse ~
Kunde.Kunde_Adresse1 Kunde.Kunde_Adresse2 Kunde.PostNr Kunde.Kunde_PostSted ~
Kunde.Kunde_FaktAdresse1 Kunde.Kunde_FaktAdresse2 Kunde.FaktPostNr ~
Kunde.Kunde_FaktPostSted Kunde.KundeSaldo Kunde.ForsteKjop Kunde.SisteKjop ~
Kunde.Telefon Kunde.ButikkNr Kunde.Aktiv Kunde.GruppeId Kunde.TypeId ~
Kunde.Kunde_HarOrdre Kunde.Kilde Kunde.TilgKilde Kunde.OrgNr Kunde.KontNavn ~
Kunde.KontTelefon Kunde.KontMobilTlf Kunde.KontE-Post Kunde.Privat ~
Kunde.RegistrertDato Kunde.EDato Kunde.Land Kunde.LevLand Kunde.FaktLand 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKunde Kunde.Navn Kunde.KundeNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwKunde Kunde
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwKunde Kunde
&Scoped-define QUERY-STRING-BrwKunde FOR EACH Kunde NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKunde OPEN QUERY BrwKunde FOR EACH Kunde NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKunde Kunde
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKunde Kunde


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbKunde searchKunde TabKunde first_tbKunde ~
prev_tbKunde next_tbKunde last_tbKunde refresh_tbKunde filter_tbKunde ~
excel_tbKunde rsHarOrdre BrwKunde 
&Scoped-Define DISPLAYED-OBJECTS rsHarOrdre 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON excel_tbKunde 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbKunde 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbKunde 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbKunde 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbKunde 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKunde 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKunde 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE rsHarOrdre AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "<Alle>", 1,
"Har ordre", 2,
"Har åpne ordre", 3
     SIZE 47.8 BY .91 NO-UNDO.

DEFINE RECTANGLE searchKunde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29.2 BY .91.

DEFINE RECTANGLE TabKunde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 137 BY 23.57.

DEFINE RECTANGLE tbKunde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 228.2 BY 1.29.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 1.2 BY 23.1
     BGCOLOR 12 FGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwKunde FOR 
      Kunde SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKunde C-Win _STRUCTURED
  QUERY BrwKunde NO-LOCK DISPLAY
      Kunde.Navn COLUMN-LABEL "Navn" FORMAT "X(40)":U
      Kunde.KundeNr COLUMN-LABEL "KundeNr" FORMAT ">>>>>>>>>>>>9":U
      Kunde.EksterntKundeNr COLUMN-LABEL "Ekst.kndnr" FORMAT "X(20)":U
      Kunde.MobilTlf COLUMN-LABEL "Mobiltelefon" FORMAT "X(15)":U
      Kunde.ePostAdresse COLUMN-LABEL "E-Post" FORMAT "X(40)":U
      Kunde.Kunde_Adresse1 COLUMN-LABEL "Adresse1" FORMAT "X(30)":U
      Kunde.Kunde_Adresse2 COLUMN-LABEL "Adresse2" FORMAT "X(30)":U
      Kunde.PostNr COLUMN-LABEL "PostNr" FORMAT "X(10)":U
      Kunde.Kunde_PostSted COLUMN-LABEL "PostSted" FORMAT "X(20)":U
      Kunde.Kunde_FaktAdresse1 COLUMN-LABEL "FaktAdresse1" FORMAT "X(30)":U
      Kunde.Kunde_FaktAdresse2 COLUMN-LABEL "FaktAdresse2" FORMAT "X(30)":U
      Kunde.FaktPostNr COLUMN-LABEL "Postnr" FORMAT "X(15)":U
      Kunde.Kunde_FaktPostSted COLUMN-LABEL "FaktPostSted" FORMAT "X(20)":U
      Kunde.KundeSaldo COLUMN-LABEL "Saldo" FORMAT "->,>>>,>>9.99":U
      Kunde.ForsteKjop COLUMN-LABEL "Første kjøp" FORMAT "99/99/99":U
      Kunde.SisteKjop COLUMN-LABEL "Siste kjøp" FORMAT "99/99/99":U
      Kunde.Telefon COLUMN-LABEL "Telefon" FORMAT "X(15)":U
      Kunde.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      Kunde.Aktiv COLUMN-LABEL "Aktiv" FORMAT "*/":U
      Kunde.GruppeId COLUMN-LABEL "Grp" FORMAT ">>>9":U
      Kunde.TypeId COLUMN-LABEL "Type" FORMAT ">>>9":U
      Kunde.Kunde_HarOrdre COLUMN-LABEL "Har ordre" FORMAT "9":U
      Kunde.Kilde COLUMN-LABEL "Kilde" FORMAT "X(30)":U
      Kunde.TilgKilde COLUMN-LABEL "Tilg.kilde" FORMAT "X(30)":U
      Kunde.OrgNr COLUMN-LABEL "OrgNr" FORMAT "X(15)":U
      Kunde.KontNavn COLUMN-LABEL "Kontaktperson" FORMAT "X(40)":U
      Kunde.KontTelefon COLUMN-LABEL "Telefon" FORMAT "X(15)":U
      Kunde.KontMobilTlf COLUMN-LABEL "Mobiltelefon" FORMAT "X(15)":U
      Kunde.KontE-Post COLUMN-LABEL "E-Post" FORMAT "X(40)":U
      Kunde.Privat COLUMN-LABEL "Privat" FORMAT "yes/no":U
      Kunde.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      Kunde.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      Kunde.Land COLUMN-LABEL "Land" FORMAT "X(30)":U
      Kunde.LevLand COLUMN-LABEL "Land" FORMAT "X(30)":U
      Kunde.FaktLand COLUMN-LABEL "Land" FORMAT "X(30)":U
  ENABLE
      Kunde.Navn HELP "Navn eller firmanavn"
      Kunde.KundeNr HELP "Kundenummer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83.6 BY 23.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbKunde AT ROW 1.29 COL 2 WIDGET-ID 4
     prev_tbKunde AT ROW 1.29 COL 6.8 WIDGET-ID 6
     next_tbKunde AT ROW 1.29 COL 11.4 WIDGET-ID 8
     last_tbKunde AT ROW 1.29 COL 16 WIDGET-ID 10
     refresh_tbKunde AT ROW 1.29 COL 20.6 WIDGET-ID 20
     filter_tbKunde AT ROW 1.29 COL 25.2 WIDGET-ID 22
     excel_tbKunde AT ROW 1.29 COL 29.8 WIDGET-ID 24
     rsHarOrdre AT ROW 2.57 COL 41.2 NO-LABEL WIDGET-ID 26
     BrwKunde AT ROW 3.86 COL 3 WIDGET-ID 200
     tbKunde AT ROW 1.19 COL 1.8 WIDGET-ID 2
     searchKunde AT ROW 2.62 COL 2.4 WIDGET-ID 16
     TabKunde AT ROW 3.86 COL 90 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 230.2 BY 26.71 WIDGET-ID 100.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1 COL 24.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 64 ROW 4.1
         SIZE 46 BY 23.1 WIDGET-ID 300.


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
         HEIGHT             = 26.86
         WIDTH              = 229.2
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 236
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 236
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
/* BROWSE-TAB BrwKunde rsHarOrdre DEFAULT-FRAME */
ASSIGN 
       tbKunde:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcelmaxborder".

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKunde
/* Query rebuild information for BROWSE BrwKunde
     _TblList          = "SkoTex.Kunde"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Kunde.Navn
"Kunde.Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "Navn eller firmanavn" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Kunde.KundeNr
"Kunde.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? yes "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Kunde.EksterntKundeNr
"Kunde.EksterntKundeNr" "Ekst.kndnr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Eksternt kundenummer (Fra f.eks fakturasystem)" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Kunde.MobilTlf
"Kunde.MobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Kunde.ePostAdresse
"Kunde.ePostAdresse" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Kunde.Kunde_Adresse1" "Adresse1" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Kunde.Kunde_Adresse2" "Adresse2" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Kunde.PostNr
"Kunde.PostNr" "PostNr" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Postnummer" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Kunde.Kunde_PostSted" "PostSted" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"Kunde.Kunde_FaktAdresse1" "FaktAdresse1" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Kunde.Kunde_FaktAdresse2" "FaktAdresse2" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Kunde.FaktPostNr
"Kunde.FaktPostNr" "Postnr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Postnr. fakturaadresse." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"Kunde.Kunde_FaktPostSted" "FaktPostSted" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Kunde.KundeSaldo
"Kunde.KundeSaldo" "Saldo" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Kundens utestående saldo" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Kunde.ForsteKjop
"Kunde.ForsteKjop" "Første kjøp" "99/99/99" "DATE" ? ? ? ? ? ? no "" no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Kunde.SisteKjop
"Kunde.SisteKjop" "Siste kjøp" "99/99/99" "DATE" ? ? ? ? ? ? no "Siste gang kunden kjøpte" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Kunde.Telefon
"Kunde.Telefon" "Telefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Telefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Kunde.ButikkNr
"Kunde.ButikkNr" "Butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk som rekrutterte kunden" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Kunde.Aktiv
"Kunde.Aktiv" "Aktiv" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Aktiv kunde" no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Kunde.GruppeId
"Kunde.GruppeId" "Grp" ">>>9" "INTEGER" ? ? ? ? ? ? no "Kundegruppe (Bedriftskunde, privatkunde o.l.)" no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Kunde.TypeId
"Kunde.TypeId" "Type" ">>>9" "INTEGER" ? ? ? ? ? ? no "Kundetype (Internkunde - ansatt, eksternkunde)" no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"Kunde.Kunde_HarOrdre" "Har ordre" "9" "INTEGER" ? ? ? ? ? ? no "" no no "8.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > Kunde.Kilde
"Kunde.Kilde" "Kilde" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Hvor kommer kunden fra." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > Kunde.TilgKilde
"Kunde.TilgKilde" "Tilg.kilde" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Hvilken tilknyttning har kunden." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > Kunde.OrgNr
"Kunde.OrgNr" "OrgNr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Kundens organisasjonsnummer" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > Kunde.KontNavn
"Kunde.KontNavn" "Kontaktperson" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Navn på kontaktperson" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > Kunde.KontTelefon
"Kunde.KontTelefon" "Telefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Telefon direkte til kontaktperson" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > Kunde.KontMobilTlf
"Kunde.KontMobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Kontaktpersonens mobiltelefonnummer" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > Kunde.KontE-Post
"Kunde.KontE-Post" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > Kunde.Privat
"Kunde.Privat" "Privat" "yes/no" "LOGICAL" ? ? ? ? ? ? no "Privatkunde" no no "5.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > Kunde.RegistrertDato
"Kunde.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > Kunde.EDato
"Kunde.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > Kunde.Land
"Kunde.Land" "Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > Kunde.LevLand
"Kunde.LevLand" "Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > Kunde.FaktLand
"Kunde.FaktLand" "Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKunde */
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
&Scoped-define SELF-NAME rsHarOrdre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsHarOrdre C-Win
ON VALUE-CHANGED OF rsHarOrdre IN FRAME DEFAULT-FRAME
DO:
  RUN settFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwKunde
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
{incl/conttrigg.i oBrwKunde:BROWSE-HANDLE}
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
  DISPLAY rsHarOrdre 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbKunde searchKunde TabKunde first_tbKunde prev_tbKunde next_tbKunde 
         last_tbKunde refresh_tbKunde filter_tbKunde excel_tbKunde rsHarOrdre 
         BrwKunde 
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
  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 2") THEN
    iButNr = INT(JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1")).

  oBrwKunde = NEW JBoxBrowse(brwKunde:HANDLE).

  otbKunde = NEW JBoxToolbar(tbKunde:HANDLE).

  oBrwKunde:TOOLBAR-OBJECT = otbKunde.
  oBrwKunde:setSearchField(searchKunde:HANDLE,"Navn").
/*  oBrwKunde:baseQuery = "WHERE ButikkNr = '" + STRING(iButNr) + "'".*/
  oBrwKunde:baseQuery = "WHERE WebKunde = TRUE".
  otbKunde = NEW JBoxToolbar(tbKunde:HANDLE).
  
  oTabKunde = NEW JBoxMsTabs(TabKunde:HANDLE,oBrwKunde).
  oTabKunde:pageOneType = "oneToOne".

  oTabKunde:setLinkFields("KundeNr").
  oTabKunde:AddPage("Detalj","KundeDetalj.w").
  oTabKunde:AddPage("Kundens ordre","KundesOrdre.w").
  
  oContainer:setSplitBarX(btnSplitBarX:HANDLE IN FRAME frSplitBarX).
  oContainer:setFollowSplitBarX(STRING(BrwKunde:HANDLE) + ',' + STRING(TabKunde:HANDLE)).
  oContainer:setNoResizeX("BrwKunde").
  
  ASSIGN 
    rsHarOrdre = 1
    rsHarOrdre:SCREEN-VALUE = STRING(rsHarOrdre)
    .
  DISPLAY 
    rsHarOrdre
    .
  /* Gjør OpenQuery med filter satt. */
  RUN settFilter.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settFilter C-Win 
PROCEDURE settFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
    cFeltLst  = 'Kunde_HarOrdre' 
    cWhere    = rsHarOrdre:SCREEN-VALUE
    cOperator = IF INT(rsHarOrdre:SCREEN-VALUE) = 1 THEN '>=' ELSE 
                IF INT(rsHarOrdre:SCREEN-VALUE) = 2 THEN '>=' ELSE '='
    .

  oBrwKunde:setFilter(cFeltLst,cOperator,cWhere).
  oBrwKunde:OpenQuery().
END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

