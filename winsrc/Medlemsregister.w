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


/*** Start instance property definitions for JBoxBrowse object oBrwMedlem ***/
DEF VAR oBrwMedlem AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE Medlem
    FIELD EksterntMedlemsNr AS CHARACTER
    FIELD ForNavn AS CHARACTER
    FIELD EtterNavn AS CHARACTER
    FIELD MobilTlf AS CHARACTER
    FIELD ePostAdresse AS CHARACTER
    FIELD ButikkNr AS INTEGER
    FIELD MottaeMailUtsendelser AS LOGICAL
    FIELD MottaeSMSUtsendelser AS LOGICAL
    FIELD AktivertFraWeb AS DATE
    FIELD Aktiv AS LOGICAL
    FIELD MedlemsNr AS DECIMAL
    FIELD Kjonn AS LOGICAL
    FIELD Kilde AS CHARACTER
    FIELD FodselsDato AS DATE
    FIELD FodtAr AS INTEGER
    FIELD KundeNr AS DECIMAL
    FIELD MKlubbId AS INTEGER
    FIELD Opphort AS DATE
    FIELD Adresse1 AS CHARACTER
    FIELD Adresse2 AS CHARACTER
    FIELD PostNr AS CHARACTER
    FIELD Land AS CHARACTER
    FIELD Telefaks AS CHARACTER
    FIELD Telefon AS CHARACTER
    FIELD MedGruppe AS INTEGER
    FIELD MedType AS INTEGER
    FIELD RegKode AS CHARACTER
    FIELD TilgKilde AS CHARACTER
    FIELD WebBrukerId AS CHARACTER
    FIELD WebPassord AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_Medlem FOR TEMP-TABLE Medlem.


FUNCTION getBuffersAndFieldsBrwMedlem RETURNS CHARACTER():
  RETURN
    'Medlem'
     + ';EksterntMedlemsNr'
     + ';ForNavn'
     + ';EtterNavn'
     + ';MobilTlf'
     + ';ePostAdresse'
     + ';ButikkNr'
     + ';MottaeMailUtsendelser'
     + ';MottaeSMSUtsendelser'
     + ';AktivertFraWeb'
     + ';Aktiv'
     + ';MedlemsNr'
     + ';Kjonn'
     + ';Kilde'
     + ';FodselsDato'
     + ';FodtAr'
     + ';KundeNr'
     + ';MKlubbId'
     + ';Opphort'
     + ';Adresse1'
     + ';Adresse2'
     + ';PostNr'
     + ';Land'
     + ';Telefaks'
     + ';Telefon'
     + ';MedGruppe'
     + ';MedType'
     + ';RegKode'
     + ';TilgKilde'
     + ';WebBrukerId'
     + ';WebPassord'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwMedlem RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
DEF VAR otbMedlem AS JBoxToolbar NO-UNDO.


DEF VAR oTabMedlem AS JBoxMsTabs NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwMedlem

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Medlem

/* Definitions for BROWSE BrwMedlem                                     */
&Scoped-define FIELDS-IN-QUERY-BrwMedlem Medlem.EksterntMedlemsNr ~
Medlem.ForNavn Medlem.EtterNavn Medlem.MobilTlf Medlem.ePostAdresse ~
Medlem.ButikkNr Medlem.MottaeMailUtsendelser Medlem.MottaeSMSUtsendelser ~
Medlem.AktivertFraWeb Medlem.Aktiv Medlem.MedlemsNr Medlem.Kjonn ~
Medlem.Kilde Medlem.FodselsDato Medlem.FodtAr Medlem.KundeNr ~
Medlem.MKlubbId Medlem.Opphort Medlem.Adresse1 Medlem.Adresse2 ~
Medlem.PostNr Medlem.Land Medlem.Telefaks Medlem.Telefon Medlem.MedGruppe ~
Medlem.MedType Medlem.RegKode Medlem.TilgKilde Medlem.WebBrukerId ~
Medlem.WebPassord 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwMedlem Medlem.EksterntMedlemsNr 
&Scoped-define QUERY-STRING-BrwMedlem FOR EACH Medlem NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwMedlem OPEN QUERY BrwMedlem FOR EACH Medlem NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwMedlem Medlem
&Scoped-define FIRST-TABLE-IN-QUERY-BrwMedlem Medlem


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbMedlem TabMedlem first_tbMedlem ~
prev_tbMedlem next_tbMedlem last_tbMedlem refresh_tbMedlem filter_tbMedlem ~
browseconfig_tbMedlem excel_tbMedlem close_tbMedlem BrwMedlem 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbMedlem 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Column setup" 
     SIZE 4.6 BY 1.1 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON close_tbMedlem 
     IMAGE-UP FILE "bmp/e-exit.bmp":U
     LABEL "Close" 
     SIZE 4.6 BY 1.1 TOOLTIP "Close (ALT-C)".

DEFINE BUTTON excel_tbMedlem 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbMedlem 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbMedlem 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbMedlem 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbMedlem 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbMedlem 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbMedlem 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE RECTANGLE TabMedlem
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 19.29.

DEFINE RECTANGLE tbMedlem
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 161.2 BY 1.29.

DEFINE BUTTON btnSplitBarX 
     IMAGE-UP FILE "bmp/tabright.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE .8 BY 11.43
     BGCOLOR 12 FGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwMedlem FOR 
      Medlem SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwMedlem C-Win _STRUCTURED
  QUERY BrwMedlem NO-LOCK DISPLAY
      Medlem.EksterntMedlemsNr COLUMN-LABEL "Ekst.medl.nr" FORMAT "X(40)":U
            WIDTH 10
      Medlem.ForNavn COLUMN-LABEL "Fornavn" FORMAT "X(40)":U WIDTH 20
      Medlem.EtterNavn COLUMN-LABEL "Etternavn" FORMAT "X(40)":U
            WIDTH 30
      Medlem.MobilTlf COLUMN-LABEL "Mobiltelefon" FORMAT "X(15)":U
      Medlem.ePostAdresse COLUMN-LABEL "E-Post" FORMAT "X(40)":U
            WIDTH 20
      Medlem.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      Medlem.MottaeMailUtsendelser COLUMN-LABEL "Mail" FORMAT "*/":U
      Medlem.MottaeSMSUtsendelser COLUMN-LABEL "SMS" FORMAT "*/":U
      Medlem.AktivertFraWeb COLUMN-LABEL "Medlem fra" FORMAT "99/99/9999":U
            WIDTH 15.4
      Medlem.Aktiv COLUMN-LABEL "Aktiv" FORMAT "*/":U
      Medlem.MedlemsNr COLUMN-LABEL "MedlemNr" FORMAT ">>>>>>>>>>>>9":U
      Medlem.Kjonn COLUMN-LABEL "Kjønn" FORMAT "M/K":U
      Medlem.Kilde COLUMN-LABEL "Kilde" FORMAT "X(30)":U WIDTH 10
      Medlem.FodselsDato COLUMN-LABEL "Fødselsdato" FORMAT "99/99/9999":U
      Medlem.FodtAr COLUMN-LABEL "F.år" FORMAT "9999":U
      Medlem.KundeNr COLUMN-LABEL "KundeNr" FORMAT ">>>>>>>>>>>>9":U
      Medlem.MKlubbId COLUMN-LABEL "Klubbnr" FORMAT ">>9":U
      Medlem.Opphort COLUMN-LABEL "Opphørt" FORMAT "99/99/9999":U
      Medlem.Adresse1 COLUMN-LABEL "Adresse1" FORMAT "X(40)":U
            WIDTH 30
      Medlem.Adresse2 COLUMN-LABEL "Adresse2" FORMAT "X(40)":U
            WIDTH 30
      Medlem.PostNr COLUMN-LABEL "PostNr" FORMAT "X(10)":U
      Medlem.Land COLUMN-LABEL "Land" FORMAT "X(30)":U WIDTH 20
      Medlem.Telefaks COLUMN-LABEL "Telefaks" FORMAT "X(15)":U
      Medlem.Telefon COLUMN-LABEL "Telefon" FORMAT "X(15)":U
      Medlem.MedGruppe COLUMN-LABEL "Medlemsgruppe" FORMAT "zzz9":U
      Medlem.MedType COLUMN-LABEL "Medlemstype" FORMAT "zzz9":U
      Medlem.RegKode COLUMN-LABEL "RegKode" FORMAT "X(10)":U
      Medlem.TilgKilde COLUMN-LABEL "Tilg.kilde" FORMAT "X(30)":U
      Medlem.WebBrukerId COLUMN-LABEL "Brukerid (Web)" FORMAT "X(15)":U
      Medlem.WebPassord COLUMN-LABEL "Passord (Web)" FORMAT "X(15)":U
  ENABLE
      Medlem.EksterntMedlemsNr HELP "Eksternt medlemsnr (Fra f.eks. annet medlemssystem)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 69 BY 19.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbMedlem AT ROW 1.29 COL 2 WIDGET-ID 4
     prev_tbMedlem AT ROW 1.29 COL 6.6 WIDGET-ID 6
     next_tbMedlem AT ROW 1.29 COL 11.4 WIDGET-ID 8
     last_tbMedlem AT ROW 1.29 COL 16 WIDGET-ID 10
     refresh_tbMedlem AT ROW 1.29 COL 20.8 WIDGET-ID 12
     filter_tbMedlem AT ROW 1.29 COL 25.4 WIDGET-ID 14
     browseconfig_tbMedlem AT ROW 1.29 COL 30.2 WIDGET-ID 16
     excel_tbMedlem AT ROW 1.29 COL 34.8 WIDGET-ID 18
     close_tbMedlem AT ROW 1.29 COL 39.6 WIDGET-ID 20
     BrwMedlem AT ROW 2.76 COL 2 WIDGET-ID 200
     tbMedlem AT ROW 1.19 COL 1.8 WIDGET-ID 2
     TabMedlem AT ROW 2.91 COL 74 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 163.2 BY 21.57 WIDGET-ID 100.

DEFINE FRAME frSplitBarX
     btnSplitBarX AT ROW 1.19 COL 25.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 47.4 ROW 6.48
         SIZE 34.2 BY 11.67 WIDGET-ID 300.


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
         HEIGHT             = 21.57
         WIDTH              = 163.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 163.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 163.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
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
/* BROWSE-TAB BrwMedlem close_tbMedlem DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 21.57
       FRAME DEFAULT-FRAME:WIDTH            = 163.2.

ASSIGN 
       tbMedlem:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,browseconfig;Column setup,excel;Eksporter til E&xcel,close;Closemaxborder".

/* SETTINGS FOR FRAME frSplitBarX
                                                                        */
ASSIGN 
       btnSplitBarX:MOVABLE IN FRAME frSplitBarX          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwMedlem
/* Query rebuild information for BROWSE BrwMedlem
     _TblList          = "SkoTex.Medlem"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"Medlem.EksterntMedlemsNr" "Ekst.medl.nr" "X(40)" "character" ? ? ? ? ? ? yes "Eksternt medlemsnr (Fra f.eks. annet medlemssystem)" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"Medlem.ForNavn" "Fornavn" "X(40)" "character" ? ? ? ? ? ? no "Medlemmets fornavn og mellomnavn" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"Medlem.EtterNavn" "Etternavn" "X(40)" "character" ? ? ? ? ? ? no "Medlemmets etternavn" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"Medlem.MobilTlf" "Mobiltelefon" "X(15)" "character" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"Medlem.ePostAdresse" "E-Post" "X(40)" "character" ? ? ? ? ? ? no "E-Post adresse" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Medlem.ButikkNr" "Butikk" ">>>>>9" "integer" ? ? ? ? ? ? no "Butikk som rekrutterte medlemmet" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"Medlem.MottaeMailUtsendelser" "Mail" "*~~/" "logical" ? ? ? ? ? ? no "Ønsker å motta utsendelser på mail" no no "3.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"Medlem.MottaeSMSUtsendelser" "SMS" "*~~/" "logical" ? ? ? ? ? ? no "" no no "4.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"Medlem.AktivertFraWeb" "Medlem fra" "99/99/9999" "date" ? ? ? ? ? ? no "Dato da medlemmet er aktivert fra medlemsweb'en" no no "15.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"Medlem.Aktiv" "Aktiv" "*~~/" "logical" ? ? ? ? ? ? no "Aktiverer medlem i kasse." no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"Medlem.MedlemsNr" "MedlemNr" ">>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? no "Medlemsnummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"Medlem.Kjonn" "Kjønn" "M/K" "logical" ? ? ? ? ? ? no "Kjønn" no no "5.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"Medlem.Kilde" "Kilde" "X(30)" "character" ? ? ? ? ? ? no "Hvor kommer kunden fra." no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"Medlem.FodselsDato" "Fødselsdato" "99/99/9999" "date" ? ? ? ? ? ? no "Fødselsdato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"Medlem.FodtAr" "F.år" "9999" "integer" ? ? ? ? ? ? no "Fødselsår kan oppgis hvis dato er ukjent" no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"Medlem.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "decimal" ? ? ? ? ? ? no "Koblet til kunde" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"Medlem.MKlubbId" "Klubbnr" ">>9" "integer" ? ? ? ? ? ? no "" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"Medlem.Opphort" "Opphørt" "99/99/9999" "date" ? ? ? ? ? ? no "Medlemmet er meldt ut av medlemsregisteret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"Medlem.Adresse1" "Adresse1" "X(40)" "character" ? ? ? ? ? ? no "Medlemets adresse" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"Medlem.Adresse2" "Adresse2" "X(40)" "character" ? ? ? ? ? ? no "Medlemmets adresse" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"Medlem.PostNr" "PostNr" "X(10)" "character" ? ? ? ? ? ? no "Postnummer" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"Medlem.Land" "Land" "X(30)" "character" ? ? ? ? ? ? no "Land" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"Medlem.Telefaks" "Telefaks" "X(15)" "character" ? ? ? ? ? ? no "Telefaks" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"Medlem.Telefon" "Telefon" "X(15)" "character" ? ? ? ? ? ? no "Telefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"Medlem.MedGruppe" "Medlemsgruppe" "zzz9" "integer" ? ? ? ? ? ? no "Medlemsgruppe" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"Medlem.MedType" "Medlemstype" "zzz9" "integer" ? ? ? ? ? ? no "Medlemstype" no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"Medlem.RegKode" "RegKode" "X(10)" "character" ? ? ? ? ? ? no "Regionskode" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"Medlem.TilgKilde" "Tilg.kilde" "X(30)" "character" ? ? ? ? ? ? no "Hvilken tilknyttning har kunden." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"Medlem.WebBrukerId" "Brukerid (Web)" "X(15)" "character" ? ? ? ? ? ? no "Medlemmets brukerid på web." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"Medlem.WebPassord" "Passord (Web)" "X(15)" "character" ? ? ? ? ? ? no "Brukerens passord på web." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwMedlem */
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
&Scoped-define BROWSE-NAME BrwMedlem
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
{incl/conttrigg.i oBrwMedlem:BROWSE-HANDLE}
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
  ENABLE tbMedlem TabMedlem first_tbMedlem prev_tbMedlem next_tbMedlem 
         last_tbMedlem refresh_tbMedlem filter_tbMedlem browseconfig_tbMedlem 
         excel_tbMedlem close_tbMedlem BrwMedlem 
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

  oBrwMedlem = NEW JBoxBrowse(brwMedlem:HANDLE).

  otbMedlem = NEW JBoxToolbar(tbMedlem:HANDLE).

  oBrwMedlem:TOOLBAR-OBJECT = otbMedlem.
  
  oContainer:setSplitBarX(btnSplitBarX:HANDLE IN FRAME frSplitBarX).
  oContainer:setFollowSplitBarX(STRING(BrwMedlem:HANDLE)).
  oContainer:setNoResizeX("BrwMedlem").
  
  
/*  oContainer:setSplitBarX(btnSplitBarX:HANDLE IN FRAME frSplitBarX).                     */
/*  oContainer:setFollowSplitBarX(STRING(BrwKunde:HANDLE) + ',' + STRING(TabKunde:HANDLE)).*/
/*  oContainer:setNoResizeX("BrwKunde").                                                   */
  
  
  oTabMedlem = NEW JBoxMsTabs(TabMedlem:HANDLE,oBrwMedlem).
  oTabMedlem:pageOneType = "oneToOne".

  oTabMedlem:setLinkFields("MedlemsNr").
  oTabMedlem:AddPage("Medlemsinformasjon","Medlemsregister.w").
END.
oBrwMedlem:OpenQuery().

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

