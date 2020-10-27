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
DEFINE INPUT PARAMETER lRefKOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER icModus AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEF VAR opopupVare AS JBoxPopupMenu NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreLinje ***/
DEF VAR oBrwKOrdreLinje AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreLinje
    FIELD KOrdreLinjeNr AS INTEGER
    FIELD KopiKOrdreLinjeNr AS INTEGER
    FIELD Aktiv AS LOGICAL
    FIELD Returnert AS LOGICAL
    FIELD VareNr AS CHARACTER
    FIELD Varetekst AS CHARACTER
    FIELD kordrelinje_LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD Storl AS CHARACTER
    FIELD Kode AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD NettoPris AS DECIMAL
    FIELD Linjesum AS DECIMAL
    FIELD BetRef AS CHARACTER
    FIELD Notat AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_KOrdreLinje FOR TEMP-TABLE KOrdreLinje.


FUNCTION getBuffersAndFieldsBrwKOrdreLinje RETURNS CHARACTER():
  RETURN
    'KOrdreLinje'
     + ';KOrdreLinjeNr'
     + ';KopiKOrdreLinjeNr'
     + ';Aktiv'
     + ';Returnert'
     + ';VareNr'
     + ';Varetekst'
     + ';LevFargKod'
     + ';Storl'
     + ';Kode'
     + ';Antall'
     + ';NettoPris'
     + ';Linjesum'
     + ';BetRef'
     + ';Notat'
     + ';+kordrelinje_LevKod|CHARACTER||kordrelinje_LevKod|Lev.kod'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreLinje RETURNS CHARACTER():
  RETURN 
     'server/kordrelinje_brwcalc.p' /* kordrelinje_LevKod */
     .
END FUNCTION.
DEF VAR otbKOrdreLinje AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwKOrdreLinje

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KOrdreLinje

/* Definitions for BROWSE BrwKOrdreLinje                                */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdreLinjeNr ~
KOrdreLinje.KopiKOrdreLinjeNr KOrdreLinje.Aktiv KOrdreLinje.Returnert ~
KOrdreLinje.VareNr KOrdreLinje.Varetekst KOrdreLinje.kordrelinje_LevKod ~
KOrdreLinje.LevFargKod KOrdreLinje.Storl KOrdreLinje.Kode ~
KOrdreLinje.Antall KOrdreLinje.NettoPris KOrdreLinje.Linjesum ~
KOrdreLinje.BetRef KOrdreLinje.Notat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreLinje ~
KOrdreLinje.KOrdreLinjeNr KOrdreLinje.Antall 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define QUERY-STRING-BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreLinje OPEN QUERY BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrwKOrdreLinje first_tbKOrdreLinje ~
last_tbKOrdreLinje next_tbKOrdreLinje prev_tbKOrdreLinje ~
refresh_tbKOrdreLinje BtnOK BtnDone fcStrekkode fiReturKodeId ~
btnReklamasjonskode RECT-5 RECT-6 tbKOrdreLinje 
&Scoped-Define DISPLAYED-OBJECTS EkstOrdreNr KOrdre_Id Navn fcBeskr ~
fcStrekkode fiReturKodeId fcReturKodeTekst fiTekst fiTekst-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14 TOOLTIP "Avslutter registrering av varelinjer for retur."
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnReklamasjonskode 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON first_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKOrdreLinje 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE EkstOrdreNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekst.ordrenr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 19 BY 1 TOOLTIP "Ordrenr. fra nettbutikk eller annet eksternt system." NO-UNDO.

DEFINE VARIABLE fcBeskr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 54.4 BY 1 TOOLTIP "Varetekst." NO-UNDO.

DEFINE VARIABLE fcReturKodeTekst AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 62.8 BY 1 TOOLTIP "Kort beskrivelse av returkoden." NO-UNDO.

DEFINE VARIABLE fcStrekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 TOOLTIP "Strekkode på vare som skal returneres." NO-UNDO.

DEFINE VARIABLE fiReturKodeId AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Returkode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 6.8 BY 1 TOOLTIP "Returkode for retunert vare. Returkode må angis." NO-UNDO.

DEFINE VARIABLE fiTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Registrer retur" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE fiTekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Ordre som returneres" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kordre id" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kunde" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 46.8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185.8 BY 3.57.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 185.8 BY 3.1.

DEFINE RECTANGLE tbKOrdreLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 91 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwKOrdreLinje FOR 
      KOrdreLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwKOrdreLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreLinje C-Win _STRUCTURED
  QUERY BrwKOrdreLinje NO-LOCK DISPLAY
      KOrdreLinje.KOrdreLinjeNr COLUMN-LABEL "Linje" FORMAT ">>>>>>9":U
            WIDTH 11
      KOrdreLinje.KopiKOrdreLinjeNr COLUMN-LABEL "Kopi" FORMAT ">>>>>>9":U
      KOrdreLinje.Aktiv COLUMN-LABEL "Akt" FORMAT "*/":U
      KOrdreLinje.Returnert COLUMN-LABEL "Ret" FORMAT "*/":U WIDTH 3.2
      KOrdreLinje.VareNr COLUMN-LABEL "VareNr" FORMAT "X(20)":U
      KOrdreLinje.Varetekst COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      KOrdreLinje.kordrelinje_LevKod COLUMN-LABEL "Lev.kod" FORMAT "X(20)":U
      KOrdreLinje.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(15)":U
      KOrdreLinje.Storl COLUMN-LABEL "Str" FORMAT "x(10)":U
      KOrdreLinje.Kode FORMAT "X(20)":U
      KOrdreLinje.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9":U
      KOrdreLinje.NettoPris COLUMN-LABEL "Nettopris" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Linjesum COLUMN-LABEL "Linjesum" FORMAT "->>>,>>9.99":U
      KOrdreLinje.BetRef COLUMN-LABEL "Bet.ref." FORMAT "x(20)":U
      KOrdreLinje.Notat COLUMN-LABEL "Notat" FORMAT "X(40)":U WIDTH 40
  ENABLE
      KOrdreLinje.KOrdreLinjeNr HELP "Linjenummer på faktura"
      KOrdreLinje.Antall HELP "Antall"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 186 BY 8.48 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwKOrdreLinje AT ROW 6.1 COL 3 WIDGET-ID 200
     EkstOrdreNr AT ROW 3.05 COL 15.2 COLON-ALIGNED
     first_tbKOrdreLinje AT ROW 4.76 COL 3.4 WIDGET-ID 44
     KOrdre_Id AT ROW 1.95 COL 15.2 COLON-ALIGNED
     last_tbKOrdreLinje AT ROW 4.76 COL 17.4 WIDGET-ID 50
     Navn AT ROW 1.95 COL 42.2 COLON-ALIGNED
     next_tbKOrdreLinje AT ROW 4.76 COL 12.8 WIDGET-ID 48
     prev_tbKOrdreLinje AT ROW 4.76 COL 8 WIDGET-ID 46
     refresh_tbKOrdreLinje AT ROW 4.76 COL 22.2 WIDGET-ID 52
     BtnOK AT ROW 19.29 COL 3 WIDGET-ID 30
     BtnDone AT ROW 19.29 COL 173.6 WIDGET-ID 28
     fcBeskr AT ROW 16 COL 34.6 COLON-ALIGNED NO-LABEL
     fcStrekkode AT ROW 16 COL 15.2 COLON-ALIGNED
     fiReturKodeId AT ROW 17.14 COL 15.4 COLON-ALIGNED
     btnReklamasjonskode AT ROW 17.14 COL 24.2 WIDGET-ID 24
     fcReturKodeTekst AT ROW 17.14 COL 26.2 COLON-ALIGNED NO-LABEL
     fiTekst AT ROW 15.05 COL 3.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiTekst-2 AT ROW 1.14 COL 4 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     RECT-5 AT ROW 15.52 COL 3.2 WIDGET-ID 32
     RECT-6 AT ROW 1.48 COL 3.2 WIDGET-ID 38
     tbKOrdreLinje AT ROW 4.67 COL 3.2 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 188.6 BY 19.67
         DEFAULT-BUTTON BtnOK WIDGET-ID 100.


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
         TITLE              = "Registrer varelinjer på retur ordre."
         HEIGHT             = 19.67
         WIDTH              = 188.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 188.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 188.6
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BrwKOrdreLinje 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 19.67
       FRAME DEFAULT-FRAME:WIDTH            = 188.6.

/* SETTINGS FOR FILL-IN EkstOrdreNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fcBeskr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fcBeskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fcReturKodeTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTekst:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Registrer retur".

/* SETTINGS FOR FILL-IN fiTekst-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTekst-2:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Ordre som returneres".

/* SETTINGS FOR FILL-IN KOrdre_Id IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbKOrdreLinje:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refreshmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreLinje
/* Query rebuild information for BROWSE BrwKOrdreLinje
     _TblList          = "SkoTex.KOrdreLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > KOrdreLinje.KOrdreLinjeNr
"KOrdreLinje.KOrdreLinjeNr" "Linje" ">>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Linjenummer på faktura" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > KOrdreLinje.KopiKOrdreLinjeNr
"KOrdreLinje.KopiKOrdreLinjeNr" "Kopi" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Linjkepeker som peker på rad kopiert til eller fra." no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > KOrdreLinje.Aktiv
"KOrdreLinje.Aktiv" "Akt" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Viser om raden er aktiv og skal tas med på faktura o.l." no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > KOrdreLinje.Returnert
"KOrdreLinje.Returnert" "Ret" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Ordrelinje er returnert fra kunde." no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > KOrdreLinje.VareNr
"KOrdreLinje.VareNr" "VareNr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Varenummer" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > KOrdreLinje.Varetekst
"KOrdreLinje.Varetekst" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Varetekst" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreLinje.kordrelinje_LevKod" "Lev.kod" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > KOrdreLinje.LevFargKod
"KOrdreLinje.LevFargKod" "LevFargKod" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > KOrdreLinje.Storl
"KOrdreLinje.Storl" "Str" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > KOrdreLinje.Kode
"KOrdreLinje.Kode" "Strekkode" "X(20)" "character" ? ? ? ? ? ? no "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > KOrdreLinje.Antall
"KOrdreLinje.Antall" "Antall" "->>,>>9" "DECIMAL" ? ? ? ? ? ? yes "Antall" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > KOrdreLinje.NettoPris
"KOrdreLinje.NettoPris" "Nettopris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Nettopris. Pris eksklusive mva og rabatter." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > KOrdreLinje.Linjesum
"KOrdreLinje.Linjesum" "Linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > KOrdreLinje.BetRef
"KOrdreLinje.BetRef" "Bet.ref." "x(20)" "CHARACTER" ? ? ? ? ? ? no "Betalingsreferanse på tilbakebetaling." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > KOrdreLinje.Notat
"KOrdreLinje.Notat" "Notat" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Informasjon til kunde om varen." no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreLinje */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrer varelinjer på retur ordre. */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrer varelinjer på retur ordre. */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  IF fcStrekkode:SCREEN-VALUE <> '' AND INT(fiReturKodeId:SCREEN-VALUE) > 0 THEN 
  DO:
    IF NOT JBoxServerAPI:Instance:Find("ReturKodeRegister", "WHERE ReturKodeId = " + fiReturKodeId:SCREEN-VALUE IN FRAME DEFAULT-FRAME) THEN
    DO:
      fcReturKodeTekst:SCREEN-VALUE = ''.
      JBoxSession:Instance:ViewMessage("Ugyldig returkode!").
      APPLY 'ENTRY' TO fiReturKodeId.
      RETURN NO-APPLY.      
    END.
    IF NOT JBoxServerAPI:Instance:Find("Strekkode", "WHERE Kode = '" + fcStrekkode:SCREEN-VALUE + "'") THEN
    DO: 
      fcBeskr:SCREEN-VALUE = ''.
      JBoxSession:Instance:ViewMessage("Ukjent strekkode!").
      APPLY 'ENTRY' TO fcStrekkode.
      RETURN NO-APPLY.
    END.

    PUBLISH 'opprettVarelinjer' (fcStrekkode:SCREEN-VALUE, INT(fiReturKodeId:SCREEN-VALUE), lRefKOrdre_Id).
    
    oBrwKOrdreLinje:refresh().
    
    ASSIGN 
      fcStrekkode:SCREEN-VALUE = ''
      fiReturkodeId:SCREEN-VALUE = ''
      fcBeskr:SCREEN-VALUE = ''
      fcReturkodeTekst:SCREEN-VALUE = ''
      .
    APPLY 'ENTRY' TO fcStrekkode.
    RETURN NO-APPLY.
  END.  
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Ingen vare registrert!").
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReklamasjonskode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReklamasjonskode C-Win
ON CHOOSE OF btnReklamasjonskode IN FRAME DEFAULT-FRAME /* ... */
DO:
  /* To manipulate the lookup query object add the callback procedure hook "myLookupObject" 
     (probably use "WHERE false" as the initial query in this case) */

  fiReturKodeId = INT(JBoxServerAPI:Instance:FieldValue("ReturKodeRegister.ReturKodeId")).
  fcReturKodeTekst = JBoxServerAPI:Instance:FieldValue("ReturKodeRegister.ReturKodeTekst").
  
  JBoxServerAPI:Instance:LookupDialog("ReturKodeRegister"
                    + ";ReturKodeId"
                    + ";ReturKodeTekst"
                   ,"WHERE ReturKodeRegister.ReturKodeId > 0 "
                   ,"ReturKodeId,ReturKodeTekst"   /* <- return values for these fields */
                    ).

  IF JBoxServerAPI:Instance:LookupOk THEN 
  DO:
    ASSIGN       
      fiReturKodeId:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(JBoxServerAPI:Instance:LookupValue("ReturKodeId"))
      fcReturKodeTekst:SCREEN-VALUE IN FRAME DEFAULT-FRAME = JBoxServerAPI:Instance:LookupValue("ReturKodeTekst")
      .
    APPLY 'CHOOSE' TO BtnOk.
  END.
  ELSE DO:
    APPLY 'ENTRY' TO fiReturKodeId.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fcStrekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcStrekkode C-Win
ON RETURN OF fcStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  APPLY 'TAB' TO fcStrekkode.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcStrekkode C-Win
ON TAB OF fcStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  IF fcStrekkode:SCREEN-VALUE <> '' THEN 
  DO:
    IF JBoxServerAPI:Instance:Find("Strekkode", "WHERE Kode = '" + fcStrekkode:SCREEN-VALUE + "'") THEN
    DO:
        lArtikkelNr = DEC(JBoxServerAPI:Instance:FieldValue("Strekkode.ArtikkelNr")).
        IF JBoxServerAPI:Instance:Find("ArtBas", "WHERE ArtikkelNr = '" + STRING(lArtikkelNr) + "'") THEN
          fcBeskr:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("ArtBas.Beskr").
        ELSE DO: 
          fcBeskr:SCREEN-VALUE = ''.
          JBoxSession:Instance:ViewMessage("Ukjent strekkode!").
          RETURN NO-APPLY.
        END.
    END.
    ELSE DO: 
      fcBeskr:SCREEN-VALUE = ''.
      JBoxSession:Instance:ViewMessage("Ugyldig strekkode!").
      APPLY 'ENTRY' TO fcStrekkode.
      RETURN NO-APPLY.
    END.
  END.
  ELSE DO: 
    fcBeskr:SCREEN-VALUE = ''.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiReturKodeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiReturKodeId C-Win
ON RETURN OF fiReturKodeId IN FRAME DEFAULT-FRAME /* Returkode */
DO:
  APPLY 'TAB' TO fiReturKodeId.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiReturKodeId C-Win
ON TAB OF fiReturKodeId IN FRAME DEFAULT-FRAME /* Returkode */
DO:
  IF fiReturKodeId:SCREEN-VALUE IN FRAME DEFAULT-FRAME <> '' THEN 
  DO:
    IF JBoxServerAPI:Instance:Find("ReturKodeRegister", "WHERE ReturKodeId = " + fiReturKodeId:SCREEN-VALUE IN FRAME DEFAULT-FRAME) THEN
    DO:
      fcReturKodeTekst:SCREEN-VALUE IN FRAME DEFAULT-FRAME = JBoxServerAPI:Instance:FieldValue("ReturKodeRegister.ReturKodeTekst").
      APPLY 'CHOOSE' TO BtnOk.
      APPLY 'ENTRY' TO fcStrekkode.
      RETURN NO-APPLY.
    END.  
    ELSE DO:
      IF INT(fiReturKodeId) > 0 THEN 
      DO: 
        fcReturKodeTekst:SCREEN-VALUE = ''.
        JBoxSession:Instance:ViewMessage("Ugyldig returkode!").
        APPLY 'ENTRY' TO fiReturKodeId.
        RETURN NO-APPLY.
      END.
    END.
  END.  
  ELSE DO:
    fcReturKodeTekst:SCREEN-VALUE = ''.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwKOrdreLinje
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
{incl/conttrigg.i oBrwKOrdreLinje:BROWSE-HANDLE}
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
  DISPLAY EkstOrdreNr KOrdre_Id Navn fcBeskr fcStrekkode fiReturKodeId 
          fcReturKodeTekst fiTekst fiTekst-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BrwKOrdreLinje first_tbKOrdreLinje last_tbKOrdreLinje 
         next_tbKOrdreLinje prev_tbKOrdreLinje refresh_tbKOrdreLinje BtnOK 
         BtnDone fcStrekkode fiReturKodeId btnReklamasjonskode RECT-5 RECT-6 
         tbKOrdreLinje 
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
DEFINE VARIABLE pcFeltVerdier AS CHARACTER NO-UNDO.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

DO WITH FRAME {&FRAME-NAME}:  
  pcFeltVerdier = ''.
  
  oBrwKOrdreLinje = NEW JBoxBrowse(brwKOrdreLinje:HANDLE).
  otbKOrdreLinje = NEW JBoxToolbar(tbKOrdreLinje:HANDLE).
  oBrwKOrdreLinje:TOOLBAR-OBJECT = otbKOrdreLinje.
  oBrwKOrdreLinje:baseQuery = "WHERE KOrdre_Id = '" + STRING(lRefKOrdre_Id) + "'".

  opopupVare = NEW JBoxPopupMenu().
  opopupVare:AddToolGroup('KopierKode;Kopier kode').
  oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
  
/*  IF JBoxServerAPI:Instance:Find("KOrdreHode", "WHERE KOrdre_Id = '" + STRING(lRefKOrdre_Id) + "'") THEN*/
  pcFeltVerdier = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode", "WHERE KOrdre_Id = '" + STRING(lRefKOrdre_Id) + "'","Navn,EkstOrdreNr").
  IF NUM-ENTRIES(pcFeltVerdier,'|') = 2 THEN 
  DO:    
/*      Navn        = JBoxServerAPI:Instance:FieldValue("KOrdreHode.Navn").*/
      Navn        = ENTRY(1,pcFeltVerdier,'|').
      KOrdre_Id   = lRefKOrdre_Id.
/*      EkstOrdreNr = JBoxServerAPI:Instance:FieldValue("KOrdreHode.EkstOrdreNr").*/
      EkstOrdreNr = ENTRY(2,pcFeltVerdier,'|').
      DISPLAY
          Navn
          KOrdre_Id
          EkstOrdreNr
          .
  END.
  
  oContainer:setNoResizeY("RECT-5").
  oContainer:setAddMoveY("BtnOK,BtnDone,fiTekst,RECT-5,fcStrekkode,fcBeskr,fiReturKodeId,fcReturKodetekst,btnReklamasjonskode").
END.
oBrwKOrdreLinje:OpenQuery().

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  oContainer:initResize().
&ELSE
  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|ServerQuery.log," +   
                    "TransLogFile|ServerTrans.log").
&ENDIF
  RUN MoveToTop.
/*  THIS-PROCEDURE:CURRENT-WINDOW:ALWAYS-ON-TOP = TRUE.*/
  APPLY 'ENTRY' TO fcStrekkode.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierKodeRecord C-Win
PROCEDURE KopierKodeRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DO WITH FRAME DEFAULT-FRAME:
    IF AVAILABLE KOrdreLinje THEN 
    DO:
      IF icModus = '20' THEN
      BYTTE: 
      DO:
        IF KOrdreLinje.aktiv AND KOrdreLinje.Returnert = FALSE THEN 
          ASSIGN 
            fcStrekkode:SCREEN-VALUE = KOrdreLinje.Kode
            .
        ELSE IF KOrdreLinje.aktiv AND KOrdreLinje.Returnert = TRUE THEN
        DO: 
          IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Linjen er tidigere retunert. Skal den alikevel byttes?") THEN 
            RETURN.
          ASSIGN 
            fcStrekkode:SCREEN-VALUE = KOrdreLinje.Kode
            .
        END.
        ELSE DO:
          JBoxSession:Instance:ViewMessage("Bare aktive og ikke tidligere returnerte ordrelinjer kan returneres.").
        END.
      END.
      
      ELSE RETUR: DO:
        IF KOrdreLinje.aktiv AND KOrdreLinje.Returnert = FALSE THEN 
          ASSIGN 
            fcStrekkode:SCREEN-VALUE = KOrdreLinje.Kode
            .
        ELSE DO:
          JBoxSession:Instance:ViewMessage("Bare aktive og ikke tidligere returnerte ordrelinjer kan returneres.").
        END.
      END.
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

