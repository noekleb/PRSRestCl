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


/* Code for Definitions: */

/*** Start instance property definitions for JBoxBrowse object oBrwPkSdlHode ***/
DEF VAR oBrwPkSdlHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE PkSdlHode
    FIELD PkSdlId AS DECIMAL
    FIELD PkSdlNr AS CHARACTER
    FIELD EkstId AS CHARACTER
    FIELD SendtDato AS DATE
    FIELD PkSdlStatus AS INTEGER
    FIELD pksdl_InnlevDato AS DATE
    FIELD CL AS INTEGER
    FIELD ButikkNr AS INTEGER
    FIELD cPalleNr AS CHARACTER
    FIELD Lokasjon AS CHARACTER
    FIELD PkSdlOpphav AS INTEGER
    FIELD OrdreType AS CHARACTER
    FIELD LandedCost AS DECIMAL
    FIELD Sesongkode AS CHARACTER
    FIELD SendtFraLagerTilOutlet AS DATETIME
    FIELD SendtOutlet AS INTEGER
    FIELD FakturaNr AS DECIMAL
    FIELD Merknad AS CHARACTER
    FIELD MeldingFraLev AS CHARACTER
    FIELD EDato AS DATE
    FIELD ETid AS INTEGER
    FIELD BrukerID AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD RegistrertAv AS CHARACTER
    FIELD LeveringsDato AS DATE
    FIELD levnamn AS CHARACTER
    FIELD levnr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_PkSdlHode FOR TEMP-TABLE PkSdlHode.


FUNCTION getBuffersAndFieldsBrwPkSdlHode RETURNS CHARACTER():
  RETURN
    'PkSdlHode'
     + ';PkSdlId'
     + ';PkSdlNr'
     + ';EkstId'
     + ';SendtDato'
     + ';PkSdlStatus'
     + ';CL'
     + ';ButikkNr'
     + ';cPalleNr'
     + ';Lokasjon'
     + ';PkSdlOpphav'
     + ';OrdreType'
     + ';LandedCost'
     + ';Sesongkode'
     + ';SendtFraLagerTilOutlet'
     + ';SendtOutlet'
     + ';FakturaNr'
     + ';Merknad'
     + ';MeldingFraLev'
     + ';EDato'
     + ';ETid'
     + ';BrukerID'
     + ';RegistrertDato'
     + ';RegistrertTid'
     + ';RegistrertAv'
     + ';LeveringsDato'
     + ';levnamn'
     + ';levnr'
     + ';+pksdl_InnlevDato|DATE||pksdl_InnlevDato(ROWID)|InnlevertDato'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwPkSdlHode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwPkSdlHode RETURNS CHARACTER():
  RETURN 
     'server/pksdl_brwcalc.p' /* pksdl_InnlevDato(ROWID) */
     .
END FUNCTION.


DEF VAR otbPkSdlHode AS JBoxToolbar NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwPkSdlHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PkSdlHode

/* Definitions for BROWSE BrwPkSdlHode                                  */
&Scoped-define FIELDS-IN-QUERY-BrwPkSdlHode PkSdlHode.PkSdlId ~
PkSdlHode.PkSdlNr PkSdlHode.EkstId PkSdlHode.SendtDato ~
PkSdlHode.PkSdlStatus PkSdlHode.pksdl_InnlevDato PkSdlHode.CL ~
PkSdlHode.ButikkNr PkSdlHode.cPalleNr PkSdlHode.Lokasjon ~
PkSdlHode.PkSdlOpphav PkSdlHode.OrdreType PkSdlHode.LandedCost ~
PkSdlHode.Sesongkode PkSdlHode.SendtFraLagerTilOutlet PkSdlHode.SendtOutlet ~
PkSdlHode.FakturaNr PkSdlHode.Merknad PkSdlHode.MeldingFraLev ~
PkSdlHode.EDato PkSdlHode.ETid PkSdlHode.BrukerID PkSdlHode.RegistrertDato ~
PkSdlHode.RegistrertTid PkSdlHode.RegistrertAv PkSdlHode.LeveringsDato ~
PkSdlHode.levnamn PkSdlHode.levnr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwPkSdlHode PkSdlHode.PkSdlId ~
PkSdlHode.BrukerID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define QUERY-STRING-BrwPkSdlHode FOR EACH PkSdlHode NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwPkSdlHode OPEN QUERY BrwPkSdlHode FOR EACH PkSdlHode NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwPkSdlHode PkSdlHode


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbPkSdlHode RECT-6 first_tbPkSdlHode ~
prev_tbPkSdlHode next_tbPkSdlHode last_tbPkSdlHode fiPkSdlNr BrwPkSdlHode 
&Scoped-Define DISPLAYED-OBJECTS fiPkSdlNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON first_tbPkSdlHode 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbPkSdlHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON next_tbPkSdlHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbPkSdlHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE VARIABLE fiPkSdlNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pakkseddelnummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Pakkseddelnummer på pakkseddel som søkes." NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 1.91.

DEFINE RECTANGLE tbPkSdlHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 130.6 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwPkSdlHode FOR 
      PkSdlHode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwPkSdlHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwPkSdlHode C-Win _STRUCTURED
  QUERY BrwPkSdlHode NO-LOCK DISPLAY
      PkSdlHode.PkSdlId COLUMN-LABEL "Pakkseddel id" FORMAT ">>>>>>>>>>>>9":U
      PkSdlHode.PkSdlNr COLUMN-LABEL "Pk.sdl.nr" FORMAT "X(15)":U
      PkSdlHode.EkstId COLUMN-LABEL "EkstId" FORMAT "X(15)":U
      PkSdlHode.SendtDato COLUMN-LABEL "Sendt dato" FORMAT "99/99/9999":U
      PkSdlHode.PkSdlStatus COLUMN-LABEL "Status" FORMAT ">9":U
      PkSdlHode.pksdl_InnlevDato COLUMN-LABEL "InnlevertDato" FORMAT "99/99/9999":U
      PkSdlHode.CL COLUMN-LABEL "CL" FORMAT ">>>>>9":U
      PkSdlHode.ButikkNr COLUMN-LABEL "Butikknr" FORMAT ">>>>>9":U
      PkSdlHode.cPalleNr COLUMN-LABEL "PalleNr" FORMAT "x(8)":U
      PkSdlHode.Lokasjon COLUMN-LABEL "Lokasjon" FORMAT "x(20)":U
      PkSdlHode.PkSdlOpphav COLUMN-LABEL "Opphav" FORMAT ">9":U
      PkSdlHode.OrdreType COLUMN-LABEL "OType" FORMAT "x(5)":U
      PkSdlHode.LandedCost COLUMN-LABEL "LandedCost" FORMAT "->>,>>>,>>>,>>9.99":U
      PkSdlHode.Sesongkode COLUMN-LABEL "Sesong" FORMAT "x(8)":U
      PkSdlHode.SendtFraLagerTilOutlet COLUMN-LABEL "Sendt outlet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      PkSdlHode.SendtOutlet COLUMN-LABEL "SB" FORMAT ">>>>>9":U
      PkSdlHode.FakturaNr COLUMN-LABEL "FakturaNr" FORMAT ">>>>>>>>>>>>9":U
      PkSdlHode.Merknad COLUMN-LABEL "Merknad" FORMAT "X(30)":U
      PkSdlHode.MeldingFraLev COLUMN-LABEL "Merkn. fra lev." FORMAT "X(30)":U
      PkSdlHode.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      PkSdlHode.ETid COLUMN-LABEL "ETid" FORMAT "->,>>>,>>9":U
      PkSdlHode.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      PkSdlHode.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      PkSdlHode.RegistrertTid COLUMN-LABEL "Registreringstidspunkt" FORMAT "->,>>>,>>9":U
      PkSdlHode.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      PkSdlHode.LeveringsDato COLUMN-LABEL "Leveringsdato" FORMAT "99/99/99":U
      PkSdlHode.levnamn COLUMN-LABEL "Navn" FORMAT "x(30)":U
      PkSdlHode.levnr COLUMN-LABEL "LevNr" FORMAT "zzzzz9":U WIDTH 6.2
  ENABLE
      PkSdlHode.PkSdlId HELP "Internt pakkseddelid."
      PkSdlHode.BrukerID HELP "Bruker som registrerte/endret posten"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 130.6 BY 13.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbPkSdlHode AT ROW 1.29 COL 2.2 WIDGET-ID 4
     prev_tbPkSdlHode AT ROW 1.29 COL 7 WIDGET-ID 6
     next_tbPkSdlHode AT ROW 1.29 COL 11.6 WIDGET-ID 8
     last_tbPkSdlHode AT ROW 1.29 COL 16.2 WIDGET-ID 10
     fiPkSdlNr AT ROW 3.43 COL 52 COLON-ALIGNED
     BrwPkSdlHode AT ROW 5.05 COL 2.4 WIDGET-ID 200
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.48 COL 31.8 WIDGET-ID 16
          FONT 6
     tbPkSdlHode AT ROW 1.19 COL 2 WIDGET-ID 2
     RECT-6 AT ROW 2.95 COL 30 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 132.6 BY 17.91 WIDGET-ID 100.


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
         TITLE              = "Oppslag pakkseddel"
         HEIGHT             = 17.91
         WIDTH              = 132.6
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 132.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 132.6
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrwPkSdlHode fiPkSdlNr DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 17.91
       FRAME DEFAULT-FRAME:WIDTH            = 132.6.

ASSIGN 
       PkSdlHode.CL:VISIBLE IN BROWSE BrwPkSdlHode = FALSE.

ASSIGN 
       tbPkSdlHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Lastmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwPkSdlHode
/* Query rebuild information for BROWSE BrwPkSdlHode
     _TblList          = "SkoTex.PkSdlHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > PkSdlHode.PkSdlId
"PkSdlHode.PkSdlId" "Pakkseddel id" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? yes "Internt pakkseddelid." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > PkSdlHode.PkSdlNr
"PkSdlHode.PkSdlNr" "Pk.sdl.nr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Pakkseddelnummer" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > PkSdlHode.EkstId
"PkSdlHode.EkstId" "EkstId" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Kobllingsfelt for å koble til ekstern ordre." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > PkSdlHode.SendtDato
"PkSdlHode.SendtDato" "Sendt dato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da varene er sendt fra leverandør." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > PkSdlHode.PkSdlStatus
"PkSdlHode.PkSdlStatus" "Status" ">9" "INTEGER" ? ? ? ? ? ? no "Pakkseddel status" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"PkSdlHode.pksdl_InnlevDato" "InnlevertDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "" no no "12.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > PkSdlHode.CL
"PkSdlHode.CL" "CL" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Sentrallager pakkseddelen er sendt til." no no "7.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > PkSdlHode.ButikkNr
"PkSdlHode.ButikkNr" "Butikknr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk ordren er stilet til. Settes til 0  hvis den går til fle" no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > PkSdlHode.cPalleNr
"PkSdlHode.cPalleNr" "PalleNr" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > PkSdlHode.Lokasjon
"PkSdlHode.Lokasjon" "Lokasjon" "x(20)" "CHARACTER" ? ? ? ? ? ? no "Viser hvilken lokasjon pakkseddelens varer er plassert i." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > PkSdlHode.PkSdlOpphav
"PkSdlHode.PkSdlOpphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Pakkseddelopphav. 1-Internt,2-ERP,3-Pda/Ht" no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > PkSdlHode.OrdreType
"PkSdlHode.OrdreType" "OType" "x(5)" "CHARACTER" ? ? ? ? ? ? no "Ordretype." no no "6.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > PkSdlHode.LandedCost
"PkSdlHode.LandedCost" "LandedCost" "->>,>>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "18.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > PkSdlHode.Sesongkode
"PkSdlHode.Sesongkode" "Sesong" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > PkSdlHode.SendtFraLagerTilOutlet
"PkSdlHode.SendtFraLagerTilOutlet" "Sendt outlet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato/tid når pakkseddel ble sendt fra lager til outlet" no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > PkSdlHode.SendtOutlet
"PkSdlHode.SendtOutlet" "SB" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Pakkseddelens varer er sendt til Outlet." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > PkSdlHode.FakturaNr
"PkSdlHode.FakturaNr" "FakturaNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Fakturanummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > PkSdlHode.Merknad
"PkSdlHode.Merknad" "Merknad" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Merknad" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > PkSdlHode.MeldingFraLev
"PkSdlHode.MeldingFraLev" "Merkn. fra lev." "X(30)" "CHARACTER" ? ? ? ? ? ? no "Merknad fra leverandør." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > PkSdlHode.EDato
"PkSdlHode.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > PkSdlHode.ETid
"PkSdlHode.ETid" "ETid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Endret tidspunkt" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > PkSdlHode.BrukerID
"PkSdlHode.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? yes "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > PkSdlHode.RegistrertDato
"PkSdlHode.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > PkSdlHode.RegistrertTid
"PkSdlHode.RegistrertTid" "Registreringstidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt for registrering av posten" no no "20.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > PkSdlHode.RegistrertAv
"PkSdlHode.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > PkSdlHode.LeveringsDato
"PkSdlHode.LeveringsDato" "Leveringsdato" "99/99/99" "DATE" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > PkSdlHode.levnamn
"PkSdlHode.levnamn" "Navn" "x(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > PkSdlHode.levnr
"PkSdlHode.levnr" "LevNr" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "" no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwPkSdlHode */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Oppslag pakkseddel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Oppslag pakkseddel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPkSdlNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPkSdlNr C-Win
ON RETURN OF fiPkSdlNr IN FRAME DEFAULT-FRAME /* Pakkseddelnummer */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPkSdlNr C-Win
ON TAB OF fiPkSdlNr IN FRAME DEFAULT-FRAME /* Pakkseddelnummer */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwPkSdlHode
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
{incl/conttrigg.i oBrwPkSdlHode:BROWSE-HANDLE}
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
IF oBrwPkSdlHode:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  
  otbPkSdlHode:disabledTools = 'filter'.
  
  RUN SUPER.
  IF oBrwPkSdlHode:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
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
  DISPLAY fiPkSdlNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbPkSdlHode RECT-6 first_tbPkSdlHode prev_tbPkSdlHode next_tbPkSdlHode 
         last_tbPkSdlHode fiPkSdlNr BrwPkSdlHode 
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

  oBrwPkSdlHode = NEW JBoxBrowse(brwPkSdlHode:HANDLE).
  otbPkSdlHode = NEW JBoxToolbar(tbPkSdlHode:HANDLE).

  oBrwPkSdlHode:TOOLBAR-OBJECT = otbPkSdlHode.
  oContainer:setNoResizeY("RECT-6").
  otbPkSdlHode:disabledTools = 'Filter'.
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
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cOperator AS CHARACTER NO-UNDO.
  
  DO WITH FRAME DEFAULT-FRAME:
      ASSIGN 
        cFields   = ''
        cWhere    = ''
        cOperator = ''
        .

    IF fiPkSdlNr:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'PkSdlNr'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiPkSdlNr:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + '='
        .
    oBrwPkSdlHode:setFilter(cFields,cOperator,cWhere).
    oBrwPkSdlHode:openQuery().    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

