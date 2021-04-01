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
DEFINE VARIABLE hBokforingsVisning_TekstColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hBokforingsVisning_BelopColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttBokforingsbilag NO-UNDO
  FIELD BokforingsId AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  .

/*** Start instance property definitions for JBoxBrowse object oBrwBokforingsVisning ***/
DEF VAR oBrwBokforingsVisning AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE BokforingsVisning
    FIELD Tekst AS CHARACTER
    FIELD Belop AS CHARACTER
    FIELD Konto AS CHARACTER
    FIELD LinjeNr AS INTEGER
    FIELD BokforingsID AS DECIMAL
    FIELD KorrTillatt AS LOGICAL
    FIELD TTId AS INTEGER
    FIELD TBId AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .


FUNCTION getBuffersAndFieldsBrwBokforingsVisning RETURNS CHARACTER():
  RETURN
    'BokforingsVisning'
     + ';Tekst'
     + ';Belop'
     + ';Konto'
     + ';LinjeNr'
     + ';BokforingsID'
     + ';KorrTillatt'
     + ';TTId'
     + ';TBId'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwBokforingsVisning RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.


DEF VAR otbBokforingsKorrBilag AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwBokforingsKorrBilag ***/
DEF VAR oBrwBokforingsKorrBilag AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE BokforingsKorrBilag
    FIELD TTId AS INTEGER
    FIELD TBId AS INTEGER
    FIELD BokforingsKorrBilag_Type AS CHARACTER
    FIELD Merknad AS CHARACTER
    FIELD Belop AS DECIMAL
    FIELD KontoNr AS INTEGER
    FIELD LinjeNr AS INTEGER
    FIELD EDatoTid AS DATETIME
    FIELD DatoTid AS DATETIME
    FIELD BrukerId AS CHARACTER
    FIELD EBrukerid AS CHARACTER
    FIELD BokforingsID AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .

FUNCTION getBuffersAndFieldsBrwBokforingsKorrBilag RETURNS CHARACTER():
  RETURN
    'BokforingsKorrBilag'
     + ';TTId'
     + ';TBId'
     + ';Merknad'
     + ';Belop'
     + ';KontoNr'
     + ';LinjeNr'
     + ';EDatoTid'
     + ';DatoTid'
     + ';BrukerId'
     + ';EBrukerid'
     + ';BokforingsID'
     + ';+BokforingsKorrBilag_Type|CHARACTER||BokforingsKorrBilag_Type|Type'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwBokforingsKorrBilag RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwBokforingsKorrBilag RETURNS CHARACTER():
  RETURN 
     'BokforingsKorrBilag_brwcalc.p' /* BokforingsKorrBilag_Type */
     .
END FUNCTION.


DEF VAR oFmBokforingsKorrBilag AS JBoxFieldMap NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwBokforingsKorrBilag

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BokforingsKorrBilag BokforingsVisning

/* Definitions for BROWSE BrwBokforingsKorrBilag                        */
&Scoped-define FIELDS-IN-QUERY-BrwBokforingsKorrBilag ~
BokforingsKorrBilag.TTId BokforingsKorrBilag.TBId ~
BokforingsKorrBilag.BokforingsKorrBilag_Type BokforingsKorrBilag.Merknad ~
BokforingsKorrBilag.Belop BokforingsKorrBilag.KontoNr ~
BokforingsKorrBilag.LinjeNr BokforingsKorrBilag.EDatoTid ~
BokforingsKorrBilag.DatoTid BokforingsKorrBilag.BrukerId ~
BokforingsKorrBilag.EBrukerid BokforingsKorrBilag.BokforingsID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwBokforingsKorrBilag ~
BokforingsKorrBilag.TTId BokforingsKorrBilag.LinjeNr 
&Scoped-define QUERY-STRING-BrwBokforingsKorrBilag FOR EACH BokforingsKorrBilag NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwBokforingsKorrBilag OPEN QUERY BrwBokforingsKorrBilag FOR EACH BokforingsKorrBilag NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwBokforingsKorrBilag BokforingsKorrBilag
&Scoped-define FIRST-TABLE-IN-QUERY-BrwBokforingsKorrBilag BokforingsKorrBilag


/* Definitions for BROWSE BrwBokforingsVisning                          */
&Scoped-define FIELDS-IN-QUERY-BrwBokforingsVisning BokforingsVisning.Tekst ~
BokforingsVisning.Belop BokforingsVisning.Konto BokforingsVisning.LinjeNr ~
BokforingsVisning.BokforingsID BokforingsVisning.KorrTillatt ~
BokforingsVisning.TTId BokforingsVisning.TBId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwBokforingsVisning ~
BokforingsVisning.Tekst BokforingsVisning.LinjeNr 
&Scoped-define QUERY-STRING-BrwBokforingsVisning FOR EACH BokforingsVisning NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwBokforingsVisning OPEN QUERY BrwBokforingsVisning FOR EACH BokforingsVisning NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwBokforingsVisning BokforingsVisning
&Scoped-define FIRST-TABLE-IN-QUERY-BrwBokforingsVisning BokforingsVisning


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbBokforingsKorrBilag RECT-11 ~
BrwBokforingsVisning new_tbBokforingsKorrBilag edit_tbBokforingsKorrBilag ~
undo_tbBokforingsKorrBilag save_tbBokforingsKorrBilag ~
delete_tbBokforingsKorrBilag refresh_tbBokforingsKorrBilag ~
VisBokfBilag_tbBokforingsVisning Merknad KontoNr Belop ~
BrwBokforingsKorrBilag 
&Scoped-Define DISPLAYED-OBJECTS LinjeNr BokforingsID Merknad KontoNr Belop 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON delete_tbBokforingsKorrBilag 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 7.8 BY 1.95 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbBokforingsKorrBilag 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 7.8 BY 1.95 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON new_tbBokforingsKorrBilag 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 7.8 BY 1.95 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON refresh_tbBokforingsKorrBilag 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 7.8 BY 1.95 TOOLTIP "Refresh (F5)".

DEFINE BUTTON save_tbBokforingsKorrBilag 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 7.8 BY 1.95 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbBokforingsKorrBilag 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 7.8 BY 1.95 TOOLTIP "Angre (CTRL-Z)".

DEFINE BUTTON VisBokfBilag_tbBokforingsVisning 
     LABEL "Oppdater" 
     SIZE 15 BY 1.95 TOOLTIP "Viser/oppdaterer bokforings informasjonen".

DEFINE VARIABLE KontoNr AS INTEGER FORMAT ">>9999":U INITIAL 0 
     LABEL "Konto" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 44.4 BY 1 TOOLTIP "Konto bilaget skal føres på." NO-UNDO.

DEFINE VARIABLE Belop AS DECIMAL FORMAT "->>,>>>,>>9.99" INITIAL 0 
     LABEL "Beløp" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 TOOLTIP "Belop".

DEFINE VARIABLE BokforingsID AS DECIMAL FORMAT ">>>>>>>>>>>>>>>9" INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 20.4 BY 1.

DEFINE VARIABLE LinjeNr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Linje" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1 TOOLTIP "Rekkefølge registrering er gjort.".

DEFINE VARIABLE Merknad AS CHARACTER FORMAT "x(50)" 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 44.4 BY 1 TOOLTIP "Kort merknad til bilaget.".

DEFINE VARIABLE TBId AS INTEGER FORMAT ">>9" INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1 TOOLTIP "Transaksjonstype beskrivelse".

DEFINE VARIABLE TTId AS INTEGER FORMAT "zz9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1 TOOLTIP "TransaksjonstypensID".

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 5.

DEFINE RECTANGLE tbBokforingsKorrBilag
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 63 BY 2.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwBokforingsKorrBilag FOR 
      BokforingsKorrBilag SCROLLING.

DEFINE QUERY BrwBokforingsVisning FOR 
      BokforingsVisning SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwBokforingsKorrBilag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwBokforingsKorrBilag C-Win _STRUCTURED
  QUERY BrwBokforingsKorrBilag NO-LOCK DISPLAY
      BokforingsKorrBilag.TTId COLUMN-LABEL "TTId" FORMAT ">>9":U
            WIDTH 3
      BokforingsKorrBilag.TBId COLUMN-LABEL "TBId" FORMAT ">>9":U
            WIDTH 3
      BokforingsKorrBilag.BokforingsKorrBilag_Type COLUMN-LABEL "Type" FORMAT "X(8)":U
            WIDTH 3
      BokforingsKorrBilag.Merknad COLUMN-LABEL "Merknad" FORMAT "x(150)":U
            WIDTH 20
      BokforingsKorrBilag.Belop COLUMN-LABEL "Belop" FORMAT "->>,>>>,>>9.99":U
            WIDTH 9
      BokforingsKorrBilag.KontoNr COLUMN-LABEL "Kontonr" FORMAT ">>9999":U
      BokforingsKorrBilag.LinjeNr COLUMN-LABEL "Nr" FORMAT ">>9":U
            WIDTH 3
      BokforingsKorrBilag.EDatoTid COLUMN-LABEL "EDatoTid" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 23
      BokforingsKorrBilag.DatoTid COLUMN-LABEL "DatoTid" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 23
      BokforingsKorrBilag.BrukerId COLUMN-LABEL "BrukerId" FORMAT "x(15)":U
      BokforingsKorrBilag.EBrukerid COLUMN-LABEL "EBrukerid" FORMAT "x(15)":U
      BokforingsKorrBilag.BokforingsID COLUMN-LABEL "Bokføringsbilagets unike ID" FORMAT ">>>>>>>>>>>>>>>9":U
  ENABLE
      BokforingsKorrBilag.TTId HELP "TransaksjonstypensID"
      BokforingsKorrBilag.LinjeNr HELP "Rekkefølge registrering er gjort."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 62.8 BY 4.62 FIT-LAST-COLUMN.

DEFINE BROWSE BrwBokforingsVisning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwBokforingsVisning C-Win _STRUCTURED
  QUERY BrwBokforingsVisning NO-LOCK DISPLAY
      BokforingsVisning.Tekst COLUMN-LABEL "Tekst" FORMAT "x(150)":U
            WIDTH 25
      BokforingsVisning.Belop COLUMN-LABEL "Belop" FORMAT "x(15)":U
            WIDTH 8
      BokforingsVisning.Konto COLUMN-LABEL "Konto" FORMAT "x(12)":U
            WIDTH 8
      BokforingsVisning.LinjeNr COLUMN-LABEL "Nr" FORMAT ">>9":U
            WIDTH 2.6
      BokforingsVisning.BokforingsID COLUMN-LABEL "ID" FORMAT ">>>>>>>>>>>>>>>9":U
            WIDTH 3.2
      BokforingsVisning.KorrTillatt COLUMN-LABEL "KorrTillatt" FORMAT "yes/no":U
      BokforingsVisning.TTId COLUMN-LABEL "TTId" FORMAT ">>9":U
      BokforingsVisning.TBId COLUMN-LABEL "TBId" FORMAT ">>9":U
  ENABLE
      BokforingsVisning.Tekst
      BokforingsVisning.LinjeNr HELP "Rekkefølge registrering er gjort."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 49 BY 11.95 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrwBokforingsVisning AT ROW 1.19 COL 2 WIDGET-ID 200
     new_tbBokforingsKorrBilag AT ROW 1.24 COL 52.2 WIDGET-ID 16
     edit_tbBokforingsKorrBilag AT ROW 1.24 COL 60.2 WIDGET-ID 18
     undo_tbBokforingsKorrBilag AT ROW 1.24 COL 68 WIDGET-ID 22
     save_tbBokforingsKorrBilag AT ROW 1.24 COL 75.8 WIDGET-ID 24
     delete_tbBokforingsKorrBilag AT ROW 1.24 COL 83.6 WIDGET-ID 26
     refresh_tbBokforingsKorrBilag AT ROW 1.24 COL 91.4 WIDGET-ID 28
     VisBokfBilag_tbBokforingsVisning AT ROW 1.24 COL 99.2 WIDGET-ID 4
     LinjeNr AT ROW 3.81 COL 99 COLON-ALIGNED
     BokforingsID AT ROW 3.86 COL 62.6 COLON-ALIGNED
     TTId AT ROW 3.86 COL 62.6 COLON-ALIGNED NO-LABEL
     TBId AT ROW 3.86 COL 70.4 COLON-ALIGNED NO-LABEL
     Merknad AT ROW 4.91 COL 62.6 COLON-ALIGNED
     KontoNr AT ROW 5.95 COL 62.6 COLON-ALIGNED WIDGET-ID 34
     Belop AT ROW 7 COL 62.6 COLON-ALIGNED
     BrwBokforingsKorrBilag AT ROW 8.52 COL 52.2 WIDGET-ID 300
     tbBokforingsKorrBilag AT ROW 1.19 COL 52 WIDGET-ID 6
     RECT-11 AT ROW 3.38 COL 52 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.6 BY 12.43 WIDGET-ID 100.


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
         HEIGHT             = 12.48
         WIDTH              = 115
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 155.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 155.8
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
/* BROWSE-TAB BrwBokforingsVisning RECT-11 DEFAULT-FRAME */
/* BROWSE-TAB BrwBokforingsKorrBilag Belop DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN BokforingsID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LinjeNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbBokforingsKorrBilag:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "new;Ny,edit;Edit,undo;Angre,save;Lagre,delete;Slett,refresh;Refreshmaxborder".

/* SETTINGS FOR FILL-IN TBId IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TBId:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN TTId IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TTId:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwBokforingsKorrBilag
/* Query rebuild information for BROWSE BrwBokforingsKorrBilag
     _TblList          = "SkoTex.BokforingsKorrBilag"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"BokforingsKorrBilag.TTId" "TTId" ">>9" "INTEGER" ? ? ? ? ? ? yes "TransaksjonstypensID" no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"BokforingsKorrBilag.TBId" "TBId" ">>9" "INTEGER" ? ? ? ? ? ? no "Transaksjonstype beskrivelse" no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"BokforingsKorrBilag.BokforingsKorrBilag_Type" "Type" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"BokforingsKorrBilag.Merknad" "Merknad" "x(150)" "CHARACTER" ? ? ? ? ? ? no "Kort merknad til bilaget." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"BokforingsKorrBilag.Belop" "Belop" "->>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Belop" no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"BokforingsKorrBilag.KontoNr" "Kontonr" ">>9999" "INTEGER" ? ? ? ? ? ? no "" no no "7.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"BokforingsKorrBilag.LinjeNr" "Nr" ">>9" "INTEGER" ? ? ? ? ? ? yes "Rekkefølge registrering er gjort." no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"BokforingsKorrBilag.EDatoTid" "EDatoTid" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "Dato/tid for siste endring." no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"BokforingsKorrBilag.DatoTid" "DatoTid" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "Dato og tidspunkt for regsitrering av bilag." no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"BokforingsKorrBilag.BrukerId" "BrukerId" "x(15)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte bilaget" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"BokforingsKorrBilag.EBrukerid" "EBrukerid" "x(15)" "CHARACTER" ? ? ? ? ? ? no "Sist endret av" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"BokforingsKorrBilag.BokforingsID" "Bokføringsbilagets unike ID" ">>>>>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "25.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwBokforingsKorrBilag */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwBokforingsVisning
/* Query rebuild information for BROWSE BrwBokforingsVisning
     _TblList          = "SkoTex.BokforingsVisning"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"BokforingsVisning.Tekst" "Tekst" "x(150)" "CHARACTER" ? ? ? ? ? ? yes "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"BokforingsVisning.Belop" "Belop" "x(15)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"BokforingsVisning.Konto" "Konto" "x(12)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"BokforingsVisning.LinjeNr" "Nr" ">>9" "INTEGER" ? ? ? ? ? ? yes "Rekkefølge registrering er gjort." no no "2.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"BokforingsVisning.BokforingsID" "ID" ">>>>>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"BokforingsVisning.KorrTillatt" "KorrTillatt" "yes/no" "LOGICAL" ? ? ? ? ? ? no "" no no "8.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"BokforingsVisning.TTId" "TTId" ">>9" "INTEGER" ? ? ? ? ? ? no "" no no "4.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"BokforingsVisning.TBId" "TBId" ">>9" "INTEGER" ? ? ? ? ? ? no "" no no "4.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwBokforingsVisning */
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


&Scoped-define SELF-NAME Belop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Belop C-Win
ON RETURN OF Belop IN FRAME DEFAULT-FRAME /* Beløp */
DO:
  APPLY 'TAB' TO Belop.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Belop C-Win
ON TAB OF Belop IN FRAME DEFAULT-FRAME /* Beløp */
DO:
  APPLY 'ENTRY' TO Merknad.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KontoNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KontoNr C-Win
ON RETURN OF KontoNr IN FRAME DEFAULT-FRAME /* Konto */
DO:
  APPLY 'TAB' TO KontoNr.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KontoNr C-Win
ON TAB OF KontoNr IN FRAME DEFAULT-FRAME /* Konto */
DO:
  APPLY 'ENTRY' TO Belop.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Merknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Merknad C-Win
ON RETURN OF Merknad IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  APPLY 'TAB' TO Merknad.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Merknad C-Win
ON TAB OF Merknad IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  APPLY 'ENTRY' TO KontoNr.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VisBokfBilag_tbBokforingsVisning
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VisBokfBilag_tbBokforingsVisning C-Win
ON CHOOSE OF VisBokfBilag_tbBokforingsVisning IN FRAME DEFAULT-FRAME /* Oppdater */
DO:
  RUN VisBokfBilagRecord.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwBokforingsKorrBilag
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
{incl/conttrigg.i oBrwBokforingsVisning:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
     VisBokfBilag_tbBokforingsVisning:BGCOLOR = 12.
    RUN VisBokfBilagRecord.
    
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
IF oBrwBokforingsVisning:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  IF oBrwBokforingsVisning:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
  
    KontoNr:LIST-ITEM-PAIRS = '<Ingen>|0|' + DYNAMIC-FUNCTION("getFieldList",
                                                              "Syspara;Parameter1|Beskrivelse;Parameter1",
                                                              "WHERE SysPara.SysHId = 20 AND SysPara.SysGr  = 6"
                                                              ).    
    /* TN 11/5-20 Fast veksel ligger pr. butikk og hentes fra SIETrans tabellen TTId/TBId/ButikkNr. 900/30/ButNr. */
    IF AVAILABLE BokforingsVisning THEN 
      KontoNr:LIST-ITEM-PAIRS = RIGHT-TRIM(KontoNr:LIST-ITEM-PAIRS + '|' + DYNAMIC-FUNCTION("getFieldList",
                                                                "SIETransType;KontoNr|Beskrivelse;KontoNr",
                                                                "WHERE SIETransType.ButikkNr = '"  + 
                                                                  STRING(oBrwBokforingsVisning:PARENT-BROWSE-OBJECT:BUFFER-HANDLE::ButikkNr) + 
                                                                  "' AND SIETransType.TTId = 900 AND SIETransType.TBId = 30"
                                                                ),'|').    
    ASSIGN 
      KontoNr = 0
      KontoNr:SCREEN-VALUE = '0'
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
  DISPLAY LinjeNr BokforingsID Merknad KontoNr Belop 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbBokforingsKorrBilag RECT-11 BrwBokforingsVisning 
         new_tbBokforingsKorrBilag edit_tbBokforingsKorrBilag 
         undo_tbBokforingsKorrBilag save_tbBokforingsKorrBilag 
         delete_tbBokforingsKorrBilag refresh_tbBokforingsKorrBilag 
         VisBokfBilag_tbBokforingsVisning Merknad KontoNr Belop 
         BrwBokforingsKorrBilag 
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
  
  oBrwBokforingsVisning = NEW JBoxBrowse(brwBokforingsVisning:HANDLE).
  hBokforingsVisning_TekstColumn = oBrwBokforingsVisning:getColumnHandle("Tekst").
  hBokforingsVisning_BelopColumn = oBrwBokforingsVisning:getColumnHandle("Belop").
  
  oContainer:setNoResizeX("BrwBokforingsVisning").

  oBrwBokforingsKorrBilag = NEW JBoxBrowse(brwBokforingsKorrBilag:HANDLE).
  /* NB: Se i MoveTotop - Der settes parant handle til oBrwBokforingsKorrBilag. Gjøres der pga. realisering. */
  
  oBrwBokforingsKorrBilag:setQuerySort("Linjenr;desc").
  oFmBokforingsKorrBilag = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmBokforingsKorrBilag:updateFields = 'Merknad,Belop,KontoNr'.
/*  oFmBokforingsKorrBilag:displayFields = 'BokforingsID,LinjeNr,TTId,TBId,Merknad,Belop,DatoTid,EBrukerid,EDatoTid,BrukerId'.*/
  oFmBokforingsKorrBilag:displayFields = 'BokforingsID,LinjeNr,TTId,TBId,Merknad,Belop'.
  oFmBokforingsKorrBilag:primaryKeyFields = 'BokforingsID,LinjeNr,TTId,TBId'.
  
  oFmBokforingsKorrBilag:BROWSE-OBJECT = oBrwBokforingsKorrBilag.
  
  oFmBokforingsKorrBilag:customCreateProc = "BokforingsKorrbilag_create.p".
  oFmBokforingsKorrBilag:customDeleteValProc = "BokforingsKorrbilag_delete_validate.p".    
  oFmBokforingsKorrBilag:customUpdateValProc = "IGNORE".
  oFmBokforingsKorrBilag:postUpdateProc = "bokforingsKorrBilag_post_update.p".
  
  otbBokforingsKorrBilag = NEW JBoxToolbar(tbBokforingsKorrBilag:HANDLE).

  oBrwBokforingsKorrBilag:TOOLBAR-OBJECT = otbBokforingsKorrBilag.
  oFmBokforingsKorrBilag:TOOLBAR-OBJECT = otbBokforingsKorrBilag.
  
  ASSIGN 
    KontoNr:DELIMITER = "|"
    .
    
END.
/*oBrwBokforingsKorrBilag:OpenQuery().*/
/*oBrwBokforingsVisning:OpenQuery().  */

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

  /* Må gjøres her ved oppstart av programmet, da oBrwBokforingsVisning først nå er realisert. */
  oBrwBokforingsKorrBilag:setParentBrowseObject(oBrwBokforingsVisning:PARENT-BROWSE-OBJECT,"BokforingsId").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
DEFINE VARIABLE piTTId AS INTEGER NO-UNDO.
  DEFINE VARIABLE piTBId AS INTEGER NO-UNDO.
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT AVAILABLE BokforingsVisning OR NUM-ENTRIES(BokforingsVisning.Tekst,'(') < 2 THEN 
    DO:
        JBoxSession:Instance:ViewMessage("Ingen rad markert." + CHR(10) + "Marker rad i bokføringsbilaget som skal korrigeres.").
        RETURN.
    END.

    IF AVAILABLE BokforingsVisning AND BokforingsVisning.KorrTillatt = FALSE THEN 
    DO:
        JBoxSession:Instance:ViewMessage("Raden kan ikke redigeres." + CHR(10) + "Rader som kan redigeres er markert med gul farge.").
        RETURN.
    END.
        
    ASSIGN 
      pcTekst = ENTRY(2,BokforingsVisning.Tekst,'(')
      pcTekst = ENTRY(1,pcTekst,')')
      piTTId  = INT(ENTRY(1,pcTekst,'/'))
      piTBId  = INT(ENTRY(2,pcTekst,'/'))
      .
      
    oFmBokforingsKorrBilag:bufferExtraFields = "BokforingsId,TTId,TBId,EBrukerid".
    oFmBokforingsKorrBilag:bufferExtraValues = STRING(BokforingsVisning.BokforingsId) + '|' + STRING(piTTId) + '|' + STRING(piTBId) + '|' + JBoxSession:Instance:UserId.
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:

/*    IF NOT JBoxServerApi:Instance:Update("BokforingsKorrBilag",                                                                      */
/*                                  BokforingsKorrBilag.RowIdent1,                                                                     */
/*                                  "BokforingsId,TTId,TBId",                                                                          */
/*                                  STRING(BokforingsVisning.BokforingsId) + '|' + STRING(piTTId) + '|' + STRING(piTBId),              */
/*                                  FALSE,                                                                                             */
/*                                  "bokforingsKorrBilag_post_update.p",                                                               */
/*                                  TRUE) THEN                                                                                         */
/*    DO:                                                                                                                              */
/*        JBoxSession:Instance:ViewMessage("Feil ved opprettelse av korreksjonspost pga.: " + JBoxServerAPI:Instance:getCallMessage()).*/
/*        RETURN.                                                                                                                      */
/*    END.                                                                                                                             */
    
/*    oBrwBokforingsKorrBilag:refreshRow().   */
/*    oBrwBokforingsKorrBilag:displayRecord().*/

    APPLY 'ENTRY' TO Merknad.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  IF AVAILABLE BokforingsVisning THEN 
  DO:
    IF BokforingsVisning.Tekst BEGINS '.' THEN 
      hBokforingsVisning_TekstColumn:BGCOLOR = 11.
    IF BokforingsVisning.KorrTillatt = TRUE THEN 
      hBokforingsVisning_TekstColumn:BGCOLOR = 14.
    BELOP:
    DO:
      IF BokforingsVisning.Tekst BEGINS 'Kasse diff' AND DEC(BokforingsVisning.Belop) <> 0 THEN 
        hBokforingsVisning_BelopColumn:BGCOLOR = 12.
      ELSE IF BokforingsVisning.Tekst BEGINS 'Veksel for mye/lite' AND DEC(BokforingsVisning.Belop) <> 0 THEN 
        hBokforingsVisning_BelopColumn:BGCOLOR = 12.
      ELSE IF CAN-FIND(FIRST BokforingsKorrBilag NO-LOCK WHERE
          BokforingsKorrBilag.BokforingsID = BokforingsVisning.BokforingsID AND
          BokforingsKorrBilag.TTId = BokforingsVisning.TTId AND
          BokforingsKorrBilag.TbId = BokforingsVisning.TBId) THEN 
        hBokforingsVisning_BelopColumn:BGCOLOR = 17.
    END.
  END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
DO WITH FRAME {&FRAME-NAME}:
    /* Typical.. (if dtCreated and dtModified are not added as standard fields to be populated by jbserv_getautoassigncrevalues.p/jbserv_getautoassignupdvalues.p)
    IF otbBokforingsKorrBilag:isCurrent THEN DO:
      IF otbBokforingsKorrBilag:objectState = "new" THEN
        oFmBokforingsKorrBilag:bufferExtraFields = "dtCreated".
      ELSE
        oFmBokforingsKorrBilag:bufferExtraFields = "dtModified".
      oFmBokforingsKorrBilag:bufferExtraValues = STRING(NOW).
    END.
          */
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
     VisBokfBilag_tbBokforingsVisning:BGCOLOR = 12.
    RUN VisBokfBilagRecord.
/*    /* For å oppdatere visning av diff etter registrering. */                                                */
/*    oBrwBokforingsKorrBilag:setParentBrowseObject(oBrwBokforingsVisning:PARENT-BROWSE-OBJECT,"BokforingsId").*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBokfBilagRecord C-Win 
PROCEDURE VisBokfBilagRecord :
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
  
  hBuffer = BUFFER ttBokforingsbilag:HANDLE.
  
  IF oBrwBokforingsVisning:PARENT-BROWSE-OBJECT:BUFFER-HANDLE::BokforingsId > 0 THEN 
  VISBILAG: 
  DO:
    cTekst = STRING(oBrwBokforingsVisning:PARENT-BROWSE-OBJECT:BUFFER-HANDLE::BokforingsId) + '|' + JBoxSession:Instance:UserId.
    
    CREATE ttBokforingsbilag. /* En record å sende med. */
    /* Kjører her generering av rapport, men uten utskrift. Genererer bare opp temp tabellen som viser bokf.bilaget i browser. */
    JBoxServerAPI:Instance:CallServerProc("Bokforingsbilag_getBlob.p",
                                          cTekst,
                                          hBuffer
                                          ).
    EMPTY TEMP-TABLE ttBokforingsbilag.
    oBrwBokforingsVisning:PARENT-BROWSE-OBJECT:refreshRow().
/*    oBrwBokforingsKorrBilag:PARENT-BROWSE-OBJECT:refreshRow().*/
    oBrwBokforingsVisning:openQuery().
  END. /* VISBILAG */

 DO WITH FRAME {&FRAME-NAME}:
   ASSIGN 
    VisBokfBilag_tbBokforingsVisning:BGCOLOR = 0.
    
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

