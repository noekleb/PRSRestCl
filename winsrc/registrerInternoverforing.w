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
DEFINE INPUT PARAMETER iBuntNr AS INTEGER FORMAT "->>>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER cMerknad AS CHARACTER FORMAT "x(50)" NO-UNDO.
DEFINE INPUT PARAMETER iOpphav AS INTEGER NO-UNDO.
 
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bOk          AS LOG           NO-UNDO.
DEFINE VARIABLE ix           AS INTEGER       NO-UNDO.
DEFINE VARIABLE hBrowse      AS HANDLE        NO-UNDO.
DEFINE VARIABLE hQuery       AS HANDLE        NO-UNDO.
DEFINE VARIABLE hToolbar     AS HANDLE        NO-UNDO.
DEFINE VARIABLE hFieldMap    AS HANDLE        NO-UNDO.
DEFINE VARIABLE oContainer   AS JBoxContainer NO-UNDO.
DEFINE VARIABLE iLinjeNr     AS INTEGER       NO-UNDO.
DEFINE VARIABLE cVareFelt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVareVerdier AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNettButLager AS INTEGER NO-UNDO.
DEFINE VARIABLE cOvButLst AS CHARACTER NO-UNDO.
DEF VAR opopupOvBuffer AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAntallColumn AS HANDLE NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwOvBuffer ***/
DEFINE VARIABLE oBrwOvBuffer AS JBoxBrowse    NO-UNDO.
DEF TEMP-TABLE OvBuffer
    FIELD LinjeNr AS DECIMAL
    FIELD ovbuffer_Beskr AS CHARACTER
    FIELD ovbuffer_LevKod AS CHARACTER
    FIELD ovbuffer_LevFargKod AS CHARACTER
    FIELD Storl AS CHARACTER
    FIELD ovbuffer_Kode AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD ButikkNrTil AS INTEGER
    FIELD ovbuffer_TilButNavn AS CHARACTER
    FIELD ovbuffer_DatoTidEndret AS CHARACTER
    FIELD BrukerID AS CHARACTER
    FIELD ovbuffer_DatoTidRegistrert AS CHARACTER
    FIELD RegistrertAv AS CHARACTER
    FIELD BuntNr AS INTEGER
    FIELD ButikkNrFra AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD ovbuffer_BuntStatus AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
  .
DEFINE BUFFER v_OvBuffer FOR TEMP-TABLE OvBuffer.


FUNCTION getBuffersAndFieldsBrwOvBuffer RETURNS CHARACTER():
  RETURN
    'OvBuffer'
     + ';LinjeNr'
     + ';Storl'
     + ';Antall'
     + ';ButikkNrTil'
     + ';BrukerID'
     + ';RegistrertAv'
     + ';BuntNr'
     + ';ButikkNrFra'
     + ';ArtikkelNr'
     + ';+ovbuffer_Beskr|CHARACTER||ovbuffer_Beskr|Varetekst'
     + ';+ovbuffer_LevKod|CHARACTER||ovbuffer_LevKod|Lev.art.nr'
     + ';+ovbuffer_LevFargKod|CHARACTER||ovbuffer_LevFargKod|Fargekode'
     + ';+ovbuffer_Kode|CHARACTER||ovbuffer_Kode|Strekkode'
     + ';+ovbuffer_TilButNavn|CHARACTER||ovbuffer_TilButNavn|Navn'
     + ';+ovbuffer_DatoTidEndret|CHARACTER||ovbuffer_DatoTidEndret|Endret'
     + ';+ovbuffer_DatoTidRegistrert|CHARACTER||ovbuffer_DatoTidRegistrert|Registrert'
     + ';+ovbuffer_BuntStatus|CHARACTER||ovbuffer_BuntStatus|BuntStatus'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOvBuffer RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwOvBuffer RETURNS CHARACTER():
  RETURN 
     'server/ovbuffer_brwcalc.p' /* ovbuffer_Beskr */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_LevKod */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_LevFargKod */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_Kode */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_TilButNavn */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_DatoTidEndret */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_DatoTidRegistrert */
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_BuntStatus */
     .
END FUNCTION.


DEFINE VARIABLE otbOvBuffer AS JBoxToolbar   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrwOvBuffer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES OvBuffer

/* Definitions for BROWSE BrwOvBuffer                                   */
&Scoped-define FIELDS-IN-QUERY-BrwOvBuffer OvBuffer.LinjeNr ~
OvBuffer.ovbuffer_Beskr OvBuffer.ovbuffer_LevKod ~
OvBuffer.ovbuffer_LevFargKod OvBuffer.Storl OvBuffer.ovbuffer_Kode ~
OvBuffer.Antall OvBuffer.ButikkNrTil OvBuffer.ovbuffer_TilButNavn ~
OvBuffer.ovbuffer_DatoTidEndret OvBuffer.BrukerID ~
OvBuffer.ovbuffer_DatoTidRegistrert OvBuffer.RegistrertAv OvBuffer.BuntNr ~
OvBuffer.ButikkNrFra OvBuffer.ArtikkelNr OvBuffer.ovbuffer_BuntStatus 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOvBuffer OvBuffer.LinjeNr ~
OvBuffer.BuntNr 
&Scoped-define QUERY-STRING-BrwOvBuffer FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOvBuffer OPEN QUERY BrwOvBuffer FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOvBuffer OvBuffer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOvBuffer OvBuffer


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 tbOvBuffer Merknad first_tbOvBuffer ~
prev_tbOvBuffer next_tbOvBuffer last_tbOvBuffer copy_tbOvBuffer ~
delete_tbOvBuffer refresh_tbOvBuffer excel_tbOvBuffer ~
LeggTilVare_tbOvBuffer Etikett_tbOvBuffer close_tbOvBuffer cbTilBut ~
fiStrekkode BrwOvBuffer 
&Scoped-Define DISPLAYED-OBJECTS BuntNr Merknad cbFraBut cbTilBut ~
fiStrekkode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON close_tbOvBuffer 
     IMAGE-UP FILE "bmp/e-exit.bmp":U
     LABEL "Close" 
     SIZE 4.6 BY 1.1 TOOLTIP "Close (ALT-C)".

DEFINE BUTTON copy_tbOvBuffer 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbOvBuffer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON Etikett_tbOvBuffer 
     LABEL "Etikett" 
     SIZE 8 BY 1.1 TOOLTIP "Utskrift av etikett".

DEFINE BUTTON excel_tbOvBuffer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON first_tbOvBuffer 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbOvBuffer 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON LeggTilVare_tbOvBuffer 
     LABEL "Legg til vare" 
     SIZE 14 BY 1.1 TOOLTIP "Legger inn vare fra varesøk.".

DEFINE BUTTON next_tbOvBuffer 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbOvBuffer 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbOvBuffer 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbFraBut AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Butikk fra/til" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "16",16
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Butikk det overføres fra." NO-UNDO.

DEFINE VARIABLE cbTilBut AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Butikk det skal overføres til" NO-UNDO.

DEFINE VARIABLE BuntNr AS INTEGER FORMAT ">>>>>>>>>9":U INITIAL 0 
     LABEL "Buntnr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "Overføringens unike id." NO-UNDO.

DEFINE VARIABLE fiStrekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 38.8 BY 1 NO-UNDO.

DEFINE VARIABLE Merknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 103 BY 1 TOOLTIP "Kort merknad til overføringen." NO-UNDO.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 183 BY 3.1.

DEFINE RECTANGLE tbOvBuffer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 183 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwOvBuffer FOR 
      OvBuffer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwOvBuffer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOvBuffer C-Win _STRUCTURED
  QUERY BrwOvBuffer NO-LOCK DISPLAY
      OvBuffer.LinjeNr COLUMN-LABEL "LinjeNr" FORMAT ">>>>>>>>>9":U
      OvBuffer.ovbuffer_Beskr COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      OvBuffer.ovbuffer_LevKod COLUMN-LABEL "Lev.art.nr" FORMAT "X(20)":U
      OvBuffer.ovbuffer_LevFargKod COLUMN-LABEL "Fargekode" FORMAT "X(20)":U
      OvBuffer.Storl COLUMN-LABEL "Størrelse" FORMAT "x(10)":U
      OvBuffer.ovbuffer_Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      OvBuffer.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9.99":U
      OvBuffer.ButikkNrTil COLUMN-LABEL "Til butikk" FORMAT ">>>>>9":U
      OvBuffer.ovbuffer_TilButNavn COLUMN-LABEL "Navn" FORMAT "X(20)":U
      OvBuffer.ovbuffer_DatoTidEndret COLUMN-LABEL "Endret" FORMAT "X(17)":U
      OvBuffer.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      OvBuffer.ovbuffer_DatoTidRegistrert COLUMN-LABEL "Registrert" FORMAT "X(17)":U
      OvBuffer.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
            WIDTH 97.4
      OvBuffer.BuntNr COLUMN-LABEL "BuntNr" FORMAT ">>>>>>>>>9":U
      OvBuffer.ButikkNrFra COLUMN-LABEL "Fra butikknummer" FORMAT ">>>>>9":U
      OvBuffer.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      OvBuffer.ovbuffer_BuntStatus COLUMN-LABEL "BuntStatus" FORMAT "X(6)":U
  ENABLE
      OvBuffer.LinjeNr HELP "Linjenummer"
      OvBuffer.BuntNr HELP "Buntnummer."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 183 BY 14.29 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BuntNr AT ROW 1.95 COL 26 COLON-ALIGNED
     Merknad AT ROW 2.91 COL 26 COLON-ALIGNED
     first_tbOvBuffer AT ROW 4.81 COL 2.2 WIDGET-ID 54
     prev_tbOvBuffer AT ROW 4.81 COL 7 WIDGET-ID 56
     next_tbOvBuffer AT ROW 4.81 COL 11.6 WIDGET-ID 58
     last_tbOvBuffer AT ROW 4.81 COL 16.2 WIDGET-ID 60
     copy_tbOvBuffer AT ROW 4.81 COL 20.8 WIDGET-ID 70
     delete_tbOvBuffer AT ROW 4.81 COL 25.4 WIDGET-ID 62
     refresh_tbOvBuffer AT ROW 4.81 COL 30 WIDGET-ID 66
     excel_tbOvBuffer AT ROW 4.81 COL 34.6 WIDGET-ID 68
     LeggTilVare_tbOvBuffer AT ROW 4.81 COL 39.2 WIDGET-ID 72
     Etikett_tbOvBuffer AT ROW 4.81 COL 53.2 WIDGET-ID 74
     close_tbOvBuffer AT ROW 4.81 COL 61 WIDGET-ID 76
     cbFraBut AT ROW 6.19 COL 96 COLON-ALIGNED WIDGET-ID 46
     cbTilBut AT ROW 6.19 COL 112.6 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fiStrekkode AT ROW 6.19 COL 142 COLON-ALIGNED
     BrwOvBuffer AT ROW 7.43 COL 2 WIDGET-ID 200
     RECT-7 AT ROW 1.48 COL 2 WIDGET-ID 6
     tbOvBuffer AT ROW 4.71 COL 2 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 185.2 BY 20.76 WIDGET-ID 100.


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
         TITLE              = "Registrering av varelinjer for internoverføring"
         HEIGHT             = 20.76
         WIDTH              = 185.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 185.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 185.2
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
/* BROWSE-TAB BrwOvBuffer fiStrekkode DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 20.76
       FRAME DEFAULT-FRAME:WIDTH            = 185.2.

ASSIGN 
       OvBuffer.ButikkNrFra:VISIBLE IN BROWSE BrwOvBuffer = FALSE.

/* SETTINGS FOR FILL-IN BuntNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BuntNr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR COMBO-BOX cbFraBut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbOvBuffer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,copy;Kopier,delete;Slett,refresh;Refresh,excel;Eksporter til E&xcel,LeggTilVare;Legg til vare¤enable,Etikett;Etikett,close;Closemaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOvBuffer
/* Query rebuild information for BROWSE BrwOvBuffer
     _TblList          = "SkoTex.OvBuffer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"OvBuffer.LinjeNr" "LinjeNr" ">>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? yes "Linjenummer" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"OvBuffer.ovbuffer_Beskr" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"OvBuffer.ovbuffer_LevKod" "Lev.art.nr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"OvBuffer.ovbuffer_LevFargKod" "Fargekode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"OvBuffer.Storl" "Størrelse" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse som skal overføres" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"OvBuffer.ovbuffer_Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"OvBuffer.Antall" "Antall" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Antall par som skal overøres" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"OvBuffer.ButikkNrTil" "Til butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk det overføres til" no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"OvBuffer.ovbuffer_TilButNavn" "Navn" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"OvBuffer.ovbuffer_DatoTidEndret" "Endret" "X(17)" "CHARACTER" ? ? ? ? ? ? no "" no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"OvBuffer.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"OvBuffer.ovbuffer_DatoTidRegistrert" "Registrert" "X(17)" "CHARACTER" ? ? ? ? ? ? no "" no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"OvBuffer.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "97.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"OvBuffer.BuntNr" "BuntNr" ">>>>>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Buntnummer." no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"OvBuffer.ButikkNrFra" "Fra butikknummer" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk det overføres fra" no no "16.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"OvBuffer.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"OvBuffer.ovbuffer_BuntStatus" "BuntStatus" "X(6)" "CHARACTER" ? ? ? ? ? ? no "" no no "10.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOvBuffer */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrering av varelinjer for internoverføring */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
  DO:
    /* This case occurs when the user presses the "Esc" key.
       In a persistently run window, just ignore this.  If we did not, the
       application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrering av varelinjer for internoverføring */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbTilBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbTilBut C-Win
ON VALUE-CHANGED OF cbTilBut IN FRAME DEFAULT-FRAME
DO:
    DO WITH FRAME DEFAULT-FRAME:
      ASSIGN 
        cbTilBut = INT(cbTilBut:SCREEN-VALUE)
        .
    END.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStrekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStrekkode C-Win
ON LEAVE OF fiStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
    IF fiStrekkode:SCREEN-VALUE <> '' THEN 
    DO:
      IF NOT JBoxServerAPI:Instance:Find("Strekkode", "WHERE Kode = '"+ fiStrekkode:SCREEN-VALUE + "'") THEN
      DO:
        JBoxSession:Instance:ViewMessage("Ukjent strekkode!").
        RETURN.
      END.  
      IF JBoxServerApi:Instance:CallServerProc("ovbuffer_NyttLinjeNr.p", 
        STRING(iBuntNr)
        ) THEN 
        iLinjeNr = INT(JBoxServerApi:Instance:getCallReturnParam()).
      IF NOT JBoxServerApi:Instance:CallServerProc("ovbuffer_NyFraStrekkode.p",  
        STRING(iBuntNr) + "|" + fiStrekkode:SCREEN-VALUE + '|' + STRING(iLinjeNr) + '|' + cbFraBut:SCREEN-VALUE + "|" + cbTilBut:SCREEN-VALUE
        ) THEN 
      DO: 
        JBoxSession:Instance:ViewMessage("Feil ved opprettelse av linje: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
      IF NUM-ENTRIES(JBoxServerApi:Instance:getCallReturnParam(),'|') = 2 THEN 
      DO:
        ASSIGN 
          iBuntNr  = INT(ENTRY(1,JBoxServerApi:Instance:getCallReturnParam(),'|'))
          iLinjeNr = INT(ENTRY(2,JBoxServerApi:Instance:getCallReturnParam(),'|'))
          .
        FIND FIRST v_OvBuffer NO-LOCK WHERE 
          v_Ovbuffer.BuntNr = iBuntNr AND 
          v_Ovbuffer.LinjeNr = iLinjeNr NO-ERROR.
        
        IF AVAILABLE v_Ovbuffer THEN
        DO:
          oBrwOvBuffer:BROWSE-HANDLE:QUERY:REPOSITION-TO-ROWID(ROWID(v_Ovbuffer)).        
          oBrwOvBuffer:refreshRow(). /* for å friske opp raden. */
          oBrwOvBuffer:displayRecord(). /* For å friske opp update feltet hvis dette er aktivt på raden. */
        END.
        ELSE
          oBrwOvBuffer:OpenQuery().
      END.
      ELSE
        oBrwOvBuffer:OpenQuery().
      ASSIGN 
        fiStrekkode:SCREEN-VALUE = ''
        fiStrekkode              = ''
        .
      APPLY 'ENTRY' TO fiStrekkode IN FRAME DEFAULT-FRAME.
      RETURN NO-APPLY. /* NB: Dette er nødvendig for at ikke 'ENTRY' ovenfor skal overstyres av progress. */
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStrekkode C-Win
ON RETURN OF fiStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
    APPLY 'LEAVE' TO fiStrekkode.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwOvBuffer
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
ON CLOSE OF THIS-PROCEDURE 
  DO:
    IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
    DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
    PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
    RUN disable_UI.
  END.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
{incl/wintrigg.i}
{incl/conttrigg.i oBrwOvBuffer:BROWSE-HANDLE}
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

  IF NOT THIS-PROCEDURE:PERSISTENT THEN 
  DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    IF NOT JBoxServerApi:Instance:CallServerProc("ovbuffer_NyttLinjeNr.p", 
                                             STRING(OvBuffer.BuntNr)
                                             ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved kopiering av linje: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.  
    ELSE DO:
      ASSIGN 
        iLinjeNr = INT(JBoxServerApi:Instance:getCallReturnParam())
        .  
    END.

    IF NOT JBoxServerApi:Instance:CallServerProc("ovbuffer_kopier.p", 
                                             STRING(OvBuffer.BuntNr) + "|" + STRING(OvBuffer.LinjeNr) + "|" + STRING(iLinjeNr)
                                             ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved kopiering av linje: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END. 
  END.
/*  RUN SUPER.*/
  DO WITH FRAME {&FRAME-NAME}:
  END.
  oBrwOvBuffer:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  /* Slette rad. */
  IF otbOvBuffer:isCurrent THEN
  DO:
  END.
  
  RUN SUPER.

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
  DISPLAY BuntNr Merknad cbFraBut cbTilBut fiStrekkode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-7 tbOvBuffer Merknad first_tbOvBuffer prev_tbOvBuffer 
         next_tbOvBuffer last_tbOvBuffer copy_tbOvBuffer delete_tbOvBuffer 
         refresh_tbOvBuffer excel_tbOvBuffer LeggTilVare_tbOvBuffer 
         Etikett_tbOvBuffer close_tbOvBuffer cbTilBut fiStrekkode BrwOvBuffer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
IF otbOvBuffer:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}: 
    IF oBrwOvbuffer:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
    DO:
      IF AVAILABLE Ovbuffer THEN 
        RUN enkeltEtikett.w (INPUT ovbuffer_Kode, 
                             INPUT OvBuffer.Antall, 
                             INPUT INT(cbFraBut:SCREEN-VALUE),
                             INPUT 0 /* KampanjeId = 0 */
                             ).
    END.
    ELSE DO:
      JBoxSession:Instance:ViewMessage("Marker EN rad for etikettutskrift! ").
      RETURN.
    END.
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
DEF VAR oBrwFillInAntall AS JBoxBrowseFillIn NO-UNDO.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF
  DO WITH FRAME {&FRAME-NAME}:
    SUBSCRIBE "msgFraLagerListeButArtStr" ANYWHERE.
    SUBSCRIBE "OpenQueryOverfor" ANYWHERE.

    oBrwOvBuffer = NEW JBoxBrowse(brwOvBuffer:HANDLE).
    oBrwOvBuffer:baseQuery = "WHERE BuntNr = '" + STRING(iBuntNr) + "'".
    otbOvBuffer = NEW JBoxToolbar(tbOvBuffer:HANDLE).
    oBrwOvBuffer:TOOLBAR-OBJECT = otbOvBuffer.

    hAntallColumn = oBrwOvBuffer:getColumnHandle("Antall").
    oBrwFillInAntall = NEW JBoxBrowseFillIn(oBrwOvBuffer,"Antall",TRUE).
    oBrwOvBuffer:enabledColumns = "Antall".

    IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 3") THEN
      ASSIGN 
        iNettbutLager = INT(JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1"))
        .
    IF iOpphav = 10 /* Reservasjon */ THEN 
    RESERVASJON:
    DO:
      IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 11 and SysGr = 7 and ParaNr = 2") THEN
        cOvButLst = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter2").
      IF cOvButLst <> '' THEN 
          ASSIGN 
            cbTilBut:LIST-ITEMS = cOvButLst
            NO-ERROR.
      IF JBoxServerAPI:Instance:Find("OvBunt","WHERE BuntNr = '" + STRING(iBuntNr) + "'") THEN
      DO:
        cbFraBut:LIST-ITEM-PAIRS = JBoxServerAPI:Instance:FieldValue("OvBunt.FrabutNr") + ',' + JBoxServerAPI:Instance:FieldValue("OvBunt.FrabutNr").
        cbFraBut:SENSITIVE = FALSE.
        cbTilBut = INT(JBoxServerAPI:Instance:FieldValue("OvBunt.TilbutNr")).
        cbFraBut = INT(JBoxServerAPI:Instance:FieldValue("OvBunt.FrabutNr")).
        cbTilBut:SENSITIVE = FALSE.
        Merknad:SENSITIVE = FALSE.
      END.
    END. /* RESERVASJON */

    ELSE OVERFORING: DO: 
      cbFraBut = iNettButLager. 
      IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 11 and SysGr = 7 and ParaNr = 5") THEN
        cOvButLst = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter2").
      IF cOvButLst <> '' THEN 
          ASSIGN 
            cbTilBut = INT(ENTRY(11,cOvButLst))
            NO-ERROR.
          ASSIGN 
            cbTilBut:LIST-ITEMS = cOvButLst
            NO-ERROR.
      IF JBoxServerAPI:Instance:Find("FIRST OvBunt","WHERE BuntNr = '" + STRING(iBuntNr) + "'") THEN
      DO:
        cbTilBut = INT(JBoxServerAPI:Instance:FieldValue("OvBunt.TilbutNr")).
        IF INT(cbTilBut) > 0 THEN 
          cbTilBut:SENSITIVE = FALSE.
        cbFraBut = INT(JBoxServerAPI:Instance:FieldValue("OvBunt.FrabutNr")).
        ASSIGN         
          cbFraBut:SCREEN-VALUE = STRING(cbFraBut)
          cbTilBut:SCREEN-VALUE = STRING(cbTilBut)
          .
      END.
    END. /* OVERFORING */
    
    ASSIGN 
      Merknad = cMerknad
      BuntNr  = iBuntNr
      .
    DISPLAY
      Merknad
      BuntNr
      .
  DO:
    opopupOvbuffer = NEW JBoxPopupMenu().
    opopupOvBuffer:AddToolGroup('OppslagModell;Vis i modell liste').
    oBrwOvBuffer:POPUP-MENU-OBJECT = opopupOvBuffer.
  END.
      
  END.
  oBrwOvBuffer:OpenQuery().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggTilVareRecord C-Win 
PROCEDURE LeggTilVareRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbOvBuffer:isCurrent THEN 
    DO:
      RAD: 
      DO:
        IF INT(cbTilBut:SCREEN-VALUE) = 0 OR cbTilBut:SCREEN-VALUE = ? THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Mottagende butikk må være angitt før varelinjer kan legges inn.").
          RETURN NO-APPLY.          
        END.
        THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
        ASSIGN 
          cVareFelt    = ''
          cVareVerdier = ''
          .

        IF iOpphav = 5 THEN 
          RUN LagerListeButArtStr.w ('16|50||' + STRING(iBuntNr) + '|' + cbTilBut:SCREEN-VALUE ).
        ELSE IF iOpphav = 10 THEN 
          RUN LagerListeButArtStr.w ('16|70||' + STRING(iBuntNr) + '|' + cbTilBut:SCREEN-VALUE ).
        THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

        /* TN 7/10-19 Etter råd fra Brynjar. WorkAroound for bug - Se mail. */
        /*  oContainer:unFreezeContainer(). NB: virker ikke. har en bug. Brynjar ser pådet. */
        APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
        RUN MoveToTop.
        oBrwOvBuffer:openQuery().
      END. /* RAD */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msgFraLagerListeButArtStr C-Win 
PROCEDURE msgFraLagerListeButArtStr :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER icVareFelt AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER icVareVerdier AS CHARACTER NO-UNDO.
  
  ASSIGN 
    cVareFelt    = icVareFelt
    cVareVerdier = icVareVerdier
    . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
  DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
  DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO.

  IF ihBuffer:BUFFER-FIELD("ovbuffer_BuntStatus"):BUFFER-VALUE <> '10' THEN
  DO:
      JBoxSession:Instance:ViewMessage("Det kan ikke gøres endringer på ordre med denne status.").
      RETURN NO-APPLY.
  END.

  DYNAMIC-FUNCTION("setPostUpdProc","ovbuffer_post_update.p").
  obOK = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,"",
              "",
              ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
              DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
              ihFillIn:SCREEN-VALUE,
              TRUE).
/*   Gør refresh på moder raden - Kordrehode.*/
/*  DYNAMIC-FUNCTION("RefreshRowids",oBrwBunt:BROWSE-HANDLE,STRING(oBrwKOrdreLinje:PARENT-BUFFER-HANDLE:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)).*/
/*  oBrwKOrdreLinje:refreshRow().*/
  oBrwOvBuffer:refresh().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}: 
  
/*  /* Her opprettes en tom rad. */                                                                                        */
/*  RUN SUPER.                                                                                                             */
/*                                                                                                                         */
/*  IF otbOvBuffer:isCurrent THEN                                                                                          */
/*  DO:                                                                                                                    */
/*    CURRENT-WINDOW:SENSITIVE = FALSE.                                                                                    */
/*    RUN LagerListeButArtStr.w ('16|30|').                                                                                */
/*    CURRENT-WINDOW:SENSITIVE = TRUE.                                                                                     */
/*    IF cVareFelt = '' THEN                                                                                               */
/*    DO:                                                                                                                  */
/*      IF AVAILABLE OvBuffer THEN                                                                                         */
/*        RUN DeleteRecord.                                                                                                */
/*      RUN MoveToTop.                                                                                                     */
/*      RETURN.                                                                                                            */
/*    END.                                                                                                                 */
/*                                                                                                                         */
/*    /* TN 31/5-19 Brynjar fikset problemet med validering. Bruker derfor standard kallet. */                             */
/*    IF NOT JBoxServerApi:Instance:Update("OvBuffer",                                                                     */
/*                                  OvBuffer.RowIdent1,                                                                    */
/*                                  "ButikkNrFra,ButikkNrTil,Antall,ArtikkelNr,Storl",                                     */
/*                                  cbFraBut:SCREEN-VALUE + "|" + cbTilBut:SCREEN-VALUE + "|1|" + cVareVerdier,            */
/*                                  FALSE,                                                                                 */
/*                                  "ovbuffer_post_update.p",                                                              */
/*                                  TRUE) THEN                                                                             */
/*      DO:                                                                                                                */
/*          JBoxSession:Instance:ViewMessage("Feil ved opprettelse av rad pga " + JBoxServerAPI:Instance:getCallMessage()).*/
/*          RETURN.                                                                                                        */
/*      END.                                                                                                               */
/*                                                                                                                         */
/*    ASSIGN                                                                                                               */
/*      cVareFelt = ''                                                                                                     */
/*      cVareVerdier = ''                                                                                                  */
/*      .                                                                                                                  */
/*    oBrwOvBuffer:OpenQuery().                                                                                            */
/*    APPLY 'ENTRY' TO fiStrekkode IN FRAME DEFAULT-FRAME.                                                                 */
/*    RUN MoveToTop.                                                                                                       */
/*    RETURN NO-APPLY. /* NB: Dette er nødvendig for at ikke 'ENTRY' ovenfor skal overstyres av progress. */               */
/*  END.                                                                                                                   */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryOverfor C-Win 
PROCEDURE OpenQueryOverfor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  oBrwOvbuffer:openQuery().

  /* TN 7/10-19 Etter råd fra Brynjar. WorkAroound for bug - Se mail. */
/*  oContainer:unFreezeContainer(). NB: virker ikke. har en bug. Brynjar ser pådet. */
  APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppslagModellRecord C-Win 
PROCEDURE OppslagModellRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE OvBuffer THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',OvBuffer.OvBuffer_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',OvBuffer.OvBuffer_LevFargKod)
        .
      DO ON ERROR UNDO, LEAVE:  
        oContainer:StartTabWindow('LagerListeModButStr.w', 'Modelliste', FALSE, YES).
        PUBLISH 'settModellFilter' (cFilterTekst).
      END.
    END.
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

