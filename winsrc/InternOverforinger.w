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

DEFINE VARIABLE bOk         AS LOG    NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBrowse     AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
DEFINE VARIABLE hToolbar    AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE iNettButLager AS INTEGER NO-UNDO.
DEFINE VARIABLE cVareFelt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVareVerdier AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAntallColumn AS HANDLE NO-UNDO. 
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iBuntNr AS INTEGER NO-UNDO.
DEF VAR opopupOvBuffer AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwOvBunt ***/
DEFINE VARIABLE oBrwOvBunt AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE OvBunt
    FIELD BuntNr AS INTEGER
    FIELD FraButNr AS INTEGER
    FIELD Merknad AS CHARACTER
    FIELD opphav AS INTEGER
    FIELD ovbunt_OpphavTekst AS CHARACTER
    FIELD ovbunt_Opprettet AS CHARACTER
    FIELD RegistrertAv AS CHARACTER
    FIELD BatchNr AS INTEGER
    FIELD ovbunt_DatoTidOppdatert AS CHARACTER
    FIELD OppdatertAv AS CHARACTER
    FIELD ovbunt_FakturaNr AS DECIMAL
    FIELD Faktura_Id AS DECIMAL
    FIELD ovbunt_DatotidFakturert AS CHARACTER
    FIELD PlListeId AS DECIMAL
    FIELD BuntStatus AS INTEGER
    FIELD EDato AS DATE
    FIELD ETid AS INTEGER
    FIELD BrukerID AS CHARACTER
    FIELD FakturertDato AS DATE
    FIELD FakturertTid AS INTEGER
    FIELD DatoOppdatert AS DATE
    FIELD TidOppdatert AS INTEGER
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD ovbunt_Dummy AS CHARACTER
    FIELD TilbutNr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_OvBunt FOR TEMP-TABLE OvBunt.


FUNCTION getBuffersAndFieldsBrwOvBunt RETURNS CHARACTER():
  RETURN
    'OvBunt'
     + ';BuntNr'
     + ';FraButNr'
     + ';Merknad'
     + ';opphav'
     + ';RegistrertAv'
     + ';BatchNr'
     + ';OppdatertAv'
     + ';Faktura_Id'
     + ';PlListeId'
     + ';BuntStatus'
     + ';EDato'
     + ';ETid'
     + ';BrukerID'
     + ';FakturertDato'
     + ';FakturertTid'
     + ';DatoOppdatert'
     + ';TidOppdatert'
     + ';RegistrertDato'
     + ';RegistrertTid'
     + ';TilbutNr'
     + ';+ovbunt_OpphavTekst|CHARACTER||ovbunt_OpphavTekst|Fra modul'
     + ';+ovbunt_Opprettet|CHARACTER||ovbunt_Opprettet|Registrert'
     + ';+ovbunt_DatoTidOppdatert|CHARACTER||ovbunt_DatoTidOppdatert|Oppdatert'
     + ';+ovbunt_FakturaNr|DECIMAL||ovbunt_FakturaNr|Fakturanr'
     + ';+ovbunt_DatotidFakturert|CHARACTER||ovbunt_DatotidFakturert|Fakturert'
     + ';+ovbunt_Dummy|CHARACTER||ovbunt_Dummy|.'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOvBunt RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwOvBunt RETURNS CHARACTER():
  RETURN 
     'server/ovbunt_brwcalc.p' /* ovbunt_OpphavTekst */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_Opprettet */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_DatoTidOppdatert */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_FakturaNr */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_DatotidFakturert */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_Dummy */
     .
END FUNCTION.
DEFINE VARIABLE otbOvBunt AS JBoxToolbar NO-UNDO.
DEFINE VARIABLE otbOvBuffer AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwOvBuffer ***/
DEFINE VARIABLE oBrwOvBuffer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE OvBuffer
    FIELD BuntNr AS INTEGER
    FIELD LinjeNr AS DECIMAL
    FIELD ArtikkelNr AS DECIMAL
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
    FIELD ButikkNrFra AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_OvBuffer FOR TEMP-TABLE OvBuffer.
DEFINE BUFFER bufOvbuffer FOR TEMP-TABLE OvBuffer.

FUNCTION getBuffersAndFieldsBrwOvBuffer RETURNS CHARACTER():
  RETURN
    'OvBuffer'
     + ';BuntNr'
     + ';LinjeNr'
     + ';ArtikkelNr'
     + ';Storl'
     + ';Antall'
     + ';ButikkNrTil'
     + ';BrukerID'
     + ';RegistrertAv'
     + ';ButikkNrFra'
     + ';+ovbuffer_Beskr|CHARACTER||ovbuffer_Beskr|Varetekst'
     + ';+ovbuffer_LevKod|CHARACTER||ovbuffer_LevKod|Lev.art.nr'
     + ';+ovbuffer_LevFargKod|CHARACTER||ovbuffer_LevFargKod|Farge'
     + ';+ovbuffer_Kode|CHARACTER||ovbuffer_Kode|Strekkode'
     + ';+ovbuffer_TilButNavn|CHARACTER||ovbuffer_TilButNavn|Butikk navn'
     + ';+ovbuffer_DatoTidEndret|CHARACTER||ovbuffer_DatoTidEndret|Endret'
     + ';+ovbuffer_DatoTidRegistrert|CHARACTER||ovbuffer_DatoTidRegistrert|Opprettet'
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
&Scoped-define BROWSE-NAME BrwOvBuffer

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES OvBuffer OvBunt

/* Definitions for BROWSE BrwOvBuffer                                   */
&Scoped-define FIELDS-IN-QUERY-BrwOvBuffer OvBuffer.BuntNr OvBuffer.LinjeNr ~
OvBuffer.ArtikkelNr OvBuffer.ovbuffer_Beskr OvBuffer.ovbuffer_LevKod ~
OvBuffer.ovbuffer_LevFargKod OvBuffer.Storl OvBuffer.ovbuffer_Kode ~
OvBuffer.Antall OvBuffer.ButikkNrTil OvBuffer.ovbuffer_TilButNavn ~
OvBuffer.ovbuffer_DatoTidEndret OvBuffer.BrukerID ~
OvBuffer.ovbuffer_DatoTidRegistrert OvBuffer.RegistrertAv ~
OvBuffer.ButikkNrFra 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOvBuffer OvBuffer.BuntNr 
&Scoped-define QUERY-STRING-BrwOvBuffer FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOvBuffer OPEN QUERY BrwOvBuffer FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOvBuffer OvBuffer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOvBuffer OvBuffer


/* Definitions for BROWSE BrwOvBunt                                     */
&Scoped-define FIELDS-IN-QUERY-BrwOvBunt OvBunt.BuntNr OvBunt.FraButNr ~
OvBunt.Merknad OvBunt.opphav OvBunt.ovbunt_OpphavTekst ~
OvBunt.ovbunt_Opprettet OvBunt.RegistrertAv OvBunt.BatchNr ~
OvBunt.ovbunt_DatoTidOppdatert OvBunt.OppdatertAv OvBunt.ovbunt_FakturaNr ~
OvBunt.Faktura_Id OvBunt.ovbunt_DatotidFakturert OvBunt.PlListeId ~
OvBunt.BuntStatus OvBunt.EDato OvBunt.ETid OvBunt.BrukerID ~
OvBunt.FakturertDato OvBunt.FakturertTid OvBunt.DatoOppdatert ~
OvBunt.TidOppdatert OvBunt.RegistrertDato OvBunt.RegistrertTid ~
OvBunt.ovbunt_Dummy OvBunt.TilbutNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOvBunt OvBunt.BuntNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwOvBunt OvBunt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwOvBunt OvBunt
&Scoped-define QUERY-STRING-BrwOvBunt FOR EACH OvBunt NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOvBunt OPEN QUERY BrwOvBunt FOR EACH OvBunt NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOvBunt OvBunt
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOvBunt OvBunt


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RegistrerLinjer_tbOvBuffer ~
Etikett_tbOvBuffer btnSplitBarY first_tbOvBunt prev_tbOvBunt next_tbOvBunt ~
last_tbOvBunt new_tbOvBunt delete_tbOvBunt filter_tbOvBunt refresh_tbOvBunt ~
excel_tbOvBunt BrwOvBunt first_tbOvBuffer prev_tbOvBuffer next_tbOvBuffer ~
last_tbOvBuffer refresh_tbOvBuffer excel_tbOvBuffer BrwOvBuffer tbOvBunt ~
searchOvBunt tbOvBuffer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 172 BY .43.

DEFINE BUTTON delete_tbOvBunt 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON Etikett_tbOvBuffer 
     LABEL "Etikett" 
     SIZE 8 BY 1.1 TOOLTIP "Etikettutskrift".

DEFINE BUTTON excel_tbOvBuffer 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON excel_tbOvBunt 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbOvBunt 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbOvBuffer 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON first_tbOvBunt 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbOvBuffer 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON last_tbOvBunt 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON new_tbOvBunt 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbOvBuffer 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON next_tbOvBunt 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbOvBuffer 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON prev_tbOvBunt 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbOvBuffer 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON refresh_tbOvBunt 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON RegistrerLinjer_tbOvBuffer 
     LABEL "Registrer/endre linjer" 
     SIZE 25.2 BY 1.1 TOOLTIP "Registrering av varelinjer".

DEFINE RECTANGLE searchOvBunt
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28.2 BY .91.

DEFINE RECTANGLE tbOvBuffer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 172 BY 1.29.

DEFINE RECTANGLE tbOvBunt
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 172.2 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwOvBuffer FOR 
      OvBuffer SCROLLING.

DEFINE QUERY BrwOvBunt FOR 
      OvBunt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwOvBuffer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOvBuffer C-Win _STRUCTURED
  QUERY BrwOvBuffer NO-LOCK DISPLAY
      OvBuffer.BuntNr COLUMN-LABEL "BuntNr" FORMAT "->,>>>,>>9":U
      OvBuffer.LinjeNr COLUMN-LABEL "LinjeNr" FORMAT ">>>>>>>>>9":U
      OvBuffer.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      OvBuffer.ovbuffer_Beskr COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      OvBuffer.ovbuffer_LevKod COLUMN-LABEL "Lev.art.nr" FORMAT "X(25)":U
      OvBuffer.ovbuffer_LevFargKod COLUMN-LABEL "Farge" FORMAT "X(25)":U
      OvBuffer.Storl COLUMN-LABEL "Størrelse" FORMAT "x(10)":U
      OvBuffer.ovbuffer_Kode COLUMN-LABEL "Strekkode" FORMAT "X(18)":U
      OvBuffer.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9.99":U
      OvBuffer.ButikkNrTil COLUMN-LABEL "Til butikk" FORMAT ">>>>>9":U
            WIDTH 9
      OvBuffer.ovbuffer_TilButNavn COLUMN-LABEL "Butikk navn" FORMAT "X(25)":U
      OvBuffer.ovbuffer_DatoTidEndret COLUMN-LABEL "Endret" FORMAT "X(18)":U
      OvBuffer.BrukerID COLUMN-LABEL "Endret av" FORMAT "X(10)":U
      OvBuffer.ovbuffer_DatoTidRegistrert COLUMN-LABEL "Opprettet" FORMAT "X(18)":U
      OvBuffer.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      OvBuffer.ButikkNrFra COLUMN-LABEL "Fra butikknummer" FORMAT ">>>>>9":U
  ENABLE
      OvBuffer.BuntNr HELP "Buntnummer."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 172 BY 8.86 FIT-LAST-COLUMN.

DEFINE BROWSE BrwOvBunt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOvBunt C-Win _STRUCTURED
  QUERY BrwOvBunt NO-LOCK DISPLAY
      OvBunt.BuntNr COLUMN-LABEL "BuntNr" FORMAT "->,>>>,>>9":U
      OvBunt.FraButNr FORMAT ">>>>>9":U
      OvBunt.Merknad COLUMN-LABEL "Merknad" FORMAT "X(30)":U
      OvBunt.opphav COLUMN-LABEL "Opphav" FORMAT ">9":U
      OvBunt.ovbunt_OpphavTekst COLUMN-LABEL "Fra modul" FORMAT "X(20)":U
      OvBunt.ovbunt_Opprettet COLUMN-LABEL "Registrert" FORMAT "X(18)":U
      OvBunt.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      OvBunt.BatchNr COLUMN-LABEL "Batch" FORMAT "zzzzzzzz9":U
            WIDTH 9.2
      OvBunt.ovbunt_DatoTidOppdatert COLUMN-LABEL "Oppdatert" FORMAT "X(18)":U
      OvBunt.OppdatertAv COLUMN-LABEL "Oppdatert av" FORMAT "X(15)":U
      OvBunt.ovbunt_FakturaNr COLUMN-LABEL "Fakturanr" FORMAT ">>>>>>>>>>>>9":U
            WIDTH 10.8
      OvBunt.Faktura_Id COLUMN-LABEL "FId" FORMAT ">>>>>>>>>>>>9":U
      OvBunt.ovbunt_DatotidFakturert COLUMN-LABEL "Fakturert" FORMAT "X(18)":U
      OvBunt.PlListeId COLUMN-LABEL "Pl.lst.id" FORMAT ">>>>>>>9":U
      OvBunt.BuntStatus COLUMN-LABEL "Status" FORMAT "->,>>>,>>9":U
      OvBunt.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      OvBunt.ETid COLUMN-LABEL "ETid" FORMAT "->,>>>,>>9":U
      OvBunt.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      OvBunt.FakturertDato COLUMN-LABEL "Fakturert" FORMAT "99/99/99":U
      OvBunt.FakturertTid COLUMN-LABEL "Kl" FORMAT "->,>>>,>>9":U
      OvBunt.DatoOppdatert COLUMN-LABEL "Oppdatert" FORMAT "99/99/99":U
      OvBunt.TidOppdatert COLUMN-LABEL "Tidspunkt oppdatert" FORMAT "->,>>>,>>9":U
      OvBunt.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      OvBunt.RegistrertTid COLUMN-LABEL "Registreringstidspunkt" FORMAT "->,>>>,>>9":U
      OvBunt.ovbunt_Dummy COLUMN-LABEL "." FORMAT "X(1)":U
      OvBunt.TilbutNr FORMAT ">>>>>9":U
  ENABLE
      OvBunt.BuntNr HELP "Buntnummer."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 172.2 BY 10.71 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RegistrerLinjer_tbOvBuffer AT ROW 15.19 COL 30.2 WIDGET-ID 58
     Etikett_tbOvBuffer AT ROW 15.19 COL 55.4 WIDGET-ID 56
     btnSplitBarY AT ROW 14.57 COL 1.8 WIDGET-ID 18
     first_tbOvBunt AT ROW 1.29 COL 2 WIDGET-ID 4
     prev_tbOvBunt AT ROW 1.29 COL 6.8 WIDGET-ID 6
     next_tbOvBunt AT ROW 1.29 COL 11.4 WIDGET-ID 8
     last_tbOvBunt AT ROW 1.29 COL 16 WIDGET-ID 10
     new_tbOvBunt AT ROW 1.29 COL 20.6 WIDGET-ID 20
     delete_tbOvBunt AT ROW 1.29 COL 25.2 WIDGET-ID 24
     filter_tbOvBunt AT ROW 1.29 COL 29.8 WIDGET-ID 12
     refresh_tbOvBunt AT ROW 1.29 COL 34.4 WIDGET-ID 22
     excel_tbOvBunt AT ROW 1.29 COL 39 WIDGET-ID 14
     BrwOvBunt AT ROW 3.86 COL 1.8 WIDGET-ID 200
     first_tbOvBuffer AT ROW 15.19 COL 2.4 WIDGET-ID 50
     prev_tbOvBuffer AT ROW 15.19 COL 7.2 WIDGET-ID 30
     next_tbOvBuffer AT ROW 15.19 COL 11.8 WIDGET-ID 32
     last_tbOvBuffer AT ROW 15.19 COL 16.4 WIDGET-ID 34
     refresh_tbOvBuffer AT ROW 15.19 COL 21 WIDGET-ID 52
     excel_tbOvBuffer AT ROW 15.19 COL 25.6 WIDGET-ID 44
     BrwOvBuffer AT ROW 16.43 COL 2 WIDGET-ID 400
     tbOvBunt AT ROW 1.19 COL 1.8 WIDGET-ID 2
     searchOvBunt AT ROW 2.71 COL 1.8 WIDGET-ID 16
     tbOvBuffer AT ROW 15.1 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 173.2 BY 24.33 WIDGET-ID 100.


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
         HEIGHT             = 24.33
         WIDTH              = 173.2
         MAX-HEIGHT         = 24.33
         MAX-WIDTH          = 173.2
         VIRTUAL-HEIGHT     = 24.33
         VIRTUAL-WIDTH      = 173.2
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
/* BROWSE-TAB BrwOvBunt excel_tbOvBunt DEFAULT-FRAME */
/* BROWSE-TAB BrwOvBuffer excel_tbOvBuffer DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.33
       FRAME DEFAULT-FRAME:WIDTH            = 173.2.

ASSIGN 
       OvBuffer.ButikkNrFra:VISIBLE IN BROWSE BrwOvBuffer = FALSE.

ASSIGN 
       OvBunt.OppdatertAv:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.Faktura_Id:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.EDato:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.ETid:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.BrukerID:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.FakturertDato:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.FakturertTid:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.DatoOppdatert:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.TidOppdatert:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.RegistrertDato:VISIBLE IN BROWSE BrwOvBunt = FALSE
       OvBunt.RegistrertTid:VISIBLE IN BROWSE BrwOvBunt = FALSE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       tbOvBuffer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,excel;Eksporter til E&xcel,RegistrerLinjer;Registrer/endre linjer¤enable,Etikett;Etikettmaxborder".

ASSIGN 
       tbOvBunt:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,delete;Slett,filter;Filter,refresh;Refresh,excel;Eksporter til E&xcelmaxborder".

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
"OvBuffer.BuntNr" "BuntNr" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? yes "Buntnummer." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"OvBuffer.LinjeNr" "LinjeNr" ">>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Linjenummer" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"OvBuffer.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"OvBuffer.ovbuffer_Beskr" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"OvBuffer.ovbuffer_LevKod" "Lev.art.nr" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"OvBuffer.ovbuffer_LevFargKod" "Farge" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"OvBuffer.Storl" "Størrelse" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse som skal overføres" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"OvBuffer.ovbuffer_Kode" "Strekkode" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"OvBuffer.Antall" "Antall" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Antall par som skal overøres" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"OvBuffer.ButikkNrTil" "Til butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk det overføres til" no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"OvBuffer.ovbuffer_TilButNavn" "Butikk navn" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"OvBuffer.ovbuffer_DatoTidEndret" "Endret" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"OvBuffer.BrukerID" "Endret av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"OvBuffer.ovbuffer_DatoTidRegistrert" "Opprettet" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"OvBuffer.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"OvBuffer.ButikkNrFra" "Fra butikknummer" ">>>>>9" "integer" ? ? ? ? ? ? no "Butikk det overføres fra" no no "16.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOvBuffer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOvBunt
/* Query rebuild information for BROWSE BrwOvBunt
     _TblList          = "SkoTex.OvBunt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > OvBunt.BuntNr
"OvBunt.BuntNr" "BuntNr" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? yes "Buntnummer." no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > OvBunt.FraButNr
"OvBunt.FraButNr" "Fra butikk" ">>>>>9" "integer" ? ? ? ? ? ? no "Butikk det overføres varer fra." no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > OvBunt.Merknad
"OvBunt.Merknad" "Merknad" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > OvBunt.opphav
"OvBunt.opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "" no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"OvBunt.ovbunt_OpphavTekst" "Fra modul" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"OvBunt.ovbunt_Opprettet" "Registrert" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > OvBunt.RegistrertAv
"OvBunt.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > OvBunt.BatchNr
"OvBunt.BatchNr" "Batch" "zzzzzzzz9" "INTEGER" ? ? ? ? ? ? no "Batch nummer som holder sammen transaksjoner" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"OvBunt.ovbunt_DatoTidOppdatert" "Oppdatert" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > OvBunt.OppdatertAv
"OvBunt.OppdatertAv" "Oppdatert av" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som oppdaterte bunten" no no "15" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"OvBunt.ovbunt_FakturaNr" "Fakturanr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "" no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > OvBunt.Faktura_Id
"OvBunt.Faktura_Id" "FId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"OvBunt.ovbunt_DatotidFakturert" "Fakturert" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > OvBunt.PlListeId
"OvBunt.PlListeId" "Pl.lst.id" ">>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Plukkliste id." no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > OvBunt.BuntStatus
"OvBunt.BuntStatus" "Status" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > OvBunt.EDato
"OvBunt.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > OvBunt.ETid
"OvBunt.ETid" "ETid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Endret tidspunkt" no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > OvBunt.BrukerID
"OvBunt.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > OvBunt.FakturertDato
"OvBunt.FakturertDato" "Fakturert" "99/99/99" "DATE" ? ? ? ? ? ? no "Fakturert dato" no no "9.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > OvBunt.FakturertTid
"OvBunt.FakturertTid" "Kl" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Fakturert tidspunkt" no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > OvBunt.DatoOppdatert
"OvBunt.DatoOppdatert" "Oppdatert" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato da bunten ble oppdatert mot overføringsordre" no no "9.4" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > OvBunt.TidOppdatert
"OvBunt.TidOppdatert" "Tidspunkt oppdatert" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "" no no "19" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > OvBunt.RegistrertDato
"OvBunt.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > OvBunt.RegistrertTid
"OvBunt.RegistrertTid" "Registreringstidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt for registrering av posten" no no "20.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"OvBunt.ovbunt_Dummy" "." "X(1)" "CHARACTER" ? ? ? ? ? ? no "" no no "1" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > OvBunt.TilbutNr
"OvBunt.TilbutNr" "Til butikk" ">>>>>9" "integer" ? ? ? ? ? ? no "Butikk det overføres varer til" no no "8.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOvBunt */
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


&Scoped-define SELF-NAME btnSplitBarY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSplitBarY C-Win
ON END-MOVE OF btnSplitBarY IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DYNAMIC-FUNCTION("setSplitBarY",THIS-PROCEDURE:CURRENT-WINDOW,btnSplitBarY:HANDLE IN FRAME {&FRAME-NAME},NO).
/*  apply "window-resized" to {&window-name}.*/
/*   RUN MoveToTop. */
/*   DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,OrderFolder:HANDLE). */
/*   RUN MoveToTop IN hCurrTabProc NO-ERROR.                            */
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
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
{incl/wintrigg.i}
{incl/conttrigg.i oBrwOvBunt:BROWSE-HANDLE}
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
  /* Slette overføringsordre. */
  IF otbOvBunt:isCurrent THEN
  DO:
    IF AVAILABLE OvBunt AND
      JBoxServerAPI:Instance:Find("OvBunt", "WHERE BuntNr = " + STRING(OvBunt.BuntNr)) THEN
    DO:
      IF OvBunt.DatoOppdatert <> ? THEN
      DO:
        JBoxSession:Instance:ViewMessage("Overføringen er oppdatert. Kan ikke slettes.").
        RETURN.
      END.
      IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
      DO:
        JBoxSession:Instance:ViewMessage("Det er markert flere rader. Bare en overføring kan slettes ad gangen.").
        RETURN.
      END.
/*      IF NOT oBrwOvBunt:processRowsNoMessage("ovbunt_slett.p", "") THEN                         */
/*        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).*/
    END. 
  END.
  /* Slette rad. */
  ELSE IF otbOvBuffer:isCurrent THEN
    DO:
      IF AVAILABLE OvBunt AND
        JBoxServerAPI:Instance:Find("OvBunt", "WHERE BuntNr = " + STRING(OvBunt.BuntNr)) THEN
      DO:
        IF OvBunt.DatoOppdatert <> ? THEN
        DO:
          JBoxSession:Instance:ViewMessage("Overføringen er oppdatert. Rader kan ikke slettes på en oppdatert overføring.").
          RETURN.
      END. 
      ELSE DO:

      END.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
IF oBrwOvBunt:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  IF oBrwOvBunt:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
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
  ENABLE RegistrerLinjer_tbOvBuffer Etikett_tbOvBuffer btnSplitBarY 
         first_tbOvBunt prev_tbOvBunt next_tbOvBunt last_tbOvBunt new_tbOvBunt 
         delete_tbOvBunt filter_tbOvBunt refresh_tbOvBunt excel_tbOvBunt 
         BrwOvBunt first_tbOvBuffer prev_tbOvBuffer next_tbOvBuffer 
         last_tbOvBuffer refresh_tbOvBuffer excel_tbOvBuffer BrwOvBuffer 
         tbOvBunt searchOvBunt tbOvBuffer 
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
        RUN enkeltEtikett.w (INPUT ovBuffer_Kode, 
                             INPUT OvBuffer.Antall, 
                             INPUT ovBuffer.ButikkNrFra,
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
  oBrwOvBunt = NEW JBoxBrowse(brwOvBunt:HANDLE).

  otbOvBunt = NEW JBoxToolbar(tbOvBunt:HANDLE).
  otbOvBuffer = NEW JBoxToolbar(tbOvBuffer:HANDLE).

  oBrwOvBunt:TOOLBAR-OBJECT = otbOvBunt.
  oBrwOvBunt:setSearchField(searchOvBunt:HANDLE,"BuntNr").
  oBrwOvBunt:setQuerySort("BuntNr;DESC").
  oBrwOvBunt:postUpdateProc = "ovbunt_post_update.p".  
  oBrwOvBunt:baseQuery = "WHERE Opphav = '5'".
  oBrwOvBunt:customDeleteValProc = "ignore". /* Fjerner vlaidering på om det ligger Ovbuffer poster under Ovbunt ved delete. */
  otbOvBunt:addToolGroup("Oppdater;Oppdater,SkrivFaktura;Skriv faktura").

  oBrwOvBuffer = NEW JBoxBrowse(brwOvBuffer:HANDLE).
  oBrwOvBuffer:setParentBrowseObject(oBrwOvBunt,"BuntNr").
  oBrwOvBuffer:TOOLBAR-OBJECT = otbOvBuffer.
  oBrwOvBuffer:postUpdateProc = "ovbuffer_post_update.p".
  oBrwOvBuffer:setQuerySort("LinjeNr;DESC").
  
  oContainer:setSplitBarY(btnSplitBarY:HANDLE).
  oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,150). /* 200 pixels from the top, 300 pixels from the bottom */
  oContainer:setFollowSplitBarY(STRING(BrwOvBunt:HANDLE) + ',' + 
                                STRING(BrwOvBuffer:HANDLE) + ',' +
                                STRING(first_tbOvBuffer:HANDLE) + ',' +
                                STRING(prev_tbOvBuffer:HANDLE) + ',' +
                                STRING(next_tbOvBuffer:HANDLE) + ',' +
                                STRING(last_tbOvBuffer:HANDLE) + ',' +
                                STRING(refresh_tbOvBuffer:HANDLE) + ',' +
                                STRING(excel_tbOvBuffer:HANDLE) + ',' +
                                STRING(RegistrerLinjer_tbOvBuffer:HANDLE) + ',' +
                                STRING(tbOvBuffer:HANDLE)
                                ).
  oContainer:setNoResizeY("BrwOvBunt"). 
  
  hAntallColumn    = oBrwOvBuffer:getColumnHandle("Antall").
  oBrwFillInAntall = NEW JBoxBrowseFillIn(oBrwOvBuffer,"Antall",TRUE).
  oBrwOvBuffer:enabledColumns = "Antall".

  DO:
    opopupOvBuffer = NEW JBoxPopupMenu().
    opopupOvbuffer:AddToolGroup('OppslagModell;Vis i modell liste').
    oBrwOvBuffer:POPUP-MENU-OBJECT = opopupOvBuffer.
  END.
   
END.
oBrwOvBunt:OpenQuery().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
DEFINE INPUT  PARAMETER ihFillIn AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer AS HANDLE NO-UNDO.
DEFINE OUTPUT PARAMETER obOk     AS LOG    NO-UNDO.

  /* Overføringsordre. */
  IF otbOvBunt:isCurrent THEN 
  DO WITH FRAME DEFAULT-FRAME:
  END.    
    
  /* Rad. */
  ELSE IF otbOvBuffer:isCurrent THEN 
  DO:

  END.
  IF NOT JBoxServerApi:Instance:Update("OvBuffer",
                                OvBuffer.RowIdent1,
                                "Antall",
                                ihFillIn:SCREEN-VALUE,
                                FALSE,
                                "ovbuffer_post_update.p",
                                TRUE) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved endring rad pga " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.  
    /* Gør refresh på moder raden - Kordrehode. */
    DYNAMIC-FUNCTION("RefreshRowids",oBrwOvBunt:BROWSE-HANDLE,STRING(oBrwOvBuffer:PARENT-BUFFER-HANDLE:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)).
    oBrwOvBuffer:refresh().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
DO WITH FRAME {&FRAME-NAME}:

  /* Ny overføringsordre. */
  IF otbOvBunt:isCurrent THEN 
  DO:
  END.
  /* Ny rad. */
  ELSE IF otbOvBuffer:isCurrent THEN 
  DO:
    IF AVAILABLE OvBunt AND OvBunt.DatoOppdatert <> ? THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Overføringsordren er allerede oppdatert. Nye linjer kan ikke registreres.").
      RETURN.
    END.
  END.

  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:

    /* Ny overføringsordre. */
    IF otbOvBunt:isCurrent THEN 
    DO:
      IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "Brukerid,FraButNr,TilButNr,Merknad,BuntStatus,Opphav", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|" + DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr") + "|0|OVERFØRING" + '|10|5' ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil opprettelse av pakkseddel pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
      ELSE oBrwOvbunt:refreshRow().
    END.
    /* Ny rad. */
    ELSE IF otbOvBuffer:isCurrent THEN 
    DO:
    END.
    
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdaterRecord C-Win 
PROCEDURE oppdaterRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  IF AVAILABLE OvBunt THEN DO WITH FRAME {&FRAME-NAME}:
    
    IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
    DO:    
      IF OvBunt.DatoOppdatert <> ? THEN
        DO:
          JBoxSession:Instance:ViewMessage("Overføringsordren er allerede oppdatert.").
          RETURN.  
        END.
      IF NOT JBoxServerAPI:Instance:Find("OvBuffer", "WHERE BuntNr = " + STRING(OvBunt.BuntNr) + " AND LinjeNr > 0") THEN
        DO:
          JBoxSession:Instance:ViewMessage("Ingen varelinjer på overføringsordren. Den kan ikke oppdateres.").
          RETURN.  
        END.
  
      IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Oppdater overføringsordre?") THEN 
          RETURN.
      IF NOT oBrwOvBunt:processRowsNoMessage("ovbunt_oppdater.p", "") THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
      oBrwOvBunt:refreshRow().  
    END.
    ELSE DO: 
      JBoxSession:Instance:ViewMessage("Marker en ordre!").
    END.
  END.
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
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',OvBuffer.ovbuffer_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',Ovbuffer.ovbuffer_LevFargKod)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegistrerLinjerRecord C-Win 
PROCEDURE RegistrerLinjerRecord :
DEFINE VARIABLE pcFraButNr AS CHARACTER NO-UNDO.
  
  IF JBoxServerAPI:Instance:Find("Syspara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 3") THEN
      ASSIGN
        pcFraButNr = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1") NO-ERROR.  

DO WITH FRAME {&FRAME-NAME}:
  
  IF AVAILABLE OvBunt THEN
  DO:
    IF OvBunt.FraButNr <> INT(pcFraButNr) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Overføringen er ikke registrert fra lager, og kan derfor ikke endres her.").
        RETURN NO-APPLY.
      END.
    IF Ovbunt.DatoOppdatert <> ? THEN
      DO:
        JBoxSession:Instance:ViewMessage("Overføringen er oppdatert. Nye linjer kan ikke registreres.").
        RETURN NO-APPLY.
      END.
/*    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.*/
    RUN registrerInternoverforing.w ( OvBunt.BuntNr, Ovbunt.Merknad, OvBunt.opphav ).
/*    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.*/
    oBrwOvBuffer:openQuery().
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivFakturaRecord C-Win 
PROCEDURE skrivFakturaRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
IF oBrwovbunt:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv ut faktura for valgt(e) poster(er)?") THEN 
        RETURN.
    IF NOT oBrwOvBunt:processRowsNoMessage("ovbunt_skrivfaktura.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv ut faktura for valgt pakkseddel?") THEN 
        RETURN.
    oBrwOvBunt:processSet("ovbunt_skrivfaktura.p","").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

