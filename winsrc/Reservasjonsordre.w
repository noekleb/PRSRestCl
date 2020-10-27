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
DEFINE INPUT PARAMETER icParam AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE bFraeCom AS LOG NO-UNDO.
DEFINE VARIABLE cTilButNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE hOvBunt_ovbunt_BuntStatusTekst AS HANDLE NO-UNDO.  
DEFINE VARIABLE hOvBuffer_Antall AS HANDLE NO-UNDO.  
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cBehStatusRowIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBehStatusIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE iKasseNr AS INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE iFraButNrSerie AS INTEGER FORMAT ">>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE iTilButNrSerie AS INTEGER FORMAT ">>>>>>>>>9" NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwOvBunt ***/
DEF VAR oBrwOvBunt AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE OvBunt
    FIELD BuntNr AS INTEGER
    FIELD FraButNr AS INTEGER
    FIELD TilbutNr AS INTEGER
    FIELD ovbunt_BuntStatusTekst AS CHARACTER
    FIELD ovbunt_OvNotatFinnes AS CHARACTER
    FIELD ovbunt_FakturaNr AS CHARACTER
    FIELD Merknad AS CHARACTER
    FIELD ovbunt_Opprettet AS CHARACTER
    FIELD ovbunt_OpphavTekst AS CHARACTER
    FIELD ovbunt_DatoTidOppdatert AS CHARACTER
    FIELD ovbunt_DatotidFakturert AS CHARACTER
    FIELD BuntStatus AS INTEGER
    FIELD BatchNr AS INTEGER
    FIELD Faktura_Id AS DECIMAL
    FIELD opphav AS INTEGER
    FIELD ovbunt_Dummy AS CHARACTER
    FIELD BrukerID AS CHARACTER
    FIELD EDato AS DATE
    FIELD RegistrertAv AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD OppdatertAv AS CHARACTER
    FIELD DatoOppdatert AS DATE
    FIELD OvNotat AS CHARACTER
    FIELD ovbunt_BuntStatusSkip AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_OvBunt FOR TEMP-TABLE OvBunt.


FUNCTION getBuffersAndFieldsBrwOvBunt RETURNS CHARACTER():
  RETURN
    'OvBunt'
     + ';BuntNr'
     + ';FraButNr'
     + ';TilbutNr'
     + ';Merknad'
     + ';BuntStatus'
     + ';BatchNr'
     + ';Faktura_Id'
     + ';opphav'
     + ';BrukerID'
     + ';EDato'
     + ';RegistrertAv'
     + ';RegistrertDato'
     + ';OppdatertAv'
     + ';DatoOppdatert'
     + ';OvNotat'
     + ';+ovbunt_BuntStatusTekst|CHARACTER||ovbunt_BuntStatusTekst|Status'
     + ';+ovbunt_OvNotatFinnes|CHARACTER||ovbunt_OvNotatFinnes|N'
     + ';+ovbunt_FakturaNr|CHARACTER||ovbunt_FakturaNr|Fakturanr'
     + ';+ovbunt_Opprettet|CHARACTER||ovbunt_Opprettet|Opprettet'
     + ';+ovbunt_OpphavTekst|CHARACTER||ovbunt_OpphavTekst|Opphavs beskrivelse'
     + ';+ovbunt_DatoTidOppdatert|CHARACTER||ovbunt_DatoTidOppdatert|Oppdatert'
     + ';+ovbunt_DatotidFakturert|CHARACTER||ovbunt_DatotidFakturert|Fakturert'
     + ';+ovbunt_Dummy|CHARACTER||ovbunt_Dummy|.'
     + ';+ovbunt_BuntStatusSkip|CHARACTER||ovbunt_BuntStatusSkip(ROWID)|ovbunt_BuntStatusSkip'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOvBunt RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwOvBunt RETURNS CHARACTER():
  RETURN 
     'server/ovbunt_brwcalc.p' /* ovbunt_BuntStatusTekst */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_OvNotatFinnes */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_FakturaNr */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_Opprettet */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_OpphavTekst */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_DatoTidOppdatert */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_DatotidFakturert */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_Dummy */
   + ',server/ovbunt_brwcalc.p' /* ovbunt_BuntStatusSkip(ROWID) */
     .
END FUNCTION.
DEF VAR otbOvBunt AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwOvBuffer ***/
DEF VAR oBrwOvBuffer AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE OvBuffer
    FIELD LinjeNr AS DECIMAL
    FIELD ovbuffer_Beskr AS CHARACTER
    FIELD ovbuffer_LevKod AS CHARACTER
    FIELD ovbuffer_LevFargKod AS CHARACTER
    FIELD Storl AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD VareKost AS DECIMAL
    FIELD ButikkNrFra AS INTEGER
    FIELD ButikkNrTil AS INTEGER
    FIELD ovbuffer_Kode AS CHARACTER
    FIELD Merknad AS CHARACTER
    FIELD ovbuffer_DatoTidRegistrert AS CHARACTER
    FIELD BrukerID AS CHARACTER
    FIELD BuntNr AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_OvBuffer FOR TEMP-TABLE OvBuffer.


FUNCTION getBuffersAndFieldsBrwOvBuffer RETURNS CHARACTER():
  RETURN
    'OvBuffer'
     + ';LinjeNr'
     + ';Storl'
     + ';Antall'
     + ';VareKost'
     + ';ButikkNrFra'
     + ';ButikkNrTil'
     + ';Merknad'
     + ';BrukerID'
     + ';BuntNr'
     + ';ArtikkelNr'
     + ';+ovbuffer_Beskr|CHARACTER||ovbuffer_Beskr|Varetekst'
     + ';+ovbuffer_LevKod|CHARACTER||ovbuffer_LevKod|Lev.art.nr'
     + ';+ovbuffer_LevFargKod|CHARACTER||ovbuffer_LevFargKod|Fargekode'
     + ';+ovbuffer_Kode|CHARACTER||ovbuffer_Kode|Kode'
     + ';+ovbuffer_DatoTidRegistrert|CHARACTER||ovbuffer_DatoTidRegistrert|Registrert'
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
   + ',server/ovbuffer_brwcalc.p' /* ovbuffer_DatoTidRegistrert */
     .
END FUNCTION.


DEF VAR otbOvBuffer AS JBoxToolbar NO-UNDO.


/*** Start instance property definitions for JBoxBrowse object oBrwOvNotat ***/
DEF VAR oBrwOvNotat AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE OvNotat
    FIELD LinjeNr AS INTEGER
    FIELD Notat AS CHARACTER
    FIELD DatoTid AS DATETIME
    FIELD BrukerId AS CHARACTER
    FIELD BuntNr AS INTEGER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEF BUFFER v_OvNotat FOR TEMP-TABLE OvNotat.


FUNCTION getBuffersAndFieldsBrwOvNotat RETURNS CHARACTER():
  RETURN
    'OvNotat'
     + ';LinjeNr'
     + ';Notat'
     + ';DatoTid'
     + ';BrukerId'
     + ';BuntNr'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwOvNotat RETURNS CHARACTER():
  RETURN ''.
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
&Scoped-define INTERNAL-TABLES OvBuffer OvBunt OvNotat

/* Definitions for BROWSE BrwOvBuffer                                   */
&Scoped-define FIELDS-IN-QUERY-BrwOvBuffer OvBuffer.LinjeNr ~
OvBuffer.ovbuffer_Beskr OvBuffer.ovbuffer_LevKod ~
OvBuffer.ovbuffer_LevFargKod OvBuffer.Storl OvBuffer.Antall ~
OvBuffer.VareKost OvBuffer.ButikkNrFra OvBuffer.ButikkNrTil ~
OvBuffer.ovbuffer_Kode OvBuffer.Merknad OvBuffer.ovbuffer_DatoTidRegistrert ~
OvBuffer.BrukerID OvBuffer.BuntNr OvBuffer.ArtikkelNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOvBuffer OvBuffer.LinjeNr ~
OvBuffer.BuntNr 
&Scoped-define QUERY-STRING-BrwOvBuffer FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOvBuffer OPEN QUERY BrwOvBuffer FOR EACH OvBuffer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOvBuffer OvBuffer
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOvBuffer OvBuffer


/* Definitions for BROWSE BrwOvBunt                                     */
&Scoped-define FIELDS-IN-QUERY-BrwOvBunt OvBunt.BuntNr OvBunt.FraButNr ~
OvBunt.TilbutNr OvBunt.ovbunt_BuntStatusTekst OvBunt.ovbunt_OvNotatFinnes ~
OvBunt.ovbunt_FakturaNr OvBunt.Merknad OvBunt.ovbunt_Opprettet ~
OvBunt.ovbunt_OpphavTekst OvBunt.ovbunt_DatoTidOppdatert ~
OvBunt.ovbunt_DatotidFakturert OvBunt.BuntStatus OvBunt.BatchNr ~
OvBunt.Faktura_Id OvBunt.opphav OvBunt.ovbunt_Dummy OvBunt.BrukerID ~
OvBunt.EDato OvBunt.RegistrertAv OvBunt.RegistrertDato OvBunt.OppdatertAv ~
OvBunt.DatoOppdatert OvBunt.OvNotat OvBunt.ovbunt_BuntStatusSkip 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOvBunt OvBunt.BuntNr ~
OvBunt.FraButNr OvBunt.ovbunt_Opprettet OvBunt.ovbunt_OpphavTekst 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwOvBunt OvBunt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwOvBunt OvBunt
&Scoped-define QUERY-STRING-BrwOvBunt FOR EACH OvBunt NO-LOCK, INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOvBunt OPEN QUERY BrwOvBunt FOR EACH OvBunt NO-LOCK, INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOvBunt OvBunt
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOvBunt OvBunt


/* Definitions for BROWSE BrwOvNotat                                    */
&Scoped-define FIELDS-IN-QUERY-BrwOvNotat OvNotat.LinjeNr OvNotat.Notat ~
OvNotat.DatoTid OvNotat.BrukerId OvNotat.BuntNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwOvNotat OvNotat.LinjeNr 
&Scoped-define QUERY-STRING-BrwOvNotat FOR EACH OvNotat NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwOvNotat OPEN QUERY BrwOvNotat FOR EACH OvNotat NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwOvNotat OvNotat
&Scoped-define FIRST-TABLE-IN-QUERY-BrwOvNotat OvNotat


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbOvBunt btnSplitBarY searchOvBunt ~
tbOvBuffer first_tbOvBunt prev_tbOvBunt next_tbOvBunt last_tbOvBunt ~
new_tbOvBunt delete_tbOvBunt note_tbOvBunt refresh_tbOvBunt filter_tbOvBunt ~
excel_tbOvBunt print_tbOvBunt btnUtvalgBehStatus fiMerknad fiBehStatusLst ~
BrwOvBunt BrwOvNotat delete_tbOvBuffer BrwOvBuffer 
&Scoped-Define DISPLAYED-OBJECTS fiMerknad fiBehStatusLst 

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
     SIZE 167 BY .43.

DEFINE BUTTON btnUtvalgBehStatus 
     LABEL "Behandlingsstatus..." 
     SIZE 26.8 BY 1.1 TOOLTIP "Valg av behandlingsstatus".

DEFINE BUTTON delete_tbOvBuffer 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON delete_tbOvBunt 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbOvBunt 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbOvBunt 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbOvBunt 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbOvBunt 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON new_tbOvBunt 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbOvBunt 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON note_tbOvBunt 
     IMAGE-UP FILE "bmp/note16e.bmp":U
     LABEL "Note" 
     SIZE 4.6 BY 1.1 TOOLTIP "Note (ALT-N)".

DEFINE BUTTON prev_tbOvBunt 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON print_tbOvBunt 
     IMAGE-UP FILE "bmp/print16e.bmp":U
     LABEL "Print" 
     SIZE 4.6 BY 1.1 TOOLTIP "Print (ALT-P)".

DEFINE BUTTON refresh_tbOvBunt 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE fiBehStatusLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beh.status" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1 TOOLTIP "Liste med behandlingsstatus det skal vises reservasjoner for" NO-UNDO.

DEFINE VARIABLE fiMerknad AS CHARACTER FORMAT "X(256)":U 
     LABEL "Merknad" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Filter på merknadsfeltet." NO-UNDO.

DEFINE RECTANGLE searchOvBunt
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38.6 BY .91.

DEFINE RECTANGLE tbOvBuffer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 166.8 BY 1.29.

DEFINE RECTANGLE tbOvBunt
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 168.4 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwOvBuffer FOR 
      OvBuffer SCROLLING.

DEFINE QUERY BrwOvBunt FOR 
      OvBunt SCROLLING.

DEFINE QUERY BrwOvNotat FOR 
      OvNotat SCROLLING.
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
      OvBuffer.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9.99":U
      OvBuffer.VareKost COLUMN-LABEL "Varekost" FORMAT "->,>>>,>>9.99":U
      OvBuffer.ButikkNrFra COLUMN-LABEL "Fra but" FORMAT ">>>>>9":U
      OvBuffer.ButikkNrTil COLUMN-LABEL "Til buti" FORMAT ">>>>>9":U
      OvBuffer.ovbuffer_Kode COLUMN-LABEL "Kode" FORMAT "X(18)":U
      OvBuffer.Merknad COLUMN-LABEL "Merknad" FORMAT "X(40)":U
      OvBuffer.ovbuffer_DatoTidRegistrert COLUMN-LABEL "Registrert" FORMAT "X(20)":U
      OvBuffer.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      OvBuffer.BuntNr COLUMN-LABEL "BuntNr" FORMAT ">>>>>>>>>9":U
      OvBuffer.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 23.8
  ENABLE
      OvBuffer.LinjeNr HELP "Linjenummer"
      OvBuffer.BuntNr HELP "Buntnummer."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 166.6 BY 8.81 FIT-LAST-COLUMN.

DEFINE BROWSE BrwOvBunt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOvBunt C-Win _STRUCTURED
  QUERY BrwOvBunt NO-LOCK DISPLAY
      OvBunt.BuntNr COLUMN-LABEL "Res.nr" FORMAT ">>>>>>>>>9":U
      OvBunt.FraButNr COLUMN-LABEL "Fra" FORMAT ">>>>>9":U
      OvBunt.TilbutNr COLUMN-LABEL "Til" FORMAT ">>>>>9":U
      OvBunt.ovbunt_BuntStatusTekst COLUMN-LABEL "Status" FORMAT "X(40)":U
            WIDTH 17
      OvBunt.ovbunt_OvNotatFinnes COLUMN-LABEL "N" FORMAT "X(2)":U
      OvBunt.ovbunt_FakturaNr COLUMN-LABEL "Fakturanr" FORMAT "X(12)":U
      OvBunt.Merknad COLUMN-LABEL "Merknad" FORMAT "X(100)":U WIDTH 80
      OvBunt.ovbunt_Opprettet COLUMN-LABEL "Opprettet" FORMAT "X(20)":U
      OvBunt.ovbunt_OpphavTekst COLUMN-LABEL "Opphavs beskrivelse" FORMAT "X(25)":U
      OvBunt.ovbunt_DatoTidOppdatert COLUMN-LABEL "Oppdatert" FORMAT "X(20)":U
      OvBunt.ovbunt_DatotidFakturert COLUMN-LABEL "Fakturert" FORMAT "X(20)":U
      OvBunt.BuntStatus COLUMN-LABEL "Status" FORMAT ">9":U
      OvBunt.BatchNr COLUMN-LABEL "Batch" FORMAT "zzzzzzzz9":U
            WIDTH 9.2
      OvBunt.Faktura_Id COLUMN-LABEL "FId" FORMAT ">>>>>>>>>>>>9":U
      OvBunt.opphav COLUMN-LABEL "Opphav" FORMAT ">9":U WIDTH 4
      OvBunt.ovbunt_Dummy COLUMN-LABEL "." FORMAT "X(1)":U
      OvBunt.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      OvBunt.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      OvBunt.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      OvBunt.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      OvBunt.OppdatertAv COLUMN-LABEL "Oppdatert av" FORMAT "X(15)":U
      OvBunt.DatoOppdatert COLUMN-LABEL "Oppdatert" FORMAT "99/99/99":U
      OvBunt.OvNotat COLUMN-LABEL "Notat" FORMAT "x(40)":U
      OvBunt.ovbunt_BuntStatusSkip COLUMN-LABEL "ovbunt_BuntStatusSkip" FORMAT "X(8)":U
  ENABLE
      OvBunt.BuntNr HELP "Reservasjonsnr."
      OvBunt.FraButNr HELP "Butikk det overføres varer fra."
      OvBunt.ovbunt_Opprettet
      OvBunt.ovbunt_OpphavTekst
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 119 BY 10.24 FIT-LAST-COLUMN.

DEFINE BROWSE BrwOvNotat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwOvNotat C-Win _STRUCTURED
  QUERY BrwOvNotat NO-LOCK DISPLAY
      OvNotat.LinjeNr COLUMN-LABEL "Linje" FORMAT ">>>9":U
      OvNotat.Notat COLUMN-LABEL "Loggtekst" FORMAT "x(60)":U
      OvNotat.DatoTid COLUMN-LABEL "Dato/Tid" FORMAT "99/99/9999 HH:MM:SS":U
            WIDTH 23
      OvNotat.BrukerId COLUMN-LABEL "BrukerId" FORMAT "x(15)":U
      OvNotat.BuntNr COLUMN-LABEL "BuntNr" FORMAT ">>>>>>>>>9":U
  ENABLE
      OvNotat.LinjeNr HELP "Linjenr"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 47 BY 10.19 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 14.14 COL 2 WIDGET-ID 28
     first_tbOvBunt AT ROW 1.29 COL 1.8 WIDGET-ID 4
     prev_tbOvBunt AT ROW 1.29 COL 6.6 WIDGET-ID 6
     next_tbOvBunt AT ROW 1.29 COL 11.2 WIDGET-ID 8
     last_tbOvBunt AT ROW 1.29 COL 15.8 WIDGET-ID 10
     new_tbOvBunt AT ROW 1.29 COL 20.4 WIDGET-ID 12
     delete_tbOvBunt AT ROW 1.29 COL 25 WIDGET-ID 26
     note_tbOvBunt AT ROW 1.29 COL 29.6 WIDGET-ID 42
     refresh_tbOvBunt AT ROW 1.29 COL 34.2 WIDGET-ID 16
     filter_tbOvBunt AT ROW 1.29 COL 38.8 WIDGET-ID 18
     excel_tbOvBunt AT ROW 1.29 COL 43.4 WIDGET-ID 20
     print_tbOvBunt AT ROW 1.29 COL 48 WIDGET-ID 84
     btnUtvalgBehStatus AT ROW 2.57 COL 142.2 WIDGET-ID 82
     fiMerknad AT ROW 2.62 COL 65.8 COLON-ALIGNED
     fiBehStatusLst AT ROW 2.62 COL 122.6 COLON-ALIGNED
     BrwOvBunt AT ROW 3.81 COL 2 WIDGET-ID 200
     BrwOvNotat AT ROW 3.86 COL 122 WIDGET-ID 400
     delete_tbOvBuffer AT ROW 14.86 COL 2.4 WIDGET-ID 36
     BrwOvBuffer AT ROW 16.24 COL 2.4 WIDGET-ID 300
     tbOvBunt AT ROW 1.19 COL 1.6 WIDGET-ID 2
     searchOvBunt AT ROW 2.76 COL 2.4 WIDGET-ID 22
     tbOvBuffer AT ROW 14.76 COL 1.8 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 169.2 BY 24.19 WIDGET-ID 100.


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
         HEIGHT             = 24.19
         WIDTH              = 169.2
         MAX-HEIGHT         = 24.19
         MAX-WIDTH          = 169.2
         VIRTUAL-HEIGHT     = 24.19
         VIRTUAL-WIDTH      = 169.2
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
/* BROWSE-TAB BrwOvBunt fiBehStatusLst DEFAULT-FRAME */
/* BROWSE-TAB BrwOvNotat BrwOvBunt DEFAULT-FRAME */
/* BROWSE-TAB BrwOvBuffer delete_tbOvBuffer DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.19
       FRAME DEFAULT-FRAME:WIDTH            = 169.2.

ASSIGN 
       BrwOvBunt:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 4.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       tbOvBuffer:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "delete;Slettmaxborder".

ASSIGN 
       tbOvBunt:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,delete;Slett,note;Note,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,print;Printmaxborder".

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
"OvBuffer.Antall" "Antall" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Antall par som skal overøres" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"OvBuffer.VareKost" "Varekost" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Varekost" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"OvBuffer.ButikkNrFra" "Fra but" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk det overføres fra" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"OvBuffer.ButikkNrTil" "Til buti" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk det overføres til" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"OvBuffer.ovbuffer_Kode" "Kode" "X(18)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"OvBuffer.Merknad" "Merknad" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Kort merknad" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"OvBuffer.ovbuffer_DatoTidRegistrert" "Registrert" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"OvBuffer.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"OvBuffer.BuntNr" "BuntNr" ">>>>>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Buntnummer." no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"OvBuffer.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "" no no "23.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOvBuffer */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOvBunt
/* Query rebuild information for BROWSE BrwOvBunt
     _TblList          = "SkoTex.OvBunt"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > OvBunt.BuntNr
"OvBunt.BuntNr" "Res.nr" ">>>>>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Reservasjonsnr." no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > OvBunt.FraButNr
"OvBunt.FraButNr" "Fra" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikk det overføres varer fra." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > OvBunt.TilbutNr
"OvBunt.TilbutNr" "Til" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk det overføres varer til" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"OvBunt.ovbunt_BuntStatusTekst" "Status" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"OvBunt.ovbunt_OvNotatFinnes" "N" "X(2)" "CHARACTER" ? ? ? ? ? ? no "" no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"OvBunt.ovbunt_FakturaNr" "Fakturanr" "X(12)" "CHARACTER" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > OvBunt.Merknad
"OvBunt.Merknad" "Merknad" "X(100)" "CHARACTER" ? ? ? ? ? ? no "" no no "80" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"OvBunt.ovbunt_Opprettet" "Opprettet" "X(20)" "CHARACTER" ? ? ? ? ? ? yes "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"OvBunt.ovbunt_OpphavTekst" "Opphavs beskrivelse" "X(25)" "CHARACTER" ? ? ? ? ? ? yes "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"OvBunt.ovbunt_DatoTidOppdatert" "Oppdatert" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"OvBunt.ovbunt_DatotidFakturert" "Fakturert" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > OvBunt.BuntStatus
"OvBunt.BuntStatus" "Status" ">9" "INTEGER" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > OvBunt.BatchNr
"OvBunt.BatchNr" "Batch" "zzzzzzzz9" "INTEGER" ? ? ? ? ? ? no "Batch nummer som holder sammen transaksjoner" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > OvBunt.Faktura_Id
"OvBunt.Faktura_Id" "FId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > OvBunt.opphav
"OvBunt.opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"OvBunt.ovbunt_Dummy" "." "X(1)" "CHARACTER" ? ? ? ? ? ? no "" no no "1" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > OvBunt.BrukerID
"OvBunt.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > OvBunt.EDato
"OvBunt.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > OvBunt.RegistrertAv
"OvBunt.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > OvBunt.RegistrertDato
"OvBunt.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > OvBunt.OppdatertAv
"OvBunt.OppdatertAv" "Oppdatert av" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som oppdaterte bunten" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > OvBunt.DatoOppdatert
"OvBunt.DatoOppdatert" "Oppdatert" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato da bunten ble oppdatert mot overføringsordre" no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > OvBunt.OvNotat
"OvBunt.OvNotat" "Notat" "x(40)" "CHARACTER" ? ? ? ? ? ? no "For korte notater vedrørende behandling av ordren." no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"OvBunt.ovbunt_BuntStatusSkip" "ovbunt_BuntStatusSkip" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "22.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOvBunt */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwOvNotat
/* Query rebuild information for BROWSE BrwOvNotat
     _TblList          = "SkoTex.OvNotat"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"OvNotat.LinjeNr" "Linje" ">>>9" "INTEGER" ? ? ? ? ? ? yes "Linjenr" no no "4.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"OvNotat.Notat" "Loggtekst" "x(60)" "CHARACTER" ? ? ? ? ? ? no "Loggtekst" no no "60" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"OvNotat.DatoTid" "Dato/Tid" "99/99/9999 HH:MM:SS" "DATETIME" ? ? ? ? ? ? no "Dato og tid for registrering." no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"OvNotat.BrukerId" "BrukerId" "x(15)" "CHARACTER" ? ? ? ? ? ? no "Bruker." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"OvNotat.BuntNr" "BuntNr" ">>>>>>>>>9" "INTEGER" ? ? ? ? ? ? no "Buntnummer." no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwOvNotat */
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


&Scoped-define SELF-NAME btnUtvalgBehStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgBehStatus C-Win
ON CHOOSE OF btnUtvalgBehStatus IN FRAME DEFAULT-FRAME /* Behandlingsstatus... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  IF bFraECom THEN 
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "SysPara;ParaNr;Parameter1",
                        "where SysPara.sysHId = 11 and sysPara.sysGr = 10 and syspara.ParaNr > 10",
                        INPUT-OUTPUT cBehStatusRowIdList,
                        "ParaNr",
                        INPUT-OUTPUT cBehStatusIdList,
                        "","",
                        OUTPUT bOK).
  ELSE 
    RUN JBoxSelector.w (THIS-PROCEDURE,0,
                        "SysPara;ParaNr;ParaMeter1",
                        "where SysPara.sysHId = 11 and sysPara.sysGr = 10",
                        INPUT-OUTPUT cBehStatusRowIdList,
                        "ParaNr",
                        INPUT-OUTPUT cBehStatusIdList,
                        "","",
                        OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiBehStatusLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cBehStatusIdList,'|'))
    .
  IF bOk THEN   
    RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiMerknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMerknad C-Win
ON RETURN OF fiMerknad IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMerknad C-Win
ON TAB OF fiMerknad IN FRAME DEFAULT-FRAME /* Merknad */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME print_tbOvBunt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL print_tbOvBunt C-Win
ON CHOOSE OF print_tbOvBunt IN FRAME DEFAULT-FRAME /* Print */
DO:
  RUN PrintRecord.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AngreAvvisLinjerRecord C-Win 
PROCEDURE AngreAvvisLinjerRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF NOT AVAILABLE OvBunt OR NOT AVAILABLE OvBuffer THEN 
  DO:
    JBoxSession:Instance:ViewMessage("Marker linjen hvor avvisning skal angres.").
    RETURN.
  END.
  ELSE DO:
    IF NOT CAN-DO('20',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Varelinjer på reservasjoner med denne status kan ikke endres. ").
        RETURN.
      END.
    IF NUM-ENTRIES(OvBuffer.Merknad,'=') <> 2 THEN  
      DO:
        JBoxSession:Instance:ViewMessage("Varelinjer er ikke avvist tidligere! ").
        RETURN.
      END.
    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal avvisning av varelinje " + STRING(OvBuffer.LinjeNr) + " angres (Antall settes tilbake til opprinnelig verdi)?") THEN 
      RETURN.    
    IF NOT JBoxServerApi:Instance:Update("OvBuffer",
                                      OvBuffer.RowIdent1,
                                      "BrukerId,AngreAvvis,Antall,Merknad", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "||" + ENTRY(2,OvBuffer.Merknad,'=') + "|",
                                      FALSE,
                                      "ovBuffer_post_update.p",
                                      TRUE) THEN
      DO:                                      
        JBoxSession:Instance:ViewMessage("Feil ved avvisning av varelinje pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
      ELSE oBrwOvBuffer:refreshRow().
  END.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvvisLinjerRecord C-Win 
PROCEDURE AvvisLinjerRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE piAntall AS INTEGER NO-UNDO.
DEFINE VARIABLE piOrgAntall AS INTEGER NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF NOT AVAILABLE OvBunt OR NOT AVAILABLE OvBuffer THEN 
  DO:
    JBoxSession:Instance:ViewMessage("Marker linjen som skal avvises.").
    RETURN.
  END.
  ELSE DO:
    IF NOT CAN-DO('20',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Varelinjer på reservasjoner med denne status kan ikke endres. ").
        RETURN.
      END.
    IF ovBuffer.Antall = 0 THEN  
      DO:
        JBoxSession:Instance:ViewMessage("Varelinjer er allerede avvist! ").
        RETURN.
      END.
    
    IF NOT OvBuffer.Merknad BEGINS "OrgAntall" THEN 
      JBoxServerApi:Instance:Update("OvBuffer",
                                     OvBuffer.RowIdent1,
                                     "BrukerId,Merknad", /* NB: Brukerid skal stå først. Derretter Avvis. */
                                     JBoxSession:Instance:UserId + "|OrgAntall=" + STRING(OvBuffer.Antall),
                                     FALSE,
                                     "",
                                     TRUE).
    ASSIGN 
      piAntall    = OvBuffer.Antall
      piOrgAntall = IF NUM-ENTRIES(Ovbuffer.Merknad,'=') = 2 THEN 
                      INT(ENTRY(2,OvBuffer.Merknad,'='))
                    ELSE
                      Ovbuffer.Antall
      .
    RUN ReservasjonEndreAntall.w (ovBuffer.BuntNr, OvBuffer.LinjeNr, INPUT-OUTPUT piAntall, OUTPUT bOk).
    IF bOk <> TRUE THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Avvisning av varelinje avbrutt. ").
        RETURN.
      END. 
    IF piAntall = piOrgAntall THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Ingen endring gjort på antallet. Avvisning avbrutt. ").
        RETURN.
      END. 
    IF NOT JBoxServerApi:Instance:Update("OvBuffer",
                                      OvBuffer.RowIdent1,
                                      "BrukerId,Avvis,Antall", /* NB: Brukerid skal stå først. Derretter Avvis. */
                                      JBoxSession:Instance:UserId + "||" + STRING(piAntall),
                                      FALSE,
                                      "ovBuffer_post_update.p",
                                      TRUE) THEN
      DO:                                      
        JBoxSession:Instance:ViewMessage("Feil ved avvisning av varelinje pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
      ELSE oBrwOvBuffer:refreshRow().
  END.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvvisRecord C-Win 
PROCEDURE AvvisRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.
  IF AVAILABLE OvBunt THEN 
  DO:
    IF NOT CAN-DO('20',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare reservasjonsordre godkjent i butikk kan avvises. ").
        RETURN.
      END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Avvise reservasjonen?") THEN 
      RETURN.    
    
    IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "BrukerId,BuntStatus", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|40" ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved avvisning av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    ELSE oBrwOvbunt:refreshRow().
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Marker reservasjonsordre som skal avvises.").
  RUN RefreshRecord.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
  DO WITH FRAME {&FRAME-NAME}:
    IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
    DO:
      JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
      RETURN.
    END.
  
    IF otbOvBunt:isCurrent THEN 
    DO:
      IF AVAILABLE OvBunt AND NOT CAN-DO('10,50,60',STRING(OvBunt.buntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare nyopprettede reservasjoner kan slettes. Bruk AVVIS funksjonen. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    END.
    /* Ny rad. */
    ELSE IF otbOvBuffer:isCurrent THEN 
    DO:
      IF AVAILABLE OvBuffer AND AVAILABLE OvBunt AND OvBunt.BuntStatus = 10 THEN . /* Gjør ingenting. sletting kan gjøres. */
      ELSE DO:
        JBoxSession:Instance:ViewMessage("Bare på nyopprettede reservasjoner kan varelinjer slettes. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    END.
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF otbOvBunt:isCurrent THEN 
    DO:
    END.
    /* Ny rad. */
    ELSE IF otbOvBuffer:isCurrent THEN 
    DO:
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
/* Dette gjøres for at notatet skal følge recorden. */
  IF AVAIL OvBunt THEN DO:
    otbOvbunt:noteCurrentValue = OvBunt.OvNotat.
    otbOvBunt:docAvailable = OvBunt.OvNotat <> ''.
  END.  
    
  IF oBrwOvBunt:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  IF oBrwOvBunt:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
  END.
  oBrwOvNotat:OpenQuery().
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
  DISPLAY fiMerknad fiBehStatusLst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbOvBunt btnSplitBarY searchOvBunt tbOvBuffer first_tbOvBunt 
         prev_tbOvBunt next_tbOvBunt last_tbOvBunt new_tbOvBunt delete_tbOvBunt 
         note_tbOvBunt refresh_tbOvBunt filter_tbOvBunt excel_tbOvBunt 
         print_tbOvBunt btnUtvalgBehStatus fiMerknad fiBehStatusLst BrwOvBunt 
         BrwOvNotat delete_tbOvBuffer BrwOvBuffer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FerdigstillAvvisteRecord C-Win 
PROCEDURE FerdigstillAvvisteRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.
  IF AVAILABLE OvBunt THEN 
  DO:
    IF NOT CAN-DO('40',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare avviste reservasjonsordre kan ferdigstilles her. ").
        RETURN.
      END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Ferdigstille avvist reservasjonen?") THEN 
      RETURN.    
    
    IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "BrukerId,BuntStatus", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|60" ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved makulering/ferdigstilling av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    ELSE oBrwOvbunt:refreshRow().
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Marker reservasjonsordre som skal makuleres/ferdigstilles.").
  oBrwOvBunt:openQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FerdigstillUleverteRecord C-Win 
PROCEDURE FerdigstillUleverteRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.

  IF AVAILABLE OvBunt THEN 
  DO:
    IF NOT CAN-DO('10,30',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare nye og uleverte reservasjonsordre kan ferdigstilles her. ").
        RETURN.
      END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Ferdigstille ny/ulevert reservasjonen?") THEN 
      RETURN.    
    
    IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "BrukerId,BuntStatus", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|60" ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved ferdigstilling av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    ELSE oBrwOvbunt:refreshRow().
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Marker reservasjonsordre som skal ferdigstilles.").
  oBrwOvNotat:OpenQuery().

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
  ASSIGN
    iButNr   = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr")) 
    iKasseNr = INT(DYNAMIC-FUNCTION("getAttribute",SESSION,"kassenr")) 
    .
  
  ASSIGN 
    iFraButNrSerie = INT(
                         (IF iButNr > 99 THEN 
                             STRING(iButNr,"999") 
                          ELSE 
                            '1' + STRING(iButNr,"99")
                          ) + '0000000'
                         )
    iTilButNrSerie = INT(
                         (IF iButNr > 99 THEN 
                             STRING(iButNr,"999") 
                          ELSE 
                            '1' + STRING(iButNr,"99")
                          ) + '9999999'
                         )
    .

  IF ENTRY(1,icParam,'|') = '1' THEN 
    bFraECom = TRUE.
  ELSE 
    bFraECom = FALSE.

  IF JBoxServerAPI:Instance:Find("Syspara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 3") THEN
      ASSIGN
        cTilButNr = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1") NO-ERROR.  
  oBrwOvBunt = NEW JBoxBrowse(brwOvBunt:HANDLE).
  otbOvBunt = NEW JBoxToolbar(tbOvBunt:HANDLE).
  oBrwOvBunt:TOOLBAR-OBJECT = otbOvBunt.
  oBrwOvBunt:setSearchField(searchOvBunt:HANDLE,"BuntNr").
  oBrwOvbunt:setQuerySort("BuntNr;DESC").
  oBrwOvbunt:setNoResizeX().

  oBrwOvBunt:postUpdateProc = "ovbunt_post_update.p".
  oBrwOvBunt:customDeleteValProc = "ovbunt_delete_validate.p".
  
  oBrwOvBuffer = NEW JBoxBrowse(brwOvBuffer:HANDLE).
  oBrwOvBuffer:setParentBrowseObject(oBrwOvBunt,"BuntNr").
  
  oContainer:setSplitBarY(btnSplitBarY:HANDLE).
  oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,150). /* 200 pixels from the top, 300 pixels from the bottom */
  
  otbOvBuffer = NEW JBoxToolbar(tbOvBuffer:HANDLE).

  IF bFraECom THEN
  DO:
    otbOvBunt:addToolGroup("SendTilbutikk;Godkjenn og sendtil butikk,Avvis;Avvis reservasjon").
    otbOvBunt:disabledTools = "NEW,DELETE".
    otbOvBuffer:disabledTools = "DELETE".
    oBrwOvBunt:baseQuery = "WHERE Opphav = 10 and Buntstatus > 10".
    otbOvBuffer:addToolGroup("AvvisLinjer;Avvis linjer,AngreAvvisLinjer;Angre avvis linjer").
/*    otbOvBuffer:addTool("AngreAvvisLinjer","Angre avvis linjer",NO).*/
    cBehStatusIdList = '20|30'.
  END.
  ELSE DO:
    otbOvBunt:addToolGroup("SendECom;Godkjenn og send til eCom,UtleverKunde;Utlever til kunde,FerdigstillAvviste;Ferdigstill avviste ordre,FerdigstillUleverte;Ferdigstill uleverte ordre").
    otbOvBuffer:addTool("RegistrerLinjer","Legg til/endre varelinjer",YES).
    oBrwOvBunt:baseQuery = "WHERE Opphav = 10 AND BuntNr >= '" + STRING(iFraButNrSerie) + "' AND BuntNr <= '" + STRING(iTilButNrSerie) + "'".
    cBehStatusIdList = '10|20|30|40'.
  END.
  IF cBehStatusIdList <> '' THEN 
    DO iLoop = 1 TO NUM-ENTRIES(cBehStatusIdList,'|'):        
      IF JBoxServerAPI:Instance:Find("FIRST Syspara", "WHERE SysHId = 11 and SysGr = 10 and ParaNr = '" + ENTRY(iLoop,cBehStatusIdList,'|') + "'") THEN
          cBehStatusRowIdList = cBehStatusRowIdList + 
                                (IF cBehStatusRowIdList <> '' THEN ',' ELSE '') + 
                                JBoxServerAPI:Instance:RowIdent('sysPara').
    END.
    
  oBrwOvNotat = NEW JBoxBrowse(brwOvNotat:HANDLE).
  oBrwOvNotat:setParentBrowseObject(oBrwOvBunt,"BuntNr").
  oContainer:setNoResizeY("BrwOvBunt,BrwOvNotat,fiKode").
 
  oContainer:setFollowSplitBarY(STRING(BrwOvBunt:HANDLE) + ',' + 
                                STRING(BrwOvBuffer:HANDLE) + ',' + 
                                STRING(delete_tbOvbuffer:HANDLE) + ',' +
                                STRING(otbOvBuffer:getButtonHandle("RegistrerLinjer")) + ',' +
                                STRING(otbOvBuffer:getButtonHandle("AvvisLinjer")) + ',' +
                                STRING(otbOvBuffer:getButtonHandle("AngreAvvisLinjer")) + ',' +
                                STRING(BrwOvNotat:HANDLE) + ',' + 
                                STRING(tbOvBuffer:HANDLE)
                                ).

  oBrwOvBuffer:TOOLBAR-OBJECT = otbOvBuffer.

  hOvBunt_ovbunt_BuntStatusTekst = oBrwOvBunt:getColumnHandle("ovbunt_BuntStatusTekst").
  hOvBuffer_Antall = oBrwOvBuffer:getColumnHandle("Antall").
  
  /* Her konfigureres notat editoren slik at man ikke kan endre tidligere notater og at            */
  /* brukerid og dato/tid legges på merknaden. Se også i subprocedure NoteRecord og displayrecord. */
  otbOvBunt:NoteAddTimeStamp = YES.
  otbOvBunt:NoteSplitEditor = YES.
  ASSIGN 
    fiBehStatusLst:SCREEN-VALUE = 'Ant.valgt: ' + IF cBehStatusIdList = '' THEN '0' ELSE STRING(NUM-ENTRIES(cBehStatusIdList,'|'))
    .
  
END.
RUN setFilter.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
DO WITH FRAME {&FRAME-NAME}:
    /* JBoxServerAPI:Instance:serverTransInputParam = "RESERVASJON " ).*/

    IF otbOvBunt:isCurrent THEN 
    DO:
      /* TN 4/6-20 Ved å sende ned parametre før run super, kan disse settes ved create av record. */
      /* Trenger derfor ikke gjøre run på post_update etter run super.                             */
      oBrwOvBunt:bufferExtraFields = "Brukerid,FraButNr,TilButNr,Merknad,BuntStatus,Opphav".
      oBrwOvBunt:bufferExtraValues = JBoxSession:Instance:UserId + "|" +
                                     DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr") + "|" +
                                     cTilButNr + "|" +
                                     "RESERVASJON" + "|" +
                                     "10" + '|' +
                                     "10".
    END.
    /* Ny rad. */
    ELSE IF otbOvBuffer:isCurrent THEN 
    DO:
    END.
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    /* Ny overføringsordre. */
    IF otbOvBunt:isCurrent THEN 
    DO:
/*      IF NOT JBoxServerApi:Instance:Update("OvBunt",                                                                                                                        */
/*                                        Ovbunt.RowIdent1,                                                                                                                   */
/*                                        "Brukerid,FraButNr,TilButNr,Merknad,BuntStatus,Opphav", /* NB: Brukerid skal stå først. */                                          */
/*                                        JBoxSession:Instance:UserId + "|" + cTilButNr + "|" + DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr") + "|RESERVASJON" + '|10|10' ,*/
/*                                        FALSE,                                                                                                                              */
/*                                        "ovBunt_post_update.p",                                                                                                             */
/*                                        TRUE) THEN                                                                                                                          */
/*        DO:                                                                                                                                                                 */
/*          JBoxSession:Instance:ViewMessage("Feil opprettelse av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).                                              */
/*          RETURN.                                                                                                                                                           */
/*        END.                                                                                                                                                                */
/*      ELSE DO:                                                                                                                                                              */
/*        oBrwOvbunt:refreshRow().                                                                                                                                            */
/*        RUN RegistrerLinjerRecord.                                                                                                                                          */
/*      END.                                                                                                                                                                  */
    END.
    /* Ny rad. */
    ELSE IF otbOvBuffer:isCurrent THEN 
    DO:
    END.
    
  END.
  oBrwOvNotat:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NoteRecord C-Win 
PROCEDURE NoteRecord :
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.

  RUN SUPER.
  
  /* Standard funksjon. Her lagres teksten separat for dette programmet. */
/*  IF DYNAMIC-FUNCTION("getAttribute",hToolbar,"NoteCurrentValueChanged") = "yes" THEN          */
/*  DO:                                                                                          */
/*    DYNAMIC-FUNCTION("DoUpdate",hFieldMap:NAME,"",                                             */
/*                      "",hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,                     */
/*                      "cMerknad",DYNAMIC-FUNCTION("getAttribute",hToolbar,"NoteCurrentValue"), */
/*                     YES).                                                                     */
/*    DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).*/
/*  END.                                                                                         */
  /* Her lagres teksten i et notatfelt i recorden. */
  IF otbOvBunt:noteCurrentValueChanged THEN DO:
    IF DYNAMIC-FUNCTION('DoUpdate','Ovbunt','',
       '',OvBunt.RowIdent1,
        "OvNotat",otbOvbunt:noteCurrentValue,
       YES) THEN
      oBrwOvBunt:refreshRow().
    ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  END.
  oBrwOvNotat:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv reservasjonsordre for valgt(e) post(er)?") THEN 
        RETURN.
    IF NOT oBrwOvBunt:processRowsNoMessage("ovbunt_reservasjonsordre.p", JBoxSession:Instance:UserId) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Marker de reservasjonene som skal skrives ut før utskrift aktiveres.").
    RETURN. 
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
RUN SUPER.
  RUN setFilter.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegistrerLinjerRecord C-Win 
PROCEDURE RegistrerLinjerRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.
  
  IF AVAILABLE OvBunt THEN
  DO:
    IF INTEGER(cTilButNr) = INTEGER(DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr")) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Varelinjer kan ikke endres på reservasjon.").
        RETURN NO-APPLY.
      END.
    IF OvBunt.BuntStatus <> 10 THEN
      DO:
        JBoxSession:Instance:ViewMessage("Varelinjer kan ikke legges til reservasjon med denne status.").
        RETURN NO-APPLY.
      END.
    IF Ovbunt.DatoOppdatert <> ? THEN
      DO:
        JBoxSession:Instance:ViewMessage("Reservasjonen er oppdatert. Nye linjer kan ikke registreres.").
        RETURN NO-APPLY.
      END.
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN registrerInternoverforing.w ( OvBunt.BuntNr, Ovbunt.Merknad, Ovbunt.Opphav ).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
    /* Problem her med at vinduet fryser når det legges over varer flere ganger. */
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    oBrwOvBuffer:openQuery().
  END.
  oBrwOvNotat:OpenQuery().
  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
DEFINE VARIABLE piAntall AS INTEGER NO-UNDO.
  
  RUN SUPER.

  IF AVAILABLE OvBunt THEN 
  DO:
    hOvbunt_ovbunt_BuntStatusTekst:BGCOLOR = IF OvBunt.BuntStatus = 10 THEN 14 
                         ELSE IF OvBunt.BuntStatus = 20 THEN 13  
                         ELSE IF OvBunt.BuntStatus = 30 THEN 10  
                         ELSE IF OvBunt.BuntStatus = 40 THEN 12  
                         ELSE ?.
  END. 
  IF AVAILABLE OvBuffer THEN 
  DO:
    IF NUM-ENTRIES(Ovbuffer.Merknad,'=') = 2 THEN 
      piantall = INT(ENTRY(2,OvBuffer.Merknad,'=')).
    ELSE 
      piantall = Ovbuffer.Antall.
    hOvBuffer_Antall:BGCOLOR = IF OvBuffer.Antall <> piantall THEN 12 
                         ELSE ?.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendEComRecord C-Win 
PROCEDURE SendEComRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.

  IF AVAILABLE OvBunt THEN 
  DO:
    IF NOT CAN-DO('10',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare nye reservasjonsordre kan godkjennes. ").
        RETURN.
      END.

  IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Godkjenne og sende reservasjonen?") THEN 
    RETURN.    
  iLinjeNr = 0.
  IF JBoxServerAPI:Instance:Find("OvBuffer", "WHERE BuntNr = '" + STRING(OvBunt.BuntNr) + "' AND LinjeNr >= 1") THEN
      ASSIGN
        iLinjeNr = INTEGER(JBoxServerAPI:Instance:FieldValue("OvBuffer.LinjeNr")) NO-ERROR.  
  IF iLinjeNr = 0 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Reservasjonsordren mangler varelinjer. " + JBoxServerAPI:Instance:getCallMessage()).
      RETURN.
    END.
    
    IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "BrukerId,BuntStatus", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|20" ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved godkjenning av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    ELSE oBrwOvbunt:refreshRow().
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Marker reservasjonsordre som skal godkjennes.").
  oBrwOvNotat:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendTilbutikkRecord C-Win 
PROCEDURE SendTilbutikkRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.
  IF AVAILABLE OvBunt THEN 
  DO:
    IF NOT CAN-DO('20',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare reservasjonsordre godkjent i butikk kan godkjennes. ").
        RETURN.
      END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Godkjenne og sende reservasjonen til butikk?") THEN 
      RETURN.    
    
    IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "BrukerId,BuntStatus", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|30" ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved godkjening av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    ELSE oBrwOvbunt:refreshRow().
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Marker reservasjonsordre som skal godkjennes.").
  oBrwOvNotat:OpenQuery().

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
    /* TN 9/6-20 Denne virker hos meg, men ikke hos Gant.                                   */
    /* Sjekk opp med Brynjar hvilket program i Jukebox'en hos Gant som må oppdateres.       */
    /* Sender med parameter verdi inn i icParam i pksdl_brwcalc.p (Andre input parameteren. */    
    oBrwOvBunt:setCalcFieldParam("ovbunt_BuntStatusSkip", REPLACE(cBehStatusIdList,'|',CHR(1))).
    
    IF fiMerknad:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'Merknad'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiMerknad:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .
    IF cWhere <> '' THEN 
    DO:
      oBrwOvBunt:setFilter(cFields,cOperator,cWhere).
    END.
    
    oBrwOvBunt:openQuery().
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtleverKundeRecord C-Win 
PROCEDURE UtleverKundeRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwOvBunt:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Marker bare den ene posten som skal behandles. " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.
  IF AVAILABLE OvBunt THEN 
  DO:
    IF NOT CAN-DO('30',STRING(ovBunt.BuntStatus)) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Bare reservasjonsordre godkjent fra eCom kan utleveres til kunde. ").
        RETURN.
      END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Reservasjonen utlevert til kunde?") THEN 
      RETURN.    
    
    IF NOT JBoxServerApi:Instance:Update("OvBunt",
                                      Ovbunt.RowIdent1,
                                      "BrukerId,BuntStatus", /* NB: Brukerid skal stå først. */
                                      JBoxSession:Instance:UserId + "|50" ,
                                      FALSE,
                                      "ovBunt_post_update.p",
                                      TRUE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Feil ved utlevering av reservasjon pga. " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
      END.
    ELSE oBrwOvbunt:refreshRow().
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Marker reservasjonsordre som skal utleveres.").
/*  oBrwOvNotat:OpenQuery().*/
  oBrwOvBunt:openQuery().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

