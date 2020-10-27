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
                        Siste endring.  

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
DEFINE VARIABLE otbKOrdreLinje AS JBoxToolbar NO-UNDO.
DEFINE VARIABLE hKordre_IdColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hEkstOrdreNrColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAntallColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hNettoPrisColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hNotatColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE ddummy AS LOG NO-UNDO.

DEF VAR httKOrdreLinjeBuffer AS HANDLE NO-UNDO.
{ttKOrdre.i}
httKOrdreLinjeBuffer = BUFFER ttKOrdreLinje:HANDLE.

/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreHode ***/
DEFINE VARIABLE oBrwKOrdreHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreHode
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
    FIELD DatoTidEndret AS DATETIME
    FIELD DatoTidOpprettet AS DATETIME
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowIdent2 AS CHARACTER 
    FIELD RowIdent3 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1 RowIdent2 RowIdent3
    .
DEFINE BUFFER v_KOrdreHode FOR TEMP-TABLE KOrdreHode.

FUNCTION getBuffersAndFieldsBrwKOrdreHode RETURNS CHARACTER():
  RETURN
    'KOrdreHode'
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
     + ';DatoTidEndret'
     + ';DatoTidOpprettet'
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
  RETURN 'EACH Kunde OF KOrdreHode NO-LOCK,EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.EkstOrdreNr DESCENDING'.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreHode RETURNS CHARACTER():
  RETURN 
     'kordre_brwcalc.p' /* kordre_LevStatus */
   + ',kordre_brwcalc.p' /* kordre_ShipmentSendt */
   + ',kordre_brwcalc.p' /* kordre_Butikk */
   + ',kordre_brwcalc.p' /* kordre_FakturaNr */
     .
END FUNCTION.


/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreLinje ***/
DEF VAR oBrwKOrdreLinje AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreLinje
    FIELD KOrdreLinjeNr AS INTEGER
    FIELD VareNr AS CHARACTER
    FIELD Varetekst AS CHARACTER
    FIELD kordrelinje_LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD Storl AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD NettoPris AS DECIMAL
    FIELD Pris AS DECIMAL
    FIELD Linjesum AS DECIMAL
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
     + ';VareNr'
     + ';Varetekst'
     + ';LevFargKod'
     + ';Storl'
     + ';Antall'
     + ';NettoPris'
     + ';Pris'
     + ';Linjesum'
     + ';Notat'
     + ';+kordrelinje_LevKod|CHARACTER||kordrelinje_LevKod|Lev.art.nr'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreLinje RETURNS CHARACTER():
  RETURN 
     'kordrelinje_brwcalc.p' /* kordrelinje_LevKod */
     .
END FUNCTION.


DEF VAR oFmKOrdreLinje AS JBoxFieldMap NO-UNDO.

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
&Scoped-define INTERNAL-TABLES KOrdreHode KOrdreLinje

/* Definitions for BROWSE BrwKOrdreHode                                 */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.Navn ~
KOrdreHode.KOrdre_Id KOrdreHode.kordre_LevStatus ~
KOrdreHode.LevFormBeskrivelse KOrdreHode.EkstOrdreNr KOrdreHode.Totalt ~
KOrdreHode.AntApnet KOrdreHode.AntPPEti KOrdreHode.Leveringsdato ~
KOrdreHode.SendingsNr KOrdreHode.ReturNr KOrdreHode.ShipmentSendt ~
KOrdreHode.MobilTlf KOrdreHode.kordre_Butikk KOrdreHode.LevAdresse1 ~
KOrdreHode.LevPostNr KOrdreHode.LevPostSted KOrdreHode.LevLand ~
KOrdreHode.LevStatus KOrdreHode.cOpt1 KOrdreHode.LevFNr ~
KOrdreHode.ePostAdresse KOrdreHode.Opphav KOrdreHode.KundeNr ~
KOrdreHode.Faktura_Id KOrdreHode.kordre_FakturaNr KOrdreHode.DatoTidEndret ~
KOrdreHode.DatoTidOpprettet 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.Navn 
&Scoped-define QUERY-STRING-BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK ~
    BY KOrdreHode.EkstOrdreNr DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreHode OPEN QUERY BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK ~
    BY KOrdreHode.EkstOrdreNr DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode


/* Definitions for BROWSE BrwKOrdreLinje                                */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdreLinjeNr ~
KOrdreLinje.VareNr KOrdreLinje.Varetekst KOrdreLinje.kordrelinje_LevKod ~
KOrdreLinje.LevFargKod KOrdreLinje.Storl KOrdreLinje.Antall ~
KOrdreLinje.NettoPris KOrdreLinje.Pris KOrdreLinje.Linjesum ~
KOrdreLinje.Notat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreLinje ~
KOrdreLinje.KOrdreLinjeNr 
&Scoped-define QUERY-STRING-BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreLinje OPEN QUERY BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbKOrdreHode searchKOrdreHode btnSplitBarY ~
RECT-5 tbKOrdreHode-10 first_tbKOrdreHode prev_tbKOrdreHode ~
next_tbKOrdreHode last_tbKOrdreHode refresh_tbKOrdreHode ~
filter_tbKOrdreHode excel_tbKOrdreHode multiSortBrowse_tbKOrdreHode ~
browseconfig_tbKOrdreHode BrwKOrdreHode KOrdre_Id EkstOrdreNr KundeNr Navn ~
first_tbKOrdreHode-10 prev_tbKOrdreHode-10 next_tbKOrdreHode-10 ~
last_tbKOrdreHode-10 new_tbKOrdreHode copy_tbKOrdreHode delete_tbKOrdreHode ~
refresh_tbKOrdreHode-10 excel_tbKOrdreHode-10 fiStrekkode BrwKOrdreLinje 
&Scoped-Define DISPLAYED-OBJECTS KOrdre_Id EkstOrdreNr KundeNr Navn ~
fiStrekkode 

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

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 163 BY .43.

DEFINE BUTTON copy_tbKOrdreHode 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbKOrdreHode 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbKOrdreHode 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON excel_tbKOrdreHode-10 
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

DEFINE BUTTON first_tbKOrdreHode-10 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbKOrdreHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON last_tbKOrdreHode-10 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON multiSortBrowse_tbKOrdreHode 
     IMAGE-UP FILE "bmp/bullet_triangle_green.bmp":U
     LABEL "Sorter på flere kolonner" 
     SIZE 4.6 BY 1.1 TOOLTIP "Sorter på flere kolonner (ALT-S)".

DEFINE BUTTON new_tbKOrdreHode 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbKOrdreHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON next_tbKOrdreHode-10 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKOrdreHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON prev_tbKOrdreHode-10 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKOrdreHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON refresh_tbKOrdreHode-10 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE EkstOrdreNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Ekst.ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Ordrenummer fra eksternt system (Importert ordre)".

DEFINE VARIABLE fiStrekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 39.6 BY 1 TOOLTIP "Strekkode på vare som skal registreres" NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "KOrdre Id" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Internt faktura id. Tildeles autmatisk av systemet.".

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Kundenummer".

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Navn eller firmanavn".

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.6 BY 10.24.

DEFINE RECTANGLE searchKOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY .91.

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 8  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 163 BY 1.29.

DEFINE RECTANGLE tbKOrdreHode-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 47 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwKOrdreHode FOR 
      KOrdreHode SCROLLING.

DEFINE QUERY BrwKOrdreLinje FOR 
      KOrdreLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwKOrdreHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreHode C-Win _STRUCTURED
  QUERY BrwKOrdreHode NO-LOCK DISPLAY
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
      KOrdreHode.DatoTidEndret COLUMN-LABEL "Endret" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      KOrdreHode.DatoTidOpprettet COLUMN-LABEL "DatoTidOpprettet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
  ENABLE
      KOrdreHode.Navn HELP "Navn eller firmanavn"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 90 BY 10.24 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.

DEFINE BROWSE BrwKOrdreLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreLinje C-Win _STRUCTURED
  QUERY BrwKOrdreLinje NO-LOCK DISPLAY
      KOrdreLinje.KOrdreLinjeNr COLUMN-LABEL "KOrdreLinje" FORMAT ">>>>>>9":U
      KOrdreLinje.VareNr COLUMN-LABEL "VareNr" FORMAT "X(20)":U
      KOrdreLinje.Varetekst COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      KOrdreLinje.kordrelinje_LevKod COLUMN-LABEL "Lev.art.nr" FORMAT "X(15)":U
      KOrdreLinje.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(15)":U
      KOrdreLinje.Storl COLUMN-LABEL "Str" FORMAT "x(10)":U
      KOrdreLinje.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9":U
      KOrdreLinje.NettoPris COLUMN-LABEL "Nettopris" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Pris COLUMN-LABEL "Pris" FORMAT "->>,>>9.99":U
            WIDTH 12.2
      KOrdreLinje.Linjesum COLUMN-LABEL "Linjesum" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Notat COLUMN-LABEL "Notat" FORMAT "X(40)":U
  ENABLE
      KOrdreLinje.KOrdreLinjeNr HELP "Linjenummer på faktura"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 163 BY 7.62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 14.29 COL 2 WIDGET-ID 52
     first_tbKOrdreHode AT ROW 1.33 COL 2 WIDGET-ID 12
     prev_tbKOrdreHode AT ROW 1.33 COL 6.8 WIDGET-ID 14
     next_tbKOrdreHode AT ROW 1.33 COL 11.4 WIDGET-ID 16
     last_tbKOrdreHode AT ROW 1.33 COL 16 WIDGET-ID 18
     refresh_tbKOrdreHode AT ROW 1.33 COL 20.6 WIDGET-ID 6
     filter_tbKOrdreHode AT ROW 1.33 COL 25.2 WIDGET-ID 28
     excel_tbKOrdreHode AT ROW 1.33 COL 29.8 WIDGET-ID 8
     multiSortBrowse_tbKOrdreHode AT ROW 1.33 COL 34.4 WIDGET-ID 20
     browseconfig_tbKOrdreHode AT ROW 1.33 COL 39 WIDGET-ID 24
     BrwKOrdreHode AT ROW 3.86 COL 2 WIDGET-ID 200
     KOrdre_Id AT ROW 4.33 COL 106.4 COLON-ALIGNED
     EkstOrdreNr AT ROW 5.33 COL 106.4 COLON-ALIGNED
     KundeNr AT ROW 6.33 COL 106.4 COLON-ALIGNED
     Navn AT ROW 7.33 COL 106.4 COLON-ALIGNED
     first_tbKOrdreHode-10 AT ROW 14.86 COL 2.2 WIDGET-ID 34
     prev_tbKOrdreHode-10 AT ROW 14.86 COL 6.8 WIDGET-ID 36
     next_tbKOrdreHode-10 AT ROW 14.86 COL 11.6 WIDGET-ID 38
     last_tbKOrdreHode-10 AT ROW 14.86 COL 16.2 WIDGET-ID 40
     new_tbKOrdreHode AT ROW 14.86 COL 21 WIDGET-ID 42
     copy_tbKOrdreHode AT ROW 14.86 COL 25.6 WIDGET-ID 44
     delete_tbKOrdreHode AT ROW 14.86 COL 30.4 WIDGET-ID 46
     refresh_tbKOrdreHode-10 AT ROW 14.86 COL 35 WIDGET-ID 48
     excel_tbKOrdreHode-10 AT ROW 14.86 COL 39.8 WIDGET-ID 50
     fiStrekkode AT ROW 14.91 COL 59.4 COLON-ALIGNED
     BrwKOrdreLinje AT ROW 16.1 COL 2 WIDGET-ID 300
     tbKOrdreHode AT ROW 1.24 COL 1 WIDGET-ID 2
     searchKOrdreHode AT ROW 2.67 COL 2 WIDGET-ID 26
     RECT-5 AT ROW 3.86 COL 92.4 WIDGET-ID 30
     tbKOrdreHode-10 AT ROW 14.76 COL 2 WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 165 BY 23.52 WIDGET-ID 100.


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
         HEIGHT             = 23.52
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
/* BROWSE-TAB BrwKOrdreHode browseconfig_tbKOrdreHode DEFAULT-FRAME */
/* BROWSE-TAB BrwKOrdreLinje fiStrekkode DEFAULT-FRAME */
ASSIGN 
       BrwKOrdreHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1
       BrwKOrdreHode:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

ASSIGN 
       tbKOrdreHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh¤enable,filter;Filter,excel;Eksporter til E&xcel,multiSortBrowse;Sorter på flere kolonner,browseconfig;Column setupmaxborder".

ASSIGN 
       tbKOrdreHode-10:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,copy;Kopier,delete;Slett,refresh;Refresh,excel;Eksporter til E&xcelmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreHode
/* Query rebuild information for BROWSE BrwKOrdreHode
     _TblList          = "skotex.KOrdreHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.KOrdreHode.EkstOrdreNr|no"
     _FldNameList[1]   > "_<CALC>"
"KOrdreHode.Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "Navn eller firmanavn" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreHode.KOrdre_Id" "Ordrenummer" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreHode.kordre_LevStatus" "Leveringsstatus" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreHode.LevFormBeskrivelse" "Leveringsform" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Kort beskrivelse av leveringsform." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreHode.EkstOrdreNr" "Ekst.ordrenr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Ordrenummer fra eksternt system (Importert ordre)" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreHode.Totalt" "Totalt" "->>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Totalt beløp på faktura" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreHode.AntApnet" "Ant. pksdl" ">9" "INTEGER" ? ? ? ? ? ? no "Antall ganger ordren er åpnet etter første utlevering." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreHode.AntPPEti" "Antall etiketter" ">9" "INTEGER" ? ? ? ? ? ? no "Antall postpakke etiketter utskrivet." no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreHode.Leveringsdato" "Leveringsdato" "99/99/99" "DATE" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreHode.SendingsNr" "Sendingsnummer" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Sendingsnummer - for sporing." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreHode.ReturNr" "Retur nr." "x(30)" "CHARACTER" ? ? ? ? ? ? no "Returnr for sporing. Påført returetikett." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreHode.ShipmentSendt" "Shipment sendt" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreHode.MobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreHode.kordre_Butikk" "Butikk" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KOrdreHode.LevAdresse1" "Leveringsadresse" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Kundens adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KOrdreHode.LevPostNr" "Lev. PostNr" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Postnummer" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreHode.LevPostSted" "Poststed" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Poststed" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KOrdreHode.LevLand" "Lev. Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KOrdreHode.LevStatus" "Lev.status" "x(2)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreHode.cOpt1" "cOpt1" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KOrdreHode.LevFNr" "Leveringsform" ">9" "INTEGER" ? ? ? ? ? ? no "Leveringsvorm" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreHode.ePostAdresse" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KOrdreHode.Opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Ordre opphav. F.eks. 1-Manuell reg. 2-Nettbutikk." no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"KOrdreHode.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"KOrdreHode.Faktura_Id" "FakturaId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kobling mellom faktura og kundeordre" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"KOrdreHode.kordre_FakturaNr" "FakturaNr" "X(13)" "CHARACTER" ? ? ? ? ? ? no "" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"KOrdreHode.DatoTidEndret" "Endret" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for siste endring på ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"KOrdreHode.DatoTidOpprettet" "DatoTidOpprettet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for opprettelse av ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreLinje
/* Query rebuild information for BROWSE BrwKOrdreLinje
     _TblList          = "skotex.KOrdreLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KOrdreLinje.KOrdreLinjeNr" "KOrdreLinje" ">>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Linjenummer på faktura" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreLinje.VareNr" "VareNr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Varenummer" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreLinje.Varetekst" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Varetekst" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreLinje.kordrelinje_LevKod" "Lev.art.nr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreLinje.LevFargKod" "LevFargKod" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreLinje.Storl" "Str" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreLinje.Antall" "Antall" "->>,>>9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreLinje.NettoPris" "Nettopris" "->>>,>>9.99" "decimal" ? ? ? ? ? ? no "Nettopris. Pris eksklusive mva og rabatter." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreLinje.Pris" "Pris" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris (Til kunde)" no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreLinje.Linjesum" "Linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreLinje.Notat" "Notat" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Informasjon til kunde om varen." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreLinje */
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
    EMPTY TEMP-TABLE ttKOrdreHode.
    EMPTY TEMP-TABLE ttKORdreLinje.
    EMPTY TEMP-TABLE ttArtBas.
    
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
  DISPLAY KOrdre_Id EkstOrdreNr KundeNr Navn fiStrekkode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbKOrdreHode searchKOrdreHode btnSplitBarY RECT-5 tbKOrdreHode-10 
         first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode 
         last_tbKOrdreHode refresh_tbKOrdreHode filter_tbKOrdreHode 
         excel_tbKOrdreHode multiSortBrowse_tbKOrdreHode 
         browseconfig_tbKOrdreHode BrwKOrdreHode KOrdre_Id EkstOrdreNr KundeNr 
         Navn first_tbKOrdreHode-10 prev_tbKOrdreHode-10 next_tbKOrdreHode-10 
         last_tbKOrdreHode-10 new_tbKOrdreHode copy_tbKOrdreHode 
         delete_tbKOrdreHode refresh_tbKOrdreHode-10 excel_tbKOrdreHode-10 
         fiStrekkode BrwKOrdreLinje 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DEFINE VARIABLE pcLevStatusLst AS CHARACTER NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

  pcLevStatusLst = '30,35,40,45,47,55'.
  oBrwKOrdreHode:baseQuery = "WHERE LevStatus >= '30' AND Levstatus <= '47' AND Opphav = '10'".

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
/*DEF VAR oBrwDropDownAntall AS JBoxBrowseDropDown NO-UNDO.*/
DEF VAR oBrwFillInAntall AS JBoxBrowseFillIn NO-UNDO.
DEF VAR oBrwFillInNettoPris AS JBoxBrowseFillIn NO-UNDO.
DEF VAR oBrwFillInNotat AS JBoxBrowseFillIn NO-UNDO.

&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

DO WITH FRAME {&FRAME-NAME}:
  SUBSCRIBE TO 'OrdrebehandlingOpenQuery' ANYWHERE.
    
  oBrwKOrdreHode = NEW JBoxBrowse(brwKOrdreHode:HANDLE).
  oBrwKOrdreHode:setNoResizeY().
  otbKOrdreHode = NEW JBoxToolbar(tbKOrdreHode:HANDLE).
/*  otbKOrdreLinje = NEW JBoxToolbar(tbKOrdreLinje:HANDLE).*/
  oBrwKOrdreHode:setQuerySort("EkstOrdreNr;DESC").
  oBrwKOrdreHode:setSearchField(searchKOrdreHode:HANDLE,"EkstOrdreNr").
  oBrwKOrdreHode:setNoResizeX().
  
  otbKOrdreHode:addToolGroup("SkrivPkSdl;Pakkseddel,SkrivFaktura;Skriv faktura").

  oBrwKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.
  hKOrdre_IdColumn = oBrwKOrdreHode:getColumnHandle("KOrdre_Id").
  hEkstOrdreNrColumn = oBrwKOrdreHode:getColumnHandle("EkstOrdreNr").
    
  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20") THEN
    cNettButikkType = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1").
  
  RUN InitializeComponents.
  
  oBrwKOrdreLinje = NEW JBoxBrowse(brwKOrdreLinje:HANDLE).
  oBrwKOrdreLinje:setParentBrowseObject(oBrwKOrdreHode,"KOrdre_Id").

  hAntallColumn = oBrwKOrdreLinje:getColumnHandle("Antall").
  oBrwFillInAntall = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"Antall",TRUE).
  hNettoPrisColumn = oBrwKOrdreLinje:getColumnHandle("NettoPris").
  oBrwFillInNettoPris = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"NettoPris",TRUE).
  hNotatColumn = oBrwKOrdreLinje:getColumnHandle("Notat").
  oBrwFillInNotat = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"Notat",TRUE).
   
  oBrwKOrdreLinje:enableOnDblClick = YES.
  oFmKOrdreLinje = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmKOrdreLinje:updateFields = 'KOrdre_Id,EkstOrdreNr,KundeNr,Navn'.
  oFmKOrdreLinje:primaryKeyFields = 'KOrdre_Id'.

  oFmKOrdreLinje:BROWSE-OBJECT = oBrwKOrdreHode.
  oContainer:setSplitBarY(btnSplitBarY:HANDLE).
  oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,300). /* 200 pixels from the top, 300 pixels from the bottom */
/*  oContainer:setFollowSplitBarY(STRING(BrwKOrdreHode:HANDLE) + ',' +        */
/*                                STRING(BrwKOrdreLinje:HANDLE) + ',' +       */
/*                                STRING(first_tbKOrdreLinje:HANDLE) + ',' +  */
/*                                STRING(prev_tbKOrdreLinje:HANDLE) + ',' +   */
/*                                STRING(next_tbKOrdreLinje:HANDLE) + ',' +   */
/*                                STRING(last_tbKOrdreLinje:HANDLE) + ',' +   */
/*                                STRING(new_tbKOrdreLinje:HANDLE) + ',' +    */
/*                                STRING(refresh_tbKOrdreLinje:HANDLE) + ',' +*/
/*                                STRING(copy_tbKOrdreLinje:HANDLE) + ',' +   */
/*                                STRING(delete_tbKOrdreLinje:HANDLE) + ',' + */
/*                                STRING(excel_tbKOrdreLinje:HANDLE) + ',' +  */
/*                                STRING(fiStrekkode:HANDLE)                  */
/*                                ).                                          */
  oContainer:setNoResizeY("BrwKOrdreHode,RECT-5").
END.
oBrwKOrdreLinje:OpenQuery().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.


/*IF ihFillIn:INPUT-VALUE > ihBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE OR ihFillIn:INPUT-VALUE = 0 THEN DO:*/
/*  DYNAMIC-FUNCTION("DoMessage",0,0,"Ugyldig verdi","","").                                                 */
/*  obOK = NO.                                                                                               */
/*  RETURN.                                                                                                  */
/*END.                                                                                                       */
/*ihBuffer:BUFFER-FIELD("LevertAntall"):BUFFER-VALUE = ihFillIn:INPUT-VALUE.                                 */

    IF ihBuffer:BUFFER-FIELD("VareNr"):BUFFER-VALUE = 'BETALT' THEN 
    DO:
        JBoxSession:Instance:ViewMessage("Det kan ikke gjøres endringer på en betalingsrad.").            
        RETURN NO-APPLY.
    END.
    IF hAntallColumn:NAME = ihFillIn:NAME THEN 
    DO:
        IF ihFillIn:INPUT-VALUE < 0 THEN 
        DO:
            JBoxSession:Instance:ViewMessage("Ugyldig verdi. Antall kan ikke settes negativt.").            
            RETURN NO-APPLY.
        END.
    END. 
    ELSE IF hNettoPrisColumn:NAME = ihFillIn:NAME THEN 
    DO:
        IF ihFillIn:INPUT-VALUE < 0 THEN 
        DO:
            JBoxSession:Instance:ViewMessage("Pris kan ikke settes mindre enn 0.").            
            RETURN NO-APPLY.
        END.
    END. 
    DYNAMIC-FUNCTION("setPostUpdProc","kordrelinje_post_update.p").
    obOK = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,"",
                "",
                ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
                ihFillIn:SCREEN-VALUE,
                TRUE).
    oBrwKOrdreLinje:refresh(). 
    oBrwKOrdreHode:refreshRow(). 
                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DEF VAR hTempTable AS HANDLE NO-UNDO.
    DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
    
    EMPTY TEMP-TABLE ttKOrdreHode.
    EMPTY TEMP-TABLE ttKORdreLinje.
    EMPTY TEMP-TABLE ttArtBas.
    
    /* Her returneres ttKORdreLinje, og kan benyttes statis */
    IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_manko.p",'',httKOrdreLinjeBuffer) THEN
      hTempTable = JBoxServerAPI:Instance:getCallReturnTable().

/*    hTempTable:WRITE-JSON('file', 'C:\NSoft\Polygon\PRS\konv\MankoKOrdreLinje.json', TRUE).*/
    
    RUN SUPER.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OrdrebehandlingOpenQuery C-Win 
PROCEDURE OrdrebehandlingOpenQuery :
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
        IF CAN-FIND(FIRST ttKOrdreLinje WHERE 
                    ttKOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id AND 
                    ttKOrdreLinje.Manko = TRUE) THEN 
            hEkstORdreNrColumn:BGCOLOR = 11.   
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPkSdlRecord C-Win 
PROCEDURE SkrivPkSdlRecord :
/*RUN SUPER.*/

  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv pakkseddel for valgt(e) post(er)?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_pakkseddel.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv pakkseddel for alle poster?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_pakkseddel.p","").
  END.
  
  PUBLISH 'OrdrebehandlingOpenQuery'.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilPlukkRecord C-Win 
PROCEDURE TilPlukkRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send ordre for valgt(e) post(er) til plukking?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_plukkliste.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send ordre for alle poster til plukking?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_plukkliste.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilSpeditorRecord C-Win 
PROCEDURE TilSpeditorRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for valgt(e) post(er) som levert til speditør?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_LeverSpeditor.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for alle poster som levert til speditør?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_LeverSpeditor.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

