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
DEFINE INPUT PARAMETER icParam AS CHARACTER NO-UNDO.
 
/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bOk               AS LOG             NO-UNDO.
DEFINE VARIABLE ix                AS INTEGER         NO-UNDO.
DEFINE VARIABLE hBrowse           AS HANDLE          NO-UNDO.
DEFINE VARIABLE hQuery            AS HANDLE          NO-UNDO.
DEFINE VARIABLE hToolbar          AS HANDLE          NO-UNDO.
DEFINE VARIABLE hFieldMap         AS HANDLE          NO-UNDO.
DEFINE VARIABLE oContainer        AS JBoxContainer   NO-UNDO.
DEFINE VARIABLE hbfKOrdre_Id      AS HANDLE          NO-UNDO.
DEFINE VARIABLE hbcKOrdre_Id      AS HANDLE          NO-UNDO.
DEFINE VARIABLE otbKOrdreHode     AS JBoxToolbar     NO-UNDO.
DEFINE VARIABLE otbKOrdreLinje    AS JBoxToolbar     NO-UNDO.
DEFINE VARIABLE hKordre_IdColumn  AS HANDLE          NO-UNDO.
DEFINE VARIABLE cNettButikkType   AS CHARACTER       NO-UNDO.
DEFINE VARIABLE cRowIdList        AS CHAR            NO-UNDO.
DEFINE VARIABLE oSelector         AS JBoxDynSelector NO-UNDO.
DEFINE VARIABLE hLinjeAktivColumn AS HANDLE          NO-UNDO.
DEFINE VARIABLE pcFeltVerdier AS CHARACTER NO-UNDO.
DEF VAR opopupVare AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE icModus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEkstOrdreNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEF VAR hTempTable AS HANDLE NO-UNDO.
DEFINE VARIABLE hFil AS HANDLE NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttKOrdrePostPakke NO-UNDO
  FIELD KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  .

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreHode ***/
DEFINE VARIABLE oBrwKOrdreHode    AS JBoxBrowse      NO-UNDO.
DEF TEMP-TABLE KOrdreHode
    FIELD DatoTidOpprettet AS DATETIME
    FIELD kordre_PDF AS LOGICAL
    FIELD BrukerID AS CHARACTER
    FIELD Navn AS CHARACTER
    FIELD KOrdre_Id AS DECIMAL
    FIELD RefKOrdre_Id AS DECIMAL
    FIELD kordre_LevStatus AS CHARACTER
    FIELD LevFormBeskrivelse AS CHARACTER
    FIELD EkstOrdreNr AS CHARACTER
    FIELD Totalt AS DECIMAL
    FIELD ShipmentSendt AS CHARACTER
    FIELD kordre_webcurrentstatus AS CHARACTER
    FIELD kordre_webSent AS CHARACTER
    FIELD kordre_ReturFraBut AS CHARACTER
    FIELD AntApnet AS INTEGER
    FIELD AntPPEti AS INTEGER
    FIELD Leveringsdato AS DATE
    FIELD ReturNr AS CHARACTER
    FIELD SendingsNr AS CHARACTER
    FIELD kordre_cOpt1 AS CHARACTER
    FIELD MobilTlf AS CHARACTER
    FIELD kordre_Butikk AS CHARACTER
    FIELD kordre_LevAdresse AS CHARACTER
    FIELD LevPostNr AS CHARACTER
    FIELD LevPostSted AS CHARACTER
    FIELD LevLand AS CHARACTER
    FIELD LevStatus AS CHARACTER
    FIELD LevFNr AS INTEGER
    FIELD ePostAdresse AS CHARACTER
    FIELD Opphav AS INTEGER
    FIELD KundeNr AS DECIMAL
    FIELD Faktura_Id AS DECIMAL
    FIELD kordre_FakturaNr AS CHARACTER
    FIELD kordre_AntIkkeRet AS INTEGER
    FIELD ButikkNr AS INTEGER
    FIELD RegistrertDato AS DATE
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
     + ';BrukerID'
     + ';KOrdre_Id'
     + ';RefKOrdre_Id'
     + ';EkstOrdreNr'
     + ';Totalt'
     + ';AntApnet'
     + ';AntPPEti'
     + ';Leveringsdato'
     + ';ReturNr'
     + ';SendingsNr'
     + ';MobilTlf'
     + ';LevPostNr'
     + ';LevPostSted'
     + ';LevLand'
     + ';LevStatus'
     + ';LevFNr'
     + ';ePostAdresse'
     + ';Opphav'
     + ';KundeNr'
     + ';Faktura_Id'
     + ';ButikkNr'
     + ';RegistrertDato'
     + ';+kordre_PDF|LOGICAL||kordre_PDF|PDF'
     + ';+kordre_LevStatus|CHARACTER||kordre_LevStatus|Leveringsstatus'
     + ';+ShipmentSendt|CHARACTER||kordre_ShipmentSendt|Shipment sendt'
     + ';+kordre_webcurrentstatus|CHARACTER||kordre_webcurrentstatus|Overf.status'
     + ';+kordre_webSent|CHARACTER||kordre_webSent|Overf.dato/tid'
     + ';+kordre_ReturFraBut|CHARACTER||kordre_ReturFraBut|Retur fra butikk'
     + ';+kordre_cOpt1|CHARACTER||kordre_cOpt1|cOpt1'
     + ';+kordre_Butikk|CHARACTER||kordre_Butikk|Butikk'
     + ';+kordre_LevAdresse|CHARACTER||kordre_LevAdresse|LevAdresse'
     + ';+kordre_FakturaNr|CHARACTER||kordre_FakturaNr|FakturaNr'
     + ';+kordre_AntIkkeRet|INTEGER||kordre_AntIkkeRet|AntIkkeRet'
  + ',Kunde'
     + ';Navn'
  + ',LeveringsForm'
     + ';LevFormBeskrivelse'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreHode RETURNS CHARACTER():
  RETURN 'EACH Kunde OF KOrdreHode NO-LOCK,EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.DatoTidOpprettet DESCENDING'.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreHode RETURNS CHARACTER():
  RETURN 
     'server/kordre_brwcalc.p' /* kordre_PDF */
   + ',server/kordre_brwcalc.p' /* kordre_LevStatus */
   + ',server/kordre_brwcalc.p' /* kordre_ShipmentSendt */
   + ',server/kordre_brwcalc.p' /* kordre_webcurrentstatus */
   + ',server/kordre_brwcalc.p' /* kordre_webSent */
   + ',server/kordre_brwcalc.p' /* kordre_ReturFraBut */
   + ',server/kordre_brwcalc.p' /* kordre_cOpt1 */
   + ',server/kordre_brwcalc.p' /* kordre_Butikk */
   + ',server/kordre_brwcalc.p' /* kordre_LevAdresse */
   + ',server/kordre_brwcalc.p' /* kordre_FakturaNr */
   + ',server/kordre_brwcalc.p' /* kordre_AntIkkeRet */
     .
END FUNCTION.


/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreLinje ***/
DEF VAR oBrwKOrdreLinje AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreLinje
    FIELD KOrdreLinjeNr AS INTEGER
    FIELD KopiKOrdreLinjeNr AS INTEGER
    FIELD ByttetKOrdreLinjeNr AS INTEGER
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
    FIELD kordlinje_returkode AS CHARACTER
    FIELD BetRef AS CHARACTER
    FIELD Notat AS CHARACTER
    FIELD KOrdre_Id AS DECIMAL
    FIELD Pris AS DECIMAL
    FIELD PlukkButikk AS INTEGER
    FIELD UtleverButikk AS INTEGER
    FIELD DatoTidOpprettet AS DATETIME
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
     + ';ByttetKOrdreLinjeNr'
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
     + ';KOrdre_Id'
     + ';Pris'
     + ';PlukkButikk'
     + ';UtleverButikk'
     + ';DatoTidOpprettet'
     + ';+kordrelinje_LevKod|CHARACTER||kordrelinje_LevKod|kordrelinje_LevKod'
     + ';+kordlinje_returkode|CHARACTER||kordlinje_returkode|kordlinje_returkode'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreLinje RETURNS CHARACTER():
  RETURN 
     'server/kordrelinje_brwcalc.p' /* kordrelinje_LevKod */
   + ',server/kordrelinje_brwcalc.p' /* kordlinje_returkode */
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
&Scoped-define INTERNAL-TABLES KOrdreHode KOrdreLinje

/* Definitions for BROWSE BrwKOrdreHode                                 */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.DatoTidOpprettet ~
KOrdreHode.kordre_PDF KOrdreHode.BrukerID KOrdreHode.Navn ~
KOrdreHode.KOrdre_Id KOrdreHode.RefKOrdre_Id KOrdreHode.kordre_LevStatus ~
KOrdreHode.LevFormBeskrivelse KOrdreHode.EkstOrdreNr KOrdreHode.Totalt ~
KOrdreHode.ShipmentSendt KOrdreHode.kordre_webcurrentstatus ~
KOrdreHode.kordre_webSent KOrdreHode.kordre_ReturFraBut KOrdreHode.AntApnet ~
KOrdreHode.AntPPEti KOrdreHode.Leveringsdato KOrdreHode.ReturNr ~
KOrdreHode.SendingsNr KOrdreHode.kordre_cOpt1 KOrdreHode.MobilTlf ~
KOrdreHode.kordre_Butikk KOrdreHode.kordre_LevAdresse KOrdreHode.LevPostNr ~
KOrdreHode.LevPostSted KOrdreHode.LevLand KOrdreHode.LevStatus ~
KOrdreHode.LevFNr KOrdreHode.ePostAdresse KOrdreHode.Opphav ~
KOrdreHode.KundeNr KOrdreHode.Faktura_Id KOrdreHode.kordre_FakturaNr ~
KOrdreHode.kordre_AntIkkeRet KOrdreHode.ButikkNr KOrdreHode.RegistrertDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreHode ~
KOrdreHode.DatoTidOpprettet KOrdreHode.Navn 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define QUERY-STRING-BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK, ~
    EACH Kunde OF KOrdreHode NO-LOCK, ~
    EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.DatoTidOpprettet DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreHode OPEN QUERY BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK, ~
    EACH Kunde OF KOrdreHode NO-LOCK, ~
    EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.DatoTidOpprettet DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode


/* Definitions for BROWSE BrwKOrdreLinje                                */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdreLinjeNr ~
KOrdreLinje.KopiKOrdreLinjeNr KOrdreLinje.ByttetKOrdreLinjeNr ~
KOrdreLinje.Aktiv KOrdreLinje.Returnert KOrdreLinje.VareNr ~
KOrdreLinje.Varetekst KOrdreLinje.kordrelinje_LevKod KOrdreLinje.LevFargKod ~
KOrdreLinje.Storl KOrdreLinje.Kode KOrdreLinje.Antall KOrdreLinje.NettoPris ~
KOrdreLinje.Linjesum KOrdreLinje.kordlinje_returkode KOrdreLinje.BetRef ~
KOrdreLinje.Notat KOrdreLinje.KOrdre_Id KOrdreLinje.Pris ~
KOrdreLinje.PlukkButikk KOrdreLinje.UtleverButikk ~
KOrdreLinje.DatoTidOpprettet 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreLinje ~
KOrdreLinje.KOrdreLinjeNr 
&Scoped-define QUERY-STRING-BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreLinje OPEN QUERY BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY tbKOrdreHode searchKOrdreHode ~
RECT-4 tbKOrdreLinje RECT-5 first_tbKOrdreHode prev_tbKOrdreHode ~
next_tbKOrdreHode last_tbKOrdreHode delete_tbKOrdreHode ~
refresh_tbKOrdreHode filter_tbKOrdreHode excel_tbKOrdreHode ~
multiSortBrowse_tbKOrdreHode browseconfig_tbKOrdreHode ficSporingsnr ~
ficEkstOrdreNr BtnBlank fiEkstOrdreNr fiDato BrwKOrdreHode ~
first_tbKOrdreLinje prev_tbKOrdreLinje next_tbKOrdreLinje ~
last_tbKOrdreLinje delete_tbKOrdreLinje RegistrerLinjer_tbKOrdreLinje ~
BrwKOrdreLinje 
&Scoped-Define DISPLAYED-OBJECTS fiTEXT ficSporingsnr ficEkstOrdreNr ~
fiEkstOrdreNr fiDato 

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

DEFINE BUTTON BtnBlank 
     LABEL "<Blank>" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 163 BY .43.

DEFINE BUTTON delete_tbKOrdreHode 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON delete_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

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

DEFINE BUTTON first_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbKOrdreHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON last_tbKOrdreLinje 
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

DEFINE BUTTON next_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKOrdreHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON prev_tbKOrdreLinje 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKOrdreHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON RegistrerLinjer_tbKOrdreLinje 
     LABEL "Registrer linjer" 
     SIZE 17 BY 1.1 TOOLTIP "Registrere linjer som skal returneres.".

DEFINE VARIABLE ficEkstOrdreNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksternt ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 33.2 BY 1
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE ficSporingsnr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sendingsnr" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 TOOLTIP "Skann pakkseddel/faktura strekkode for å hente frem ordren"
     BGCOLOR 11  NO-UNDO.

DEFINE VARIABLE fiDato AS DATE FORMAT "99/99/99":U 
     LABEL "Registrert dato" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Returer registrert fra og med dato" NO-UNDO.

DEFINE VARIABLE fiEkstOrdreNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekst.ordrenummer" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 TOOLTIP "Filter på eksternt ordrenummer." NO-UNDO.

DEFINE VARIABLE fiTEXT AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 43 BY 1
     BGCOLOR 14 FGCOLOR 1 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 163 BY 1.91.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 1.52.

DEFINE RECTANGLE searchKOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49 BY .91.

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 8  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 163 BY 1.29.

DEFINE RECTANGLE tbKOrdreLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 162 BY 1.29.

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
      KOrdreHode.DatoTidOpprettet COLUMN-LABEL "DatoTidOpprettet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      KOrdreHode.kordre_PDF COLUMN-LABEL "PDF" FORMAT "*/":U
      KOrdreHode.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      KOrdreHode.Navn COLUMN-LABEL "Navn" FORMAT "X(40)":U
      KOrdreHode.KOrdre_Id COLUMN-LABEL "Ordrenummer" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.RefKOrdre_Id COLUMN-LABEL "KOId" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.kordre_LevStatus COLUMN-LABEL "Leveringsstatus" FORMAT "X(30)":U
      KOrdreHode.LevFormBeskrivelse COLUMN-LABEL "Leveringsform" FORMAT "X(30)":U
      KOrdreHode.EkstOrdreNr COLUMN-LABEL "Ekst.ordrenr" FORMAT "X(25)":U
      KOrdreHode.Totalt COLUMN-LABEL "Totalt" FORMAT "->>>,>>>,>>9.99":U
      KOrdreHode.ShipmentSendt COLUMN-LABEL "Shipment sendt" FORMAT "X(22)":U
      KOrdreHode.kordre_webcurrentstatus COLUMN-LABEL "Overf.status" FORMAT "X(40)":U
            WIDTH 15
      KOrdreHode.kordre_webSent COLUMN-LABEL "Overf.dato/tid" FORMAT "X(22)":U
      KOrdreHode.kordre_ReturFraBut COLUMN-LABEL "Retur fra butikk" FORMAT "X(25)":U
      KOrdreHode.AntApnet COLUMN-LABEL "Ant. pksdl" FORMAT ">9":U
            WIDTH 7.8
      KOrdreHode.AntPPEti COLUMN-LABEL "Ant.eti" FORMAT ">9":U
            WIDTH 7.2
      KOrdreHode.Leveringsdato COLUMN-LABEL "Leveringsdato" FORMAT "99/99/99":U
      KOrdreHode.ReturNr COLUMN-LABEL "Retur nr." FORMAT "x(20)":U
      KOrdreHode.SendingsNr COLUMN-LABEL "Sendingsnummer" FORMAT "X(15)":U
      KOrdreHode.kordre_cOpt1 COLUMN-LABEL "cOpt1" FORMAT "X(8)":U
      KOrdreHode.MobilTlf COLUMN-LABEL "Mobiltelefon" FORMAT "X(15)":U
      KOrdreHode.kordre_Butikk COLUMN-LABEL "Butikk" FORMAT "X(30)":U
      KOrdreHode.kordre_LevAdresse COLUMN-LABEL "LevAdresse" FORMAT "X(30)":U
      KOrdreHode.LevPostNr COLUMN-LABEL "Lev. PostNr" FORMAT "X(10)":U
      KOrdreHode.LevPostSted COLUMN-LABEL "Poststed" FORMAT "X(30)":U
      KOrdreHode.LevLand COLUMN-LABEL "Lev. Land" FORMAT "X(30)":U
      KOrdreHode.LevStatus COLUMN-LABEL "Lev.status" FORMAT "x(2)":U
      KOrdreHode.LevFNr COLUMN-LABEL "Leveringsform" FORMAT ">9":U
      KOrdreHode.ePostAdresse COLUMN-LABEL "E-Post" FORMAT "X(40)":U
      KOrdreHode.Opphav COLUMN-LABEL "Opphav" FORMAT ">9":U
      KOrdreHode.KundeNr COLUMN-LABEL "KundeNr" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.Faktura_Id COLUMN-LABEL "FakturaId" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.kordre_FakturaNr COLUMN-LABEL "FakturaNr" FORMAT "X(13)":U
      KOrdreHode.kordre_AntIkkeRet COLUMN-LABEL "AntIkkeRet" FORMAT ">>>9":U
      KOrdreHode.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      KOrdreHode.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
  ENABLE
      KOrdreHode.DatoTidOpprettet HELP "Dato og klokkeslett for opprettelse av ordre."
      KOrdreHode.Navn HELP "Navn eller firmanavn"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 163 BY 11.43 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.

DEFINE BROWSE BrwKOrdreLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreLinje C-Win _STRUCTURED
  QUERY BrwKOrdreLinje NO-LOCK DISPLAY
      KOrdreLinje.KOrdreLinjeNr COLUMN-LABEL "Linje" FORMAT ">>>>>>9":U
      KOrdreLinje.KopiKOrdreLinjeNr COLUMN-LABEL "Kopi" FORMAT ">>>>>>9":U
            WIDTH 7
      KOrdreLinje.ByttetKOrdreLinjeNr COLUMN-LABEL "Byttet" FORMAT ">>>>>>>9":U
            WIDTH 7
      KOrdreLinje.Aktiv COLUMN-LABEL "Akt" FORMAT "J/N":U
      KOrdreLinje.Returnert COLUMN-LABEL "Returnert" FORMAT "J/N":U
      KOrdreLinje.VareNr COLUMN-LABEL "VareNr" FORMAT "X(20)":U
      KOrdreLinje.Varetekst COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      KOrdreLinje.kordrelinje_LevKod COLUMN-LABEL "kordrelinje_LevKod" FORMAT "X(20)":U
      KOrdreLinje.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(15)":U
      KOrdreLinje.Storl COLUMN-LABEL "Str" FORMAT "x(10)":U
      KOrdreLinje.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      KOrdreLinje.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9":U
      KOrdreLinje.NettoPris COLUMN-LABEL "Nettopris" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Linjesum COLUMN-LABEL "Linjesum" FORMAT "->>>,>>9.99":U
      KOrdreLinje.kordlinje_returkode COLUMN-LABEL "kordlinje_returkode" FORMAT "X(25)":U
      KOrdreLinje.BetRef COLUMN-LABEL "Bet.ref." FORMAT "x(20)":U
      KOrdreLinje.Notat COLUMN-LABEL "Notat" FORMAT "X(40)":U
      KOrdreLinje.KOrdre_Id COLUMN-LABEL "FId" FORMAT ">>>>>>>>>>>>9":U
      KOrdreLinje.Pris COLUMN-LABEL "Pris" FORMAT "->>,>>9.99":U
      KOrdreLinje.PlukkButikk COLUMN-LABEL "Plukk but." FORMAT ">>>>>9":U
      KOrdreLinje.UtleverButikk COLUMN-LABEL "Utlev. but" FORMAT ">>>>>9":U
      KOrdreLinje.DatoTidOpprettet COLUMN-LABEL "DatoTidOpprettet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
  ENABLE
      KOrdreLinje.KOrdreLinjeNr HELP "Linjenummer på faktura"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 163 BY 6.19 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 18.48 COL 2 WIDGET-ID 32
     first_tbKOrdreHode AT ROW 1.33 COL 2 WIDGET-ID 10
     prev_tbKOrdreHode AT ROW 1.33 COL 6.8 WIDGET-ID 12
     next_tbKOrdreHode AT ROW 1.33 COL 11.4 WIDGET-ID 14
     last_tbKOrdreHode AT ROW 1.33 COL 16 WIDGET-ID 16
     delete_tbKOrdreHode AT ROW 1.33 COL 20.6 WIDGET-ID 48
     refresh_tbKOrdreHode AT ROW 1.33 COL 25.2 WIDGET-ID 6
     filter_tbKOrdreHode AT ROW 1.33 COL 29.8 WIDGET-ID 22
     excel_tbKOrdreHode AT ROW 1.33 COL 34.4 WIDGET-ID 8
     multiSortBrowse_tbKOrdreHode AT ROW 1.33 COL 39 WIDGET-ID 20
     browseconfig_tbKOrdreHode AT ROW 1.33 COL 43.6 WIDGET-ID 18
     fiTEXT AT ROW 3.14 COL 3 NO-LABEL
     ficSporingsnr AT ROW 3.14 COL 56.8 COLON-ALIGNED
     ficEkstOrdreNr AT ROW 3.14 COL 125.8 COLON-ALIGNED
     BtnBlank AT ROW 5.29 COL 149 WIDGET-ID 62
     fiEkstOrdreNr AT ROW 5.33 COL 71.8 COLON-ALIGNED
     fiDato AT ROW 5.33 COL 121 COLON-ALIGNED
     BrwKOrdreHode AT ROW 6.95 COL 2 WIDGET-ID 200
     first_tbKOrdreLinje AT ROW 19.05 COL 3.2 WIDGET-ID 40
     prev_tbKOrdreLinje AT ROW 19.05 COL 8 WIDGET-ID 42
     next_tbKOrdreLinje AT ROW 19.05 COL 12.6 WIDGET-ID 44
     last_tbKOrdreLinje AT ROW 19.05 COL 17.2 WIDGET-ID 46
     delete_tbKOrdreLinje AT ROW 19.05 COL 21.8 WIDGET-ID 50
     RegistrerLinjer_tbKOrdreLinje AT ROW 19.05 COL 26.4 WIDGET-ID 54
     BrwKOrdreLinje AT ROW 20.38 COL 2 WIDGET-ID 300
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 4.67 COL 55.6 WIDGET-ID 60
          FONT 6
     tbKOrdreHode AT ROW 1.24 COL 1 WIDGET-ID 2
     searchKOrdreHode AT ROW 5.91 COL 2 WIDGET-ID 26
     RECT-4 AT ROW 2.67 COL 2 WIDGET-ID 28
     tbKOrdreLinje AT ROW 18.95 COL 3 WIDGET-ID 38
     RECT-5 AT ROW 5.1 COL 54 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 165 BY 25.57 WIDGET-ID 100.


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
         TITLE              = "Retur av kundeordre"
         HEIGHT             = 25.62
         WIDTH              = 164.8
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
/* BROWSE-TAB BrwKOrdreHode fiDato DEFAULT-FRAME */
/* BROWSE-TAB BrwKOrdreLinje RegistrerLinjer_tbKOrdreLinje DEFAULT-FRAME */
ASSIGN 
       BrwKOrdreHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1
       BrwKOrdreHode:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR FILL-IN fiTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       tbKOrdreHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,delete;Slett,refresh;Refresh¤enable,filter;Filter,excel;Eksporter til E&xcel,multiSortBrowse;Sorter på flere kolonner,browseconfig;Column setupmaxborder".

ASSIGN 
       tbKOrdreLinje:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,delete;Slett,RegistrerLinjer;Registrer linjer¤enablemaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreHode
/* Query rebuild information for BROWSE BrwKOrdreHode
     _TblList          = "SkoTex.KOrdreHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.KOrdreHode.DatoTidOpprettet|no"
     _FldNameList[1]   > KOrdreHode.DatoTidOpprettet
"KOrdreHode.DatoTidOpprettet" "DatoTidOpprettet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? yes "Dato og klokkeslett for opprettelse av ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreHode.kordre_PDF" "PDF" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > KOrdreHode.BrukerID
"KOrdreHode.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > KOrdreHode.Navn
"KOrdreHode.Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "Navn eller firmanavn" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > KOrdreHode.KOrdre_Id
"KOrdreHode.KOrdre_Id" "Ordrenummer" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > KOrdreHode.RefKOrdre_Id
"KOrdreHode.RefKOrdre_Id" "KOId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Referanse til opprinnelig kundeordre." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreHode.kordre_LevStatus" "Leveringsstatus" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > KOrdreHode.LevFormBeskrivelse
"KOrdreHode.LevFormBeskrivelse" "Leveringsform" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Kort beskrivelse av leveringsform." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > KOrdreHode.EkstOrdreNr
"KOrdreHode.EkstOrdreNr" "Ekst.ordrenr" "X(25)" "CHARACTER" ? ? ? ? ? ? no "Ordrenummer fra eksternt system (Importert ordre)" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > KOrdreHode.Totalt
"KOrdreHode.Totalt" "Totalt" "->>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Totalt beløp på faktura" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreHode.ShipmentSendt" "Shipment sendt" "X(22)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreHode.kordre_webcurrentstatus" "Overf.status" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreHode.kordre_webSent" "Overf.dato/tid" "X(22)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreHode.kordre_ReturFraBut" "Retur fra butikk" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > KOrdreHode.AntApnet
"KOrdreHode.AntApnet" "Ant. pksdl" ">9" "INTEGER" ? ? ? ? ? ? no "Antall ganger ordren er åpnet etter første utlevering." no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > KOrdreHode.AntPPEti
"KOrdreHode.AntPPEti" "Ant.eti" ">9" "INTEGER" ? ? ? ? ? ? no "Antall postpakke etiketter utskrivet." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > KOrdreHode.Leveringsdato
"KOrdreHode.Leveringsdato" "Leveringsdato" "99/99/99" "DATE" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > KOrdreHode.ReturNr
"KOrdreHode.ReturNr" "Retur nr." "x(20)" "CHARACTER" ? ? ? ? ? ? no "Returnr for sporing. Påført returetikett." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > KOrdreHode.SendingsNr
"KOrdreHode.SendingsNr" "Sendingsnummer" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Sendingsnummer - for sporing." no no "16.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreHode.kordre_cOpt1" "cOpt1" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > KOrdreHode.MobilTlf
"KOrdreHode.MobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreHode.kordre_Butikk" "Butikk" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KOrdreHode.kordre_LevAdresse" "LevAdresse" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > KOrdreHode.LevPostNr
"KOrdreHode.LevPostNr" "Lev. PostNr" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Postnummer" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > KOrdreHode.LevPostSted
"KOrdreHode.LevPostSted" "Poststed" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Poststed" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > KOrdreHode.LevLand
"KOrdreHode.LevLand" "Lev. Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > KOrdreHode.LevStatus
"KOrdreHode.LevStatus" "Lev.status" "x(2)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > KOrdreHode.LevFNr
"KOrdreHode.LevFNr" "Leveringsform" ">9" "INTEGER" ? ? ? ? ? ? no "Leveringsvorm" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > KOrdreHode.ePostAdresse
"KOrdreHode.ePostAdresse" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > KOrdreHode.Opphav
"KOrdreHode.Opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Ordre opphav. F.eks. 1-Manuell reg. 2-Nettbutikk." no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > KOrdreHode.KundeNr
"KOrdreHode.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > KOrdreHode.Faktura_Id
"KOrdreHode.Faktura_Id" "FakturaId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kobling mellom faktura og kundeordre" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"KOrdreHode.kordre_FakturaNr" "FakturaNr" "X(13)" "CHARACTER" ? ? ? ? ? ? no "" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"KOrdreHode.kordre_AntIkkeRet" "AntIkkeRet" ">>>9" "INTEGER" ? ? ? ? ? ? no "" no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > KOrdreHode.ButikkNr
"KOrdreHode.ButikkNr" "Butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk som rekrutterte kunden" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > KOrdreHode.RegistrertDato
"KOrdreHode.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreLinje
/* Query rebuild information for BROWSE BrwKOrdreLinje
     _TblList          = "SkoTex.KOrdreLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KOrdreLinje.KOrdreLinjeNr" "Linje" ">>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Linjenummer på faktura" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreLinje.KopiKOrdreLinjeNr" "Kopi" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Linjkepeker som peker på rad kopiert til eller fra." no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreLinje.ByttetKOrdreLinjeNr" "Byttet" ">>>>>>>9" "INTEGER" ? ? ? ? ? ? no "Vare byttet på returordre på angitt varelinje." no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreLinje.Aktiv" "Akt" "J/N" "LOGICAL" ? ? ? ? ? ? no "Viser om raden er aktiv og skal tas med på faktura o.l." no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreLinje.Returnert" "Returnert" "J/N" "LOGICAL" ? ? ? ? ? ? no "Ordrelinje er returnert fra kunde." no no "8.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreLinje.VareNr" "VareNr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Varenummer" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreLinje.Varetekst" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Varetekst" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreLinje.kordrelinje_LevKod" "kordrelinje_LevKod" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreLinje.LevFargKod" "LevFargKod" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreLinje.Storl" "Str" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreLinje.Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreLinje.Antall" "Antall" "->>,>>9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreLinje.NettoPris" "Nettopris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Nettopris. Pris eksklusive mva og rabatter." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreLinje.Linjesum" "Linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KOrdreLinje.kordlinje_returkode" "kordlinje_returkode" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KOrdreLinje.BetRef" "Bet.ref." "x(20)" "CHARACTER" ? ? ? ? ? ? no "Betalingsreferanse på tilbakebetaling." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreLinje.Notat" "Notat" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Informasjon til kunde om varen." no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KOrdreLinje.KOrdre_Id" "FId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt ordre id. Kobler linjen til KOrdreHode." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KOrdreLinje.Pris" "Pris" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris (Til kunde)" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreLinje.PlukkButikk" "Plukk but." ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk som skal levere varen til nettbutikken." no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KOrdreLinje.UtleverButikk" "Utlev. but" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Utleveres fra butikk" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreLinje.DatoTidOpprettet" "DatoTidOpprettet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for opprettelse av ordrelinje." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreLinje */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Retur av kundeordre */
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
ON WINDOW-CLOSE OF C-Win /* Retur av kundeordre */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBlank C-Win
ON CHOOSE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank> */
DO:
  ASSIGN 
    fiEkstOrdreNr:SCREEN-VALUE = ''
    fiDato:SCREEN-VALUE = STRING(TODAY - 5)
    .
  RUN setFilter.
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


&Scoped-define SELF-NAME ficEkstOrdreNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficEkstOrdreNr C-Win
ON LEAVE OF ficEkstOrdreNr IN FRAME DEFAULT-FRAME /* Eksternt ordrenr */
DO:
    IF TRIM(ficEkstOrdreNr:SCREEN-VALUE) <> '' THEN 
      RUN getEkstOrdreNr(ficEkstOrdreNr:SCREEN-VALUE).  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficEkstOrdreNr C-Win
ON RETURN OF ficEkstOrdreNr IN FRAME DEFAULT-FRAME /* Eksternt ordrenr */
DO:
    IF TRIM(ficEkstOrdreNr:SCREEN-VALUE) <> '' THEN 
      RUN getEkstOrdreNr(ficEkstOrdreNr:SCREEN-VALUE).  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ficSporingsnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficSporingsnr C-Win
ON LEAVE OF ficSporingsnr IN FRAME DEFAULT-FRAME /* Sendingsnr */
DO:
    IF TRIM(ficSporingsnr:SCREEN-VALUE) <> '' THEN 
      RUN getKOrdreLinjer(ficSporingsnr:SCREEN-VALUE).
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ficSporingsnr C-Win
ON RETURN OF ficSporingsnr IN FRAME DEFAULT-FRAME /* Sendingsnr */
DO:
    IF TRIM(ficSporingsnr:SCREEN-VALUE) <> '' THEN 
      RUN getKOrdreLinjer(ficSporingsnr:SCREEN-VALUE).
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDato C-Win
ON RETURN OF fiDato IN FRAME DEFAULT-FRAME /* Registrert dato */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiDato C-Win
ON TAB OF fiDato IN FRAME DEFAULT-FRAME /* Registrert dato */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEkstOrdreNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEkstOrdreNr C-Win
ON RETURN OF fiEkstOrdreNr IN FRAME DEFAULT-FRAME /* Ekst.ordrenummer */
DO:
  RUN setfilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEkstOrdreNr C-Win
ON TAB OF fiEkstOrdreNr IN FRAME DEFAULT-FRAME /* Ekst.ordrenummer */
DO:
 RUN setFilter.  
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
ON CLOSE OF THIS-PROCEDURE 
  DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttVareRecord C-Win 
PROCEDURE ByttVareRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    
    IF AVAILABLE KOrdreHode AND CAN-DO('50,60',KOrdreHode.LevStatus) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Ordren er utlevert eller makulert og kan ikke endres.").
        RETURN NO-APPLY.
      END. 
    
    IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Endring kan bare gjøres på en rad ad gangen. Marker bare en rad.").
      RETURN.      
    END.

    IF AVAIL KOrdreLinje AND KOrdreLinje.VareNr = 'BETALT' THEN 
      RETURN.
    
    IF AVAILABLE KOrdreLinje AND (KOrdreLinje.ByttetKOrdreLinjeNr > 0) THEN
    DO:
      JBoxSession:Instance:ViewMessage("Vare kan ikke byttes på en rad hvor vare allerede er byttet.").
      RETURN.      
    END. 
    IF AVAILABLE KOrdreLinje AND KOrdreLinje.aktiv = FALSE THEN
    DO:
      JBoxSession:Instance:ViewMessage("Vare kan ikke byttes på en rad som ikke er aktiv.").
      RETURN.      
    END. 

    IF NOT AVAILABLE KOrdreLinje THEN 
      RETURN.
    CURRENT-WINDOW:SENSITIVE = FALSE.
    IF icModus = '20' THEN /* BYTTE */
      RUN LagerListeButArtStr.w ('16|27|' + STRING(KOrdreLinje.KOrdre_Id) + '|' + STRING(KOrdreLinje.KOrdreLinjeNr) + '|50|' + STRING(KOrdreLinje.Pris) + '||' + KOrdreLinje.Kode).
    ELSE /* RETUR */
      RUN LagerListeButArtStr.w ('16|25|' + STRING(KOrdreLinje.KOrdre_Id) + '|' + STRING(KOrdreLinje.KOrdreLinjeNr) + '|50|' + STRING(KOrdreLinje.Pris) + '||' + KOrdreLinje.Kode).
    CURRENT-WINDOW:SENSITIVE = TRUE.
/*    oBrwKOrdreLinje:refreshSelectedRows().*/
    oBrwKOrdreLinje:refresh().
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker rad som skal endres først.").
    RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
DO WITH FRAME {&FRAME-NAME}:
    /* Ny hode. */
    IF otbKOrdreHode:isCurrent THEN 
    DO:
      IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS <> 1 THEN
      DO:    
        JBoxSession:Instance:ViewMessage("Bare en ordre tillates slettet ad gangen. Marker ordre som skal slettes.").
        RETURN.      
      END.
      IF AVAILABLE KOrdreHode THEN 
      DO:
        IF CAN-DO('50,60',KOrdreHode.LevStatus) THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Bare retur ordre som ikke er utlevert kan slettes. ").
          RETURN.
        END.
        ELSE IF KOrdreHode.Opphav <> 10 THEN 
          DO: 
            JBoxSession:Instance:ViewMessage("Ordren er ikke opprettet fra eCom lager og kan ikke slettes. ").
            RETURN.
          END.
        /* Linjen på opprinnelig ordre må frigjøres. */
        IF NOT JBoxServerApi:Instance:CallServerProc("kordrlinje_frigjor_linje.p",
          STRING(KOrdreHode.KOrdre_Id) + '|0' 
          ) THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Feil ved sletting av ordre: " + JBoxServerAPI:Instance:getCallMessage()).
          RETURN.
        END.
      END.
    END.
    /* Slett rad. */
    ELSE IF otbKOrdreLinje:isCurrent THEN 
      DO:
        IF AVAILABLE KOrdreLinje AND AVAILABLE KORdrEHode THEN
        ORDRELINJE: 
        DO:
          IF CAN-DO('50,60',KOrdreHode.LevStatus) THEN
          DO: 
            JBoxSession:Instance:ViewMessage("Linjen kan ikke slettes på ordre med denne status.").
            RETURN.
          END.
          IF KOrdreHode.Opphav <> 10 THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Ordre er ikke opprettet fra eCom. Ordrelinjer kan ikke slettes. ").
            RETURN.
          END.
          
          /* Dette er den opprinnelige raden. Dens skal ikke røres når den er passiv. */
          IF KOrdreLinje.Aktiv = FALSE THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Bare aktive linjer kan slettes. Bruk 'Angre bytte' funksjonen for å slette denne linjen.").
            RETURN.
          END.
          ELSE IF KOrdreLinje.Aktiv AND (KOrdreLinje.KopiKOrdreLinjeNr > 0  OR KOrdreLinje.ByttetKOrdreLinjeNr > 0) THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Bruk 'Angre bytte' funksjonen for å slette denne linjen.").
            RETURN.
          END.

          /* Dette er en rad det er byttet vare på. Kopi rad. */
          /* Her skal bare kopien slettes og original raden aktiveres igjen */
          ELSE IF KOrdreLinje.Aktiv THEN
          SLETTLINJE: 
          DO:
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal varelinjen slettes?") THEN
                RETURN.
            /* Raden har ingen kopi og ved retur skal linjen den peker på på opprinnelig ordre frigjøres igjen. */
            IF KOrdreLinje.KopiKOrdreLinjeNr = 0 AND KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' THEN 
            DO:
              /* Linjen på opprinnelig ordre må frigjøres. */
              IF NOT JBoxServerApi:Instance:CallServerProc("kordrlinje_frigjor_linje.p",
                STRING(KOrdreHode.RefKOrdre_Id) + '|' + 
                STRING(KOrdreLinje.KOrdreLinjeNr)
                ) THEN 
              DO:
                JBoxSession:Instance:ViewMessage("Feil ved frigjøring av linje på opprinnelig ordre. Melding: " + JBoxServerAPI:Instance:getCallMessage()).
                RETURN.
              END.
            END.
            
            IF oBrwKOrdreLinje:processRowsNoMessage("kordrelinje_slettlinje.p", STRING(KOrdreHode.KOrdre_Id) + ',' + STRING(KOrdreLinje.KOrdreLinjeNr)) THEN
              oBrwKOrdreLinje:refresh().
            RETURN.            
          END. /* SLETTLINJE */
        END. /* ORDRELINJE */
      END.
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    /* Ny hode. */
    IF otbKOrdreHode:isCurrent THEN 
    DO:
    END.
    /* Ny rad. */
    ELSE IF otbKOrdreLinje:isCurrent THEN 
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
DO WITH FRAME {&FRAME-NAME}:
    IF oBrwKOrdreHode:isCurrent THEN
    DO: 
    END.
  END.
  
  RUN SUPER.
  
  IF oBrwKOrdreHode:isCurrent THEN  
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE KOrdreHode THEN
    DO:
      IF KOrdreHode.EkstOrdreNr MATCHES '*RETUR*' AND NOT CAN-DO('50,60',KOrdreHode.LevStatus) THEN 
        oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = ' Returordre under registrering.'.
      ELSE IF KOrdreHode.EkstOrdreNr MATCHES '*BYTTE*' AND NOT CAN-DO('50,60',KOrdreHode.LevStatus) THEN 
        oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = ' Bytteordre under registrering.'.
      
      ELSE IF (KOrdreHode.KOrdre_cOpt1  <> '' OR KOrdreHode.LevFNr = 8) THEN       
        oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = IF (KOrdreHode.LevFNr = 8 AND KOrdreHode.KOrdre_cOpt1  <> "") THEN
                                                       'Pakken er en gave og skal utleveres i butikk.' /* Rød */
                                               ELSE IF KOrdreHode.KOrdre_cOpt1  <> "" THEN
                                                       'Pakken skal være en gave.' /* GUL */
                                               ELSE IF KOrdreHode.LevFNr = 8 THEN
                                                       'Utlevering i butikk.'  /* Grøn */
                                               ELSE ''.
        ELSE 
            oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = ''.
    END. 
  END.

  IF AVAILABLE KOrdreHode THEN
  DO:
    IF CAN-DO('50,60',KOrdreHode.LevStatus) AND KORdreHode.Opphav = 10 THEN
      ASSIGN 
        otbKOrdreLinje:disabledTools = ""
        otbKOrdreHode:disabledTools  = "UtleverKreditNota"
        .
    ELSE IF KORdreHode.Opphav = 10 THEN
      ASSIGN 
        otbKOrdreHode:disabledTools = ""
        .
  END.
/*  IF AVAILABLE KOrdreLinje AND AVAILABLE KOrdreHode THEN          */
/*  DO:                                                             */
/*    IF KOrdreHode.LevStatus = '47' AND KORdreHode.Opphav = 10 THEN*/
/*      ASSIGN                                                      */
/*        otbKOrdreLinje:disabledTools = ""                         */
/*        .                                                         */
/*    ELSE                                                          */
/*      ASSIGN                                                      */
/*        otbKOrdreLinje:disabledTools = "Delete"                   */
/*        .                                                         */
/*  END.                                                            */
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
  DISPLAY fiTEXT ficSporingsnr ficEkstOrdreNr fiEkstOrdreNr fiDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY tbKOrdreHode searchKOrdreHode RECT-4 tbKOrdreLinje RECT-5 
         first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode 
         last_tbKOrdreHode delete_tbKOrdreHode refresh_tbKOrdreHode 
         filter_tbKOrdreHode excel_tbKOrdreHode multiSortBrowse_tbKOrdreHode 
         browseconfig_tbKOrdreHode ficSporingsnr ficEkstOrdreNr BtnBlank 
         fiEkstOrdreNr fiDato BrwKOrdreHode first_tbKOrdreLinje 
         prev_tbKOrdreLinje next_tbKOrdreLinje last_tbKOrdreLinje 
         delete_tbKOrdreLinje RegistrerLinjer_tbKOrdreLinje BrwKOrdreLinje 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getEkstOrdreNr C-Win 
PROCEDURE getEkstOrdreNr :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcEkstOrdreNr AS CHARACTER NO-UNDO.
    
  DEFINE VARIABLE pcLevStatus AS CHARACTER NO-UNDO.
  DEFINE VARIABLE plKOrdre_Id AS DECIMAL   NO-UNDO.
    
  IF TRIM(pcEkstOrdreNr) <> '' THEN
  BLOKKEN: 
  DO WITH FRAME DEFAULT-FRAME :
    ASSIGN 
      pcLevStatus = ''
      pcFeltVerdier = ''
      .
    
/* TN 14/12-19 Dette virker ikke i prod ???? */
/* TN 20/9-20 Ny test. Det virker nå. sjekket på server infopos05 hos Gant. */    
/*    IF JBoxServerAPI:Instance:Find("KOrdreHode", "WHERE EkstOrdreNr = '" + TRIM(pcEkstOrdreNr) +*/
/*      "' AND LevStatus = '50' AND NOT SendingsNr MATCHES ('*RETUR*')") THEN                     */
    /* TEST */
    IF JBoxServerAPI:Instance:Find("FIRST KOrdreHode WHERE EkstOrdreNr = '" + TRIM(pcEkstOrdreNr) +
      "' AND LevStatus = '50' AND NOT SendingsNr MATCHES ('*RETUR*')") THEN
    DO:
      plKOrdre_Id = DEC(JBoxServerAPI:Instance:FieldValue("KOrdreHode.KOrdre_Id")).
      pcLevStatus = JBoxServerAPI:Instance:FieldValue("KOrdreHode.Levstatus").
      RUN opprettReturOrdre (plKOrdre_Id, cEkstOrdreNr, pcLevStatus, OUTPUT bOk).
    END.
    ELSE JBoxSession:Instance:ViewMessage("Ukjent ordre.").
    /* TEST */

/*    /* WorkAround. */                                                                                                                                                  */
/*    pcFeltVerdier = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode", "WHERE EkstOrdreNr = '" +                                                                          */
/*                                                                    TRIM(pcEkstOrdreNr) +                                                                              */
/*                                                                    "' AND LevStatus = '50' AND NOT SendingsNr MATCHES ('*RETUR*')","KOrdre_Id,Levstatus,EkstOrdreNr").*/
/*    IF NUM-ENTRIES(pcFeltVerdier,'|') = 3 THEN                                                                                                                         */
/*    DO:                                                                                                                                                                */
/*      plKOrdre_Id = DEC(ENTRY(1,pcFeltVerdier,'|')).                                                                                                                   */
/*      pcLevStatus  = ENTRY(2,pcFeltVerdier,'|').                                                                                                                       */
/*      cEkstOrdreNr = ENTRY(3,pcFeltVerdier,'|').                                                                                                                       */
/*      RUN opprettReturOrdre (plKOrdre_Id, cEkstOrdreNr, pcLevStatus, OUTPUT bOk).                                                                                      */
/*    END.                                                                                                                                                               */
/*    ELSE JBoxSession:Instance:ViewMessage("Ukjent ordre.").                                                                                                            */
/*    /* WorkAround. */                                                                                                                                                  */

  END. /* BLOKKEN */
  ASSIGN 
    ficEkstOrdreNr                                     = ''
    ficEkstOrdreNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ''
    .
        
  APPLY 'ENTRY' TO ficEkstOrdreNr IN FRAME DEFAULT-FRAME.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKOrdreLinjer C-Win 
PROCEDURE getKOrdreLinjer :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcSporingsNr AS CHARACTER NO-UNDO.
    
  DEFINE VARIABLE pcLevStatus AS CHARACTER NO-UNDO.
  DEFINE VARIABLE plKOrdre_Id AS DECIMAL   NO-UNDO.
    
  IF TRIM(pcSporingsNr) <> '' THEN
  BLOKKEN: 
  DO WITH FRAME DEFAULT-FRAME :
    ASSIGN 
      pcLevStatus = ''
      pcFeltVerdier = ''
/*    IF JBoxServerAPI:Instance:Find("KOrdreHode",                                                                         */
/*      "WHERE SendingsNr = '" + TRIM(pcSporingsNr) + "' AND Levstatus = '50' AND NUM-ENTRIES(SendingsNr,' ') = '1' ") THEN*/

    /* WorkAround. */
    pcFeltVerdier = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode", "WHERE SendingsNr = '" + TRIM(pcSporingsNr) + "' AND Levstatus = '50' AND NUM-ENTRIES(SendingsNr,' ') = '1' ","KOrdre_Id,Levstatus,EkstOrdreNr").

    IF NUM-ENTRIES(pcFeltVerdier,'|') = 3 THEN 
    DO:
/*      plKOrdre_Id = DEC(JBoxServerAPI:Instance:FieldValue("KOrdreHode.KOrdre_Id")).*/
/*      pcLevStatus = JBoxServerAPI:Instance:FieldValue("KOrdreHode.Levstatus").     */

      plKOrdre_Id = DEC(ENTRY(1,pcFeltVerdier,'|')).
      pcLevStatus = (ENTRY(2,pcFeltVerdier,'|')).
      cEkstOrdreNr = ENTRY(3,pcFeltVerdier,'|').
      RUN opprettReturOrdre (plKOrdre_Id, cEkstOrdreNr, pcLevStatus, OUTPUT bOk).
    END.
    ELSE DO: 
      IF JBoxServerAPI:Instance:Find("KOrdreHode", 
        "WHERE SendingsNr = '" + TRIM(pcSporingsNr) + "' AND Levstatus < '50' AND NUM-ENTRIES(SendingsNr,' ') = '1' ") THEN
        JBoxSession:Instance:ViewMessage("Ordren er ikke utlevert ennå.").
      ELSE 
        JBoxSession:Instance:ViewMessage("Ukjent ordre. Eller ordren er ikke utlevert ennå.").
    END.
  END. /* BLOKKEN */
  ASSIGN 
    ficSporingsnr                                     = ''
    ficSporingsnr:SCREEN-VALUE IN FRAME DEFAULT-FRAME = ''
    .
        
  APPLY 'ENTRY' TO ficSporingsnr IN FRAME DEFAULT-FRAME.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:

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
    SUBSCRIBE TO 'opprettVarelinjer' ANYWHERE.
    
    rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.
    icModus = ENTRY(1,icParam,'|'). /* 10=Retur, 20=Bytte */
    
    oBrwKOrdreHode = NEW JBoxBrowse(brwKOrdreHode:HANDLE).
    otbKOrdreHode = NEW JBoxToolbar(tbKOrdreHode:HANDLE).
    otbKOrdreLinje = NEW JBoxToolbar(tbKOrdreLinje:HANDLE).
  
    CASE icParam:
      WHEN '10' THEN otbKOrdreHode:addToolGroup("SkrivPkSdl;Skriv pakkseddel,utleverKreditnota;Utlever RETUR ordre,skrivKreditNota;Utskrift av kreditnota").
      WHEN '20' THEN otbKOrdreHode:addToolGroup("SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,utleverKreditnota;Utlever BYTTE ordre,VisPPetikett;Vis postpakke etikett,sendEtiMail;Send epost til kunde,skrivKreditNota;Utskrift av kreditnota").
    END CASE.
    
    oBrwKOrdreHode:setSearchField(searchKOrdreHode:HANDLE,"EkstOrdreNr").
    oBrwKORdreHode:customDeleteValProc = "ignore". /* Fjerner vlaidering på om det ligger Ovbuffer poster under PkSdlHode. */
    CASE icModus:
      WHEN '10' THEN oBrwKOrdreHode:baseQuery = "WHERE LevStatus >= '10' AND LevStatus <= '50' AND SendingsNr = 'RETUR' AND Opphav = '10' ".
      WHEN '20' THEN oBrwKOrdreHode:baseQuery = "WHERE LevStatus >= '10' AND LevStatus <= '50' AND EkstOrdreNr matches '*BYTTE*' AND Opphav = '10' ".
    END CASE.
    oBrwKOrdreHode:calcFieldProc = "kordre_brwcalc.p".
    oBrwKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.
    oBrwKOrdreHode:setSearchField(searchKOrdreHode:HANDLE,"EkstOrdreNr").
    oBrwKOrdreHode:setQuerySort('KOrdre_Id;DESC').
    hKOrdre_IdColumn = oBrwKOrdreHode:getColumnHandle("KOrdre_Id").
  
    IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20") THEN
      cNettButikkType = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1").
  
    RUN InitializeComponents.
  
    oBrwKOrdreLinje = NEW JBoxBrowse(brwKOrdreLinje:HANDLE).
    oBrwKOrdreLinje:setParentBrowseObject(oBrwKOrdreHode,"KOrdre_Id").
    oBrwKOrdreLinje:TOOLBAR-OBJECT = otbKOrdreLinje.
    hLinjeAktivColumn = oBrwKOrdreLinje:getColumnHandle("Aktiv").
  
    /* Varebytte skal bare kunne gjøres på bytte ordre, ikke på retur ordre. */
    IF icModus = '20' THEN 
    DO:
      opopupVare = NEW JBoxPopupMenu().
      opopupVare:AddToolGroup('ByttVare;Bytt vare/størrelse,SlettKopi;Angre bytte,OppslagModell;Vis i modell liste').
      oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
    END.
    ELSE DO:
      opopupVare = NEW JBoxPopupMenu().
      opopupVare:AddToolGroup('OppslagModell;Vis i modell liste').
      oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
    END.

    oContainer:setSplitBarY(btnSplitBarY:HANDLE).
    oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,100). /* 200 pixels from the top, 300 pixels from the bottom */
    oContainer:setFollowSplitBarY(STRING(BrwKOrdreHode:HANDLE) + ',' + 
      STRING(BrwKOrdreLinje:HANDLE) + ',' +
      STRING(first_tbKOrdreLinje:HANDLE) + ',' +
      STRING(prev_tbKOrdreLinje:HANDLE) + ',' +
      STRING(next_tbKOrdreLinje:HANDLE) + ',' +
      STRING(last_tbKOrdreLinje:HANDLE) + ',' +
      STRING(delete_tbKOrdreLinje:HANDLE) + ',' +
      STRING(RegistrerLinjer_tbKOrdreLinje:HANDLE) 
      ).
    oContainer:setNoResizeY("BrwKOrdreHode,RECT-4,RECT-5").
    
    
    ASSIGN 
      fiDato:SCREEN-VALUE = STRING(TODAY - 5)
      .

    CASE icModus:
      WHEN '10' THEN ASSIGN fiTEXT:SCREEN-VALUE = 'RETUR av varer' fiTEXT:BGCOLOR = 14.
      WHEN '20' THEN ASSIGN fiTEXT:SCREEN-VALUE = 'BYTTE av varer' fiTEXT:BGCOLOR = 10.
    END CASE.
    
    RUN setFilter.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE multiSortBrowseRecord C-Win 
PROCEDURE multiSortBrowseRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mySelectorObject C-Win 
PROCEDURE mySelectorObject :
DEF INPUT PARAM ioSelector AS JBoxDynSelector NO-UNDO.

  ioSelector:TOOLBAR-OBJECT:addTool("Filter").
  ioSelector:ExpandSelectorDialogX(700).
  ioSelector:SOURCE-BROWSE-OBJECT:openQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettReturOrdre C-Win 
PROCEDURE opprettReturOrdre :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER plKOrdre_Id AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER pcEkstOrdreNr AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcLevStatus AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER obOk AS LOG NO-UNDO.
  
  DEFINE VARIABLE iAntRader AS INTEGER NO-UNDO.
  
/*    /* Det finnes en åpen retur ordre. */                                                             */
/*  IF (JBoxServerAPI:Instance:Find("FIRST KOrdreHode", "WHERE RefKORdre_Id = '" + STRING(plKOrdre_Id) +*/
/*    "' AND LevStatus = '47' AND SendingsNr MATCHES ('*RETUR*')")) THEN                                */
  /* WorkAround. */
  pcFeltVerdier = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode", "WHERE  RefKORdre_Id = '" + 
                                                                  STRING(plKOrdre_Id) + 
                                                                  "' AND LevStatus = '47' AND SendingsNr MATCHES ('*RETUR*')","KOrdre_Id,Levstatus,EkstOrdreNr").
  IF NUM-ENTRIES(pcFeltVerdier,'|') = 3 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Det finnes en åpen retur på denne ordren fra før. Ny bytte eller returordre kan derfor ikke ikke opprettes før den er avsluttet.").
    obOk = FALSE.
    RETURN.
  END.


/*  IF /* Det finnes en åpen bytte ordre. */                                                               */
/*    (JBoxServerAPI:Instance:Find("FIRST KOrdreHode", "WHERE EkstOrdreNr = '" + pcEkstOrdreNr + ' BYTTE' +*/
/*    "' AND LevStatus = '47'")) THEN                                                                      */
  /* WorkAround. */
  pcFeltVerdier = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode", "WHERE  EkstOrdreNr = '" + 
                                                                  pcEkstOrdreNr + 
                                                                  "' AND LevStatus = '47'","KOrdre_Id,Levstatus,EkstOrdreNr").
  IF NUM-ENTRIES(pcFeltVerdier,'|') = 3 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Det finnes et åpent varebytte på denne ordren fra før. Ny bytte eller returordre kan derfor ikke ikke opprettes før den er avsluttet.").
    obOk = FALSE.
    RETURN.
  END.
  
  /* Ved retur returneres antall rader som fortsatt kan returneres. Ved bytte, returneres antall aktive ordrerader. */
  IF NOT JBoxServerApi:Instance:CallServerProc("kordrelinje_retur_sjekk.p",STRING(plKOrdre_Id) + '|' + icModus) THEN
  DO: 
    JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
    RETURN.
  END.
  ELSE 
    iAntRader = INT(JBoxServerApi:Instance:getCallReturnParam()).

  IF NOT CAN-DO('50',pcLevStatus) OR iAntRader = 0 THEN 
  DO:
    IF pcLevStatus = '50' AND iantRader = 0 AND icModus <> '20' THEN 
      JBoxSession:Instance:ViewMessage("Alle ordrens linjer er returnert tidligere. Retur avbrutt.").              
    IF pcLevStatus = '50' AND iantRader > 0 AND icModus = '20' THEN 
      . /* Gjør ingenting. Ordren kan utleveres. */              
    ELSE IF pcLevStatus = '' THEN 
        JBoxSession:Instance:ViewMessage("Ukjent ordre.").
    ELSE 
      JBoxSession:Instance:ViewMessage("Ordren er ikke utlevert. Kan ikke byttes eller returneres.").
    obOk = FALSE.
    RETURN. 
  END.
  ELSE 
  DO:         
    /* TN 19/8-19 Denne seksjonen er koblet ut fordi det kreves at de må skanne inn linjene. Den virker. Legger opp en selector for å kunne velge inn linjer.                        */
    /*            cRowIdList = ''.                                                                                                                                                           */
    /*            JBoxServerAPI:Instance:Selector("KOrdreLinje"                                                                                                                              */
    /*                                  +  ";KOrdreLinjeNr"                                                                                                                                  */
    /*                                  +  ";Varetekst"                                                                                                                                      */
    /*                                  +  ";LevFargKod"                                                                                                                                     */
    /*                                  +  ";Storl"                                                                                                                                          */
    /*                                  +  ";Antall"                                                                                                                                         */
    /*                                  +  ";ReturKodeId"                                                                                                                                    */
    /*                                  +  ";KOrdre_Id"                                                                                                                                      */
    /*                                  +  ";Returnert"                                                                                                                                      */
    /*                                  + ",ArtBas"                                                                                                                                          */
    /*                                  +  ";Artikkelnr"                                                                                                                                     */
    /*                                  +  ";LevKod@2"                                                                                                                                       */
    /*                                  ,"where KOrdre_Id = '" + TRIM(STRING(plKOrdre_Id)) + "' AND Returnert = 'FALSE' AND Aktiv = TRUE,first ArtBas where ArtBas.ArtikkelNr = DEC(VareNr)",*/
    /*                                  cRowIdList).                                                                                                                                         */
    /*            IF JBoxServerAPI:Instance:SelectorOk AND JBoxServerAPI:Instance:SelectorRowidList > '' THEN                                                                                */
    /*            DO:                                                                                                                                                                        */
    /*                IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal de valgte ordrelinjer returneres?") THEN                                                                        */
    /*                    LEAVE BLOKKEN.                                                                                                                                                     */
    /*                IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_retur.p", TRIM(ficSporingsnr:SCREEN-VALUE) + ",ROWID" +  "," + JBoxServerAPI:Instance:SelectorRowidList) THEN   */
    /*                    JBoxSession:Instance:ViewMessage("Feil ved retur pga. " + JBoxServerAPI:Instance:getCallMessage()).                                                                */
    /*                                                                                                                                                                                       */
    /*                oBrwKOrdreHode:OpenQuery().                                                                                                                                            */
    /*                PUBLISH 'OrdrebehandlingOpenQuery'.                                                                                                                                    */
    /*                                                                                                                                                                                       */
    /*            END.                                                                                                                                                                       */
    IF NOT JBoxServerApi:Instance:CallServerProc("kordrehode_opprett_returordre.p",STRING(plKOrdre_Id) + '|' + JBoxSession:Instance:UserId + '|' + icModus) THEN
    DO: 
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
      RETURN.
    END.
    ELSE     
      plKOrdre_Id = DEC(JBoxServerApi:Instance:getCallReturnParam()).
    IF plKOrdre_Id > 0 THEN 
      oBrwKOrdreHode:OpenQuery().
    ELSE 
      obOk = FALSE.
  END.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettVarelinjer C-Win 
PROCEDURE opprettVarelinjer :
/*------------------------------------------------------------------------------
     Purpose: Variabel lKOrdre_Id settes når oppslag mot ordrelinjer startes.
              Etter oppslaget opprettes varellinjen her. 
              NB: Da er oppslagsrutinen fortsatt synlig og flere linjer kan 
              legges inn.
     Notes:
    ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcKode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER piReturKodeId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER plRefKOrdre_Id AS DECIMAL NO-UNDO.
  
  DEFINE VARIABLE piKOrdreLinjeNr AS INTEGER NO-UNDO.

/*  /* Henter nytt ledig linjenummer på ordren det registreres på. */    */
/*  IF JBoxServerApi:Instance:CallServerProc("kordrelinje_NyttLinjeNr.p",*/
/*    STRING(lKOrdre_Id)                                                 */
/*    ) THEN                                                             */
/*    piKOrdreLinjeNr = INT(JBoxServerApi:Instance:getCallReturnParam()).*/
  
  IF NOT JBoxServerApi:Instance:CallServerProc("kordrelinje_NyFraStrekkode.p",  
    STRING(lKOrdre_Id) + "|" + 
    pcKode + '|' + 
    STRING(piReturKodeId)+ '|' + 
    icModus 
    ) THEN 
  DO:
    IF NUM-ENTRIES(JBoxServerAPI:Instance:getCallMessage(),':') > 1 THEN 
    DO:  
      cTekst = ENTRY(2,JBoxServerAPI:Instance:getCallMessage(),':') + CHR(10) + 'Skal varen alikevel legges inn på bytte ordren?'.
      IF NOT JBoxSession:Instance:ViewQuestionOkCancel(cTekst) THEN
          RETURN.
      ELSE DO:
        JBoxServerApi:Instance:CallServerProc("kordrelinje_NyLinje.p",  
          STRING(lKOrdre_Id) + "|" + 
          pcKode + '|' + 
          STRING(piReturKodeId)+ '|' + 
          icModus 
          ). 
      END.
    END.
    ELSE DO:
      JBoxSession:Instance:ViewMessage(JBoxServerAPI:Instance:getCallMessage()).
      RETURN.
    END.
  END.

/*  IF NUM-ENTRIES(JBoxServerApi:Instance:getCallReturnParam(),'|') = 2 THEN                                */
/*  DO:                                                                                                     */
/*    ASSIGN                                                                                                */
/*      piKOrdreLinjeNr = INT(ENTRY(2,JBoxServerApi:Instance:getCallReturnParam(),'|'))                     */
/*      .                                                                                                   */
/*    FIND FIRST v_KOrdrelinje NO-LOCK WHERE                                                                */
/*      v_KOrdreLinje.KOrdre_Id = lKOrdre_Id AND                                                            */
/*      v_KOrdreLinje.KOrdreLinjeNr = piKOrdreLinjeNr NO-ERROR.                                             */
/*    IF AVAILABLE v_KOrdreLinje THEN                                                                       */
/*    DO:                                                                                                   */
/*      oBrwKOrdreLinje:BROWSE-HANDLE:QUERY:REPOSITION-TO-ROWID(ROWID(v_KOrdreLinje)).                      */
/*      oBrwKOrdreLinje:refreshRow(). /* for å friske opp raden. */                                         */
/*      oBrwKOrdreLinje:displayRecord(). /* For å friske opp update feltet hvis dette er aktivt på raden. */*/
/*    END.                                                                                                  */
/*  END.                                                                                                    */
/*  ELSE                                                                                                    */
  oBrwKOrdreLinje:openQuery().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppslagModellRecord C-Win
PROCEDURE OppslagModellRecord:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE KOrdreLinje THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',KordreLinje.kordrelinje_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',KordreLinje.LevFargKod)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegistrerLinjerRecord C-Win 
PROCEDURE RegistrerLinjerRecord :
DO WITH FRAME {&FRAME-NAME}:
  
  IF AVAILABLE KOrdreHode THEN 
  DO:
    IF CAN-DO('50,60',KOrdreHode.LevStatus) THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Retur/bytte -ordren er utlevert eller makulert. Nye linjer kan ikke registreres.").
        RETURN NO-APPLY.
      END. 
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
    ASSIGN 
      lKOrdre_Id = KOrdreHode.KOrdre_Id
      .
    RUN registrerreturlinjer.w (KOrdreHode.RefKOrdre_Id, KOrdreHode.KOrdre_Id, icModus).
    THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DEFINE VARIABLE piLevFnr AS INTEGER NO-UNDO.
    
  RUN SUPER.
    
  IF AVAILABLE KOrdreHode THEN 
  DO:
    piLevFnr = INT(DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(KOrdreHode.KOrdre_Id),"LevFnr")). 
        
    IF NOT CAN-DO('50,60',KORdreHode.LevStatus) AND (KOrdreHode.EkstORdreNr MATCHES '*RETUR*' OR KOrdreHode.EkstORdreNr MATCHES '*BYTTE*') THEN       
      hKOrdre_IdColumn:BGCOLOR = 13.
        
    ELSE IF KOrdreHode.KOrdre_cOpt1  <> '' OR KOrdreHode.LevFNr = 8 THEN 
    DO: 
      hKOrdre_IdColumn:BGCOLOR = IF KOrdreHode.KOrdre_cOpt1  = "" THEN 10 /* Grønn */ 
                                 ELSE IF KOrdreHode.LevFNr <> 8 THEN 14  /* Gul */
                                 ELSE 12. /* Rød */
    END.
  END. 
  
  IF AVAILABLE KOrdreLinje THEN 
  DO:
    IF KOrdreLinje.Aktiv = TRUE AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
      hLinjeAktivColumn:BGCOLOR = 10.
    ELSE IF KOrdreLinje.Aktiv = FALSE AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
        hLinjeAktivColumn:BGCOLOR = 13.

    IF KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND KOrdreLinje.Antall > 0 THEN 
      hLinjeAktivColumn:BGCOLOR = 14.
    ELSE IF KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND KOrdreLinje.Antall < 0 THEN 
        hLinjeAktivColumn:BGCOLOR = 12.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEtiMailRecord C-Win 
PROCEDURE sendEtiMailRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcEmne AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcePostAdresse AS CHARACTER NO-UNDO.

  IF AVAILABLE KOrdreHode THEN 
  DO:
    IF KOrdreHode.kordre_PDF = FALSE THEN 
      DO:
        JBoxSession:Instance:ViewMessage("PDF fil med etikett er ikke lagt inn på ordren.").
        RETURN. 
      END.
      ELSE DO:
        pcePostAdresse = KOrdreHode.ePostAdresse.
        RUN KOrdreEtikettSendMail.w (KOrdreHode.KOrdre_Id,
                                     KOrdreHode.EkstOrdreNr,
                                     KOrdreHode.KundeNr,
                                     KOrdreHode.Navn,
                                     INPUT-OUTPUT pcePostAdresse,
                                     OUTPUT pcEmne,
                                     OUTPUT pcTekst,
                                     OUTPUT bOk).
        IF bOk = FALSE THEN 
          RETURN. 
        IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_sendEtiMail.p",
                                                         STRING(KOrdreHode.KOrdre_Id) + '|' +
                                                         REPLACE(pcEmne,'|','') + '|' +
                                                         REPLACE(pcTekst,'|','') + '|' +
                                                         pcePostAdresse,
                                                         ?) THEN
          JBoxSession:Instance:ViewMessage("epost med etikett og pakkseddel som vedlegg er sendt til kunde " + pcePostAdresse + ".").
        ELSE 
          JBoxSession:Instance:ViewMessage("Fant ikke PDF fil. Mail ikke sendt.").
      END.
  END.

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

    IF fiEkstOrdreNr:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'EkstOrdreNr'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiEkstOrdreNr:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .

    IF (fiDato:SCREEN-VALUE <> '' AND fiDato:SCREEN-VALUE <> ?) THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'RegistrertDato'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + STRING(DATE(fiDato:SCREEN-VALUE))
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + '>='
        .
    
    oBrwKOrdreHode:setFilter(cFields,cOperator,cWhere).
    oBrwKOrdreHode:openQuery().
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivKreditNotaRecord C-Win 
PROCEDURE SkrivKreditNotaRecord :
IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Kreditnota kan bare skrives ut for en ordre ad gangen.").
    RETURN.
  END.
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:
    IF NOT AVAILABLE KOrdreHode OR KOrdreHode.Faktura_Id = 0 OR KOrdreHode.LevStatus = '47' THEN
    DO:
      JBoxSession:Instance:ViewMessage("Ordre ikke tilgjengelig eller kreditnota er ikke produsert for ordren.").
      RETURN.
    END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv kreditnota for valgt ordre?") THEN
      RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_skrivfaktura.p", "") THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
  END.
  ELSE 
  DO:
    JBoxSession:Instance:ViewMessage("Marker ordre det skal skrives ut kreditnota for.").
    RETURN.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPkSdlRecord C-Win 
PROCEDURE SkrivPkSdlRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv pakkseddel for valgt(e) post(er)?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_pakkseddel.p", JBoxSession:Instance:UserId) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Marker de ordrene som skal skrives ut før utskrift aktiveres.").
    RETURN. 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPPetikettRecord C-Win 
PROCEDURE SkrivPPetikettRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv postpakke etikett for valgt(e) post(er)?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_postpakke.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Marker de ordrene som det skal skrives ut etiketter for før utskrift aktiveres.").
    RETURN. 
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettKopiRecord C-Win 
PROCEDURE SlettKopiRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE piKopiKOrdreLinjeNr AS INTEGER NO-UNDO.
  
  IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Sletting kan bare gjøres på en rad ad gangen. Marker bare en rad.").
      RETURN.       
    END.
    IF AVAILABLE KOrdreLinje AND AVAILABLE KOrdreHode THEN 
    DO:
      IF KOrdreLinje.Aktiv = FALSE OR KOrdreLinje.ByttetKOrdreLinjeNr = 0 THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Det er ikke byttet varer på denne raden.").
        RETURN.
      END.
      
      /* Dette er varelinjer hvor vare ble byttet for ordren ble utlevert. */
      IF KOrdreLinje.Aktiv AND KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND  KOrdreLinje.ByttetKOrdreLinjeNr <= 99 AND KOrdreLinje.KOrdreLinjeNr <= 99 THEN 
      DO:
        /* Linjenr. på den nye linjen er nå alltid større enn linjenr på den opprinnelige linjen. */
        IF KOrdreLinje.KOrdreLinjeNr < KOrdreLinje.ByttetKOrdreLinjeNr THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Angre varebytte må gjøres på den nye raden.").
          RETURN.
        END.
        ELSE DO:
          IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Angre varebytte?") THEN
              RETURN.
          IF oBrwKOrdreLinje:processRowsNoMessage("kordrelinje_angrebytte.p", STRING(KOrdreHode.KOrdre_Id) + ',' + STRING(KOrdreLinje.KOrdreLinjeNr)) THEN     
            oBrwKOrdreLinje:refresh().
        END.
      END.
      
      /* Dette er varelinjer hvor det ikke er byttet vare for utlevering av ordre. */
      ELSE IF KOrdreLinje.Aktiv AND KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND KOrdreLinje.ByttetKOrdreLinjeNr > 99 AND KOrdreLinje.KOrdreLinjeNr <= 99 THEN 
      DO:
        IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Angre varebytte?") THEN
            RETURN.
        IF oBrwKOrdreLinje:processRowsNoMessage("kordrelinje_angrebytte.p", STRING(KOrdreHode.KOrdre_Id) + ',' + STRING(KOrdreLinje.KOrdreLinjeNr)) THEN     
          oBrwKOrdreHode:refresh().
      END.
      ELSE DO:
        JBoxSession:Instance:ViewMessage("Det er ikke byttet vare på denne varelinjen.").
        RETURN.      
      END.
    END.
   
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker rad som skal slettes først.").
    RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtleverKreditNotaRecord C-Win 
PROCEDURE UtleverKreditNotaRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbKOrdreHode:isCurrent THEN 
    DO:
      IF AVAILABLE KOrdreHode AND NOT CAN-DO('50,60',KOrdreHode.LevStatus) THEN 
      DO: 
        IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
        DO:    
          /* Varebytte */
          IF icModus = '20' THEN /* Varebytte */
          DO: 
            IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_sjekksum.p", "") THEN
            DO:
              /* Ordrens sum skal være 0. */
              JBoxSession:Instance:ViewMessage(JBoxServerAPI:Instance:getCallMessage()).
              RETURN.
            END.  
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett bytteordre for valgt(e) post(er) som levert til kunde?") THEN 
              RETURN.
          END.
          
          /* RETURN */
          ELSE DO: /* Retur */ 
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett kreditordre for valgt(e) post(er) som levert til kunde?") THEN 
              RETURN.
          END.
          
          IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_Lever.p", "") THEN
          DO:
            JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
            RETURN.
          END.  
        END.
        ELSE 
        DO: 
          JBoxSession:Instance:ViewMessage("Marker ordre som skal innleveres - bare en ordre kan innleveres ad gangen?"). 
          RETURN.
        END.
        oBrwKOrdreHode:refresh().
        oBrwKOrdreLinje:openQuery().
        /*          RUN DisplayRecord.*/
        PUBLISH 'OrdrebehandlingOpenQuery'.
      END.
    END.
    IF otbKOrdreLinje:isCurrent THEN 
    DO:
    END.    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPPetikettRecord C-Win 
PROCEDURE VisPPetikettRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.

  hFil = BUFFER ttKOrdrePostPakke:HANDLE.
  
  IF AVAILABLE KOrdreHode THEN 
  DO:
    IF KOrdreHode.kordre_PDF = FALSE THEN 
      DO:
        JBoxSession:Instance:ViewMessage("PDF fil med etikett er ikke lagt inn på ordren.").
        RETURN. 
      END.
      ELSE DO:
        CREATE ttKOrdrePostPakke. /* Bare for å ha en record å sende med. */
        
        IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_getBlob.p",STRING(KOrdreHode.KOrdre_Id),hFil) THEN
        DO: 
          hFil = JBoxServerAPI:Instance:getCallReturnTable().
          pcFilNavn = REPLACE(rclStandardFunksjoner:getTmpFileName (),'.tmp','.pdf').
          COPY-LOB FROM ttKOrdrePostPakke.PdfFil TO FILE pcFilNavn NO-ERROR. 
          IF SEARCH(pcFilNavn) <> ? THEN  
/*            RUN browse2pdf\viewxmldialog.w (pcFilNavn,"Rapport").*/
            OS-COMMAND SILENT VALUE('foxitreader.exe ' + pcFilNavn).
        END.
        ELSE 
          JBoxSession:Instance:ViewMessage("Fant ikke PDF.").
      END.
  END.
  
  EMPTY TEMP-TABLE ttKOrdrePostPakke.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

