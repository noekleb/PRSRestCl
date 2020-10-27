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

DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE hKordre_IdColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hEkstOrdreNrColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hAntallColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hLinjeAktivColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hNettoPrisColumn AS HANDLE NO-UNDO.
DEF VAR hTempTable AS HANDLE NO-UNDO.
DEFINE VARIABLE hFil AS HANDLE NO-UNDO.
DEF VAR opopupVare AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE hWebSide AS HANDLE NO-UNDO.

DEF VAR httKOrdreLinjeBuffer AS HANDLE NO-UNDO.
{ttKOrdre.i}
httKOrdreLinjeBuffer = BUFFER ttKOrdreLinje:HANDLE.

DEFINE TEMP-TABLE ttKOrdrePostPakke NO-UNDO
  FIELD KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD TYPE AS INTEGER 
  FIELD SeqNr AS INTEGER 
  FIELD PdfFil AS BLOB 
  .

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreHode ***/
DEFINE VARIABLE oBrwKOrdreHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreHode
    FIELD kordre_Navn AS CHARACTER
    FIELD KOrdre_Id AS DECIMAL
    FIELD kordre_cOpt1 AS CHARACTER
    FIELD kordre_PDF AS LOGICAL
    FIELD kordre_LevStatus AS CHARACTER
    FIELD kordre_LevFTekst AS CHARACTER
    FIELD EkstOrdreNr AS CHARACTER
    FIELD Totalt AS DECIMAL
    FIELD ShipmentSendt AS CHARACTER
    FIELD kordre_webcurrentstatus AS CHARACTER
    FIELD kordre_webSent AS CHARACTER
    FIELD WebBatchNr AS DECIMAL
    FIELD WebKode AS CHARACTER
    FIELD SendingsNr AS CHARACTER
    FIELD ReturNr AS CHARACTER
    FIELD Leveringsdato AS DATE
    FIELD AntApnet AS INTEGER
    FIELD AntPPEti AS INTEGER
    FIELD kordre_ReturFraBut AS CHARACTER
    FIELD VerkstedMerknad AS CHARACTER
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
    FIELD Kundeservice AS LOGICAL
    FIELD DatoTidEndret AS DATETIME
    FIELD DatoTidOpprettet AS DATETIME
    FIELD ButikkNr AS INTEGER
    FIELD RefKOrdre_Id AS DECIMAL
    FIELD Navn AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .

FUNCTION getBuffersAndFieldsBrwKOrdreHode RETURNS CHARACTER():
  RETURN
    'KOrdreHode'
     + ';KOrdre_Id'
     + ';EkstOrdreNr'
     + ';Totalt'
     + ';WebBatchNr'
     + ';WebKode'
     + ';SendingsNr'
     + ';ReturNr'
     + ';Leveringsdato'
     + ';AntApnet'
     + ';AntPPEti'
     + ';VerkstedMerknad'
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
     + ';Kundeservice'
     + ';DatoTidEndret'
     + ';DatoTidOpprettet'
     + ';ButikkNr'
     + ';RefKOrdre_Id'
     + ';Navn'
     + ';RegistrertDato'
     + ';+kordre_Navn|CHARACTER||kordre_Navn|Navn'
     + ';+kordre_cOpt1|CHARACTER||kordre_cOpt1|Gave'
     + ';+kordre_PDF|LOGICAL||kordre_PDF|PDF'
     + ';+kordre_LevStatus|CHARACTER||kordre_LevStatus|Leveringsstatus'
     + ';+kordre_LevFTekst|CHARACTER||kordre_LevFTekst|Leveringsform'
     + ';+ShipmentSendt|CHARACTER||kordre_ShipmentSendt|Shipment sendt'
     + ';+kordre_webcurrentstatus|CHARACTER||kordre_webcurrentstatus|Overf.status'
     + ';+kordre_webSent|CHARACTER||kordre_webSent|Overf.dato/tid'
     + ';+kordre_ReturFraBut|CHARACTER||kordre_ReturFraBut|Retur fra butikk'
     + ';+kordre_Butikk|CHARACTER||kordre_Butikk|Butikk'
     + ';+kordre_LevAdresse|CHARACTER||kordre_LevAdresse|LevAdresse'
     + ';+kordre_FakturaNr|CHARACTER||kordre_FakturaNr|FakturaNr'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreHode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreHode RETURNS CHARACTER():
  RETURN 
     'server/kordre_brwcalc.p' /* kordre_Navn */
   + ',kordre_brwcalc.p' /* kordre_cOpt1 */
   + ',kordre_brwcalc.p' /* kordre_PDF */
   + ',kordre_brwcalc.p' /* kordre_LevStatus */
   + ',kordre_brwcalc.p' /* kordre_LevFTekst */
   + ',kordre_brwcalc.p' /* kordre_ShipmentSendt */
   + ',kordre_brwcalc.p' /* kordre_webcurrentstatus */
   + ',kordre_brwcalc.p' /* kordre_webSent */
   + ',kordre_brwcalc.p' /* kordre_ReturFraBut */
   + ',kordre_brwcalc.p' /* kordre_Butikk */
   + ',kordre_brwcalc.p' /* kordre_LevAdresse */
   + ',kordre_brwcalc.p' /* kordre_FakturaNr */
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
    FIELD Manko AS LOGICAL
    FIELD VareNr AS CHARACTER
    FIELD Varetekst AS CHARACTER
    FIELD kordrelinje_LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD Storl AS CHARACTER
    FIELD Kode AS CHARACTER
    FIELD Antall AS DECIMAL
    FIELD NettoPris AS DECIMAL
    FIELD Pris AS DECIMAL
    FIELD MvaKr AS DECIMAL
    FIELD Linjesum AS DECIMAL
    FIELD OrgLinjesum AS DECIMAL
    FIELD BetRef AS CHARACTER
    FIELD Notat AS CHARACTER
    FIELD UtleverButikk AS INTEGER
    FIELD PlukkButikk AS INTEGER
    FIELD kordlinje_returkode AS CHARACTER
    FIELD ReturKodeId AS INTEGER
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
     + ';Manko'
     + ';VareNr'
     + ';Varetekst'
     + ';LevFargKod'
     + ';Storl'
     + ';Kode'
     + ';Antall'
     + ';NettoPris'
     + ';Pris'
     + ';MvaKr'
     + ';Linjesum'
     + ';OrgLinjesum'
     + ';BetRef'
     + ';Notat'
     + ';UtleverButikk'
     + ';PlukkButikk'
     + ';ReturKodeId'
     + ';+kordrelinje_LevKod|CHARACTER||kordrelinje_LevKod|Lev.art.nr'
     + ';+kordlinje_returkode|CHARACTER||kordlinje_returkode|Returkode'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreLinje RETURNS CHARACTER():
  RETURN 
     'kordrelinje_brwcalc.p' /* kordrelinje_LevKod */
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
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.kordre_Navn ~
KOrdreHode.KOrdre_Id KOrdreHode.kordre_cOpt1 KOrdreHode.kordre_PDF ~
KOrdreHode.kordre_LevStatus KOrdreHode.kordre_LevFTekst ~
KOrdreHode.EkstOrdreNr KOrdreHode.Totalt KOrdreHode.ShipmentSendt ~
KOrdreHode.kordre_webcurrentstatus KOrdreHode.kordre_webSent ~
KOrdreHode.WebBatchNr KOrdreHode.WebKode KOrdreHode.SendingsNr ~
KOrdreHode.ReturNr KOrdreHode.Leveringsdato KOrdreHode.AntApnet ~
KOrdreHode.AntPPEti KOrdreHode.kordre_ReturFraBut ~
KOrdreHode.VerkstedMerknad KOrdreHode.MobilTlf KOrdreHode.kordre_Butikk ~
KOrdreHode.kordre_LevAdresse KOrdreHode.LevPostNr KOrdreHode.LevPostSted ~
KOrdreHode.LevLand KOrdreHode.LevStatus KOrdreHode.LevFNr ~
KOrdreHode.ePostAdresse KOrdreHode.Opphav KOrdreHode.KundeNr ~
KOrdreHode.Faktura_Id KOrdreHode.kordre_FakturaNr KOrdreHode.Kundeservice ~
KOrdreHode.DatoTidEndret KOrdreHode.DatoTidOpprettet KOrdreHode.ButikkNr ~
KOrdreHode.RefKOrdre_Id KOrdreHode.Navn KOrdreHode.RegistrertDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.kordre_Navn 
&Scoped-define QUERY-STRING-BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreHode OPEN QUERY BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode


/* Definitions for BROWSE BrwKOrdreLinje                                */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdreLinjeNr ~
KOrdreLinje.KopiKOrdreLinjeNr KOrdreLinje.ByttetKOrdreLinjeNr ~
KOrdreLinje.Aktiv KOrdreLinje.Returnert KOrdreLinje.Manko ~
KOrdreLinje.VareNr KOrdreLinje.Varetekst KOrdreLinje.kordrelinje_LevKod ~
KOrdreLinje.LevFargKod KOrdreLinje.Storl KOrdreLinje.Kode ~
KOrdreLinje.Antall KOrdreLinje.NettoPris KOrdreLinje.Pris KOrdreLinje.MvaKr ~
KOrdreLinje.Linjesum KOrdreLinje.OrgLinjesum KOrdreLinje.BetRef ~
KOrdreLinje.Notat KOrdreLinje.UtleverButikk KOrdreLinje.PlukkButikk ~
KOrdreLinje.kordlinje_returkode KOrdreLinje.ReturKodeId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreLinje ~
KOrdreLinje.KOrdreLinjeNr 
&Scoped-define QUERY-STRING-BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreLinje OPEN QUERY BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY tbKOrdreHode searchKOrdreHode ~
RECT-5 first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode ~
last_tbKOrdreHode refresh_tbKOrdreHode filter_tbKOrdreHode ~
excel_tbKOrdreHode multiSortBrowse_tbKOrdreHode browseconfig_tbKOrdreHode ~
BtnBlank fiEkstOrdreNr fiFraDato tgVisPending BrwKOrdreHode BrwKOrdreLinje 
&Scoped-Define DISPLAYED-OBJECTS fiAntall fiTotalVerdi fiEkstOrdreNr ~
fiFraDato tgVisPending 

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
     SIZE 162.6 BY .43.

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

DEFINE BUTTON last_tbKOrdreHode 
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

DEFINE BUTTON prev_tbKOrdreHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKOrdreHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE fiAntall AS INTEGER FORMAT "->>>>>9":U INITIAL 0 
     LABEL "Antall og verdi på åpne kundeordre (Alle status)" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "Antall ubehandlede ordre fra nettbutikk." NO-UNDO.

DEFINE VARIABLE fiEkstOrdreNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekst.ordrenummer" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 TOOLTIP "Filter på eksternt ordrenummer." NO-UNDO.

DEFINE VARIABLE fiFraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Opprettet etter" 
     VIEW-AS FILL-IN 
     SIZE 17.4 BY 1 TOOLTIP "Vis bare ordre opprettet etter angitt dato" NO-UNDO.

DEFINE VARIABLE fiTotalVerdi AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "total verdi på ubehandlede ordre fra nettbutikk." NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120.8 BY 1.52.

DEFINE RECTANGLE searchKOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY .91.

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 8  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 163 BY 1.29.

DEFINE VARIABLE tgVisPending AS LOGICAL INITIAL NO 
     LABEL "Vis pending" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 TOOLTIP "Viser ordre som ligger som pending ved overøfring til Phnix" NO-UNDO.

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
      KOrdreHode.kordre_Navn COLUMN-LABEL "Navn" FORMAT "X(40)":U
      KOrdreHode.KOrdre_Id COLUMN-LABEL "Ordrenummer" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.kordre_cOpt1 COLUMN-LABEL "Gave" FORMAT "X(10)":U
      KOrdreHode.kordre_PDF COLUMN-LABEL "PDF" FORMAT "*/":U WIDTH 3.2
      KOrdreHode.kordre_LevStatus COLUMN-LABEL "Leveringsstatus" FORMAT "X(30)":U
      KOrdreHode.kordre_LevFTekst COLUMN-LABEL "Leveringsform" FORMAT "X(30)":U
      KOrdreHode.EkstOrdreNr COLUMN-LABEL "Ekst.ordrenr" FORMAT "X(15)":U
      KOrdreHode.Totalt COLUMN-LABEL "Totalt" FORMAT "->>>,>>>,>>9.99":U
      KOrdreHode.ShipmentSendt COLUMN-LABEL "Shipment sendt" FORMAT "X(20)":U
            WIDTH 18
      KOrdreHode.kordre_webcurrentstatus COLUMN-LABEL "Overf.status" FORMAT "X(40)":U
            WIDTH 15
      KOrdreHode.kordre_webSent COLUMN-LABEL "Overf.dato/tid" FORMAT "X(22)":U
      KOrdreHode.WebBatchNr COLUMN-LABEL "WBatchNr" FORMAT ">>>>>>>>>>>>>9":U
            WIDTH 9
      KOrdreHode.WebKode COLUMN-LABEL "Transferstatus" FORMAT "x(8)":U
            WIDTH 9.6
      KOrdreHode.SendingsNr COLUMN-LABEL "Sendingsnummer" FORMAT "X(30)":U
      KOrdreHode.ReturNr COLUMN-LABEL "Retur nr." FORMAT "x(30)":U
      KOrdreHode.Leveringsdato COLUMN-LABEL "Leveringsdato" FORMAT "99/99/99":U
      KOrdreHode.AntApnet COLUMN-LABEL "Ant. pksdl" FORMAT ">9":U
            WIDTH 9.8
      KOrdreHode.AntPPEti COLUMN-LABEL "Antall etiketter" FORMAT ">9":U
      KOrdreHode.kordre_ReturFraBut COLUMN-LABEL "Retur fra butikk" FORMAT "X(25)":U
      KOrdreHode.VerkstedMerknad COLUMN-LABEL "Notat" FORMAT "X(150)":U
            WIDTH 40
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
      KOrdreHode.Kundeservice COLUMN-LABEL "KS" FORMAT "*/":U WIDTH 2
      KOrdreHode.DatoTidEndret COLUMN-LABEL "Endret" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      KOrdreHode.DatoTidOpprettet COLUMN-LABEL "DatoTidOpprettet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      KOrdreHode.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      KOrdreHode.RefKOrdre_Id COLUMN-LABEL "KOId" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.Navn COLUMN-LABEL "Navn" FORMAT "X(40)":U
      KOrdreHode.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
  ENABLE
      KOrdreHode.kordre_Navn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 163 BY 13.1 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.

DEFINE BROWSE BrwKOrdreLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreLinje C-Win _STRUCTURED
  QUERY BrwKOrdreLinje NO-LOCK DISPLAY
      KOrdreLinje.KOrdreLinjeNr COLUMN-LABEL "KOrdreLinje" FORMAT ">>>>>>9":U
      KOrdreLinje.KopiKOrdreLinjeNr COLUMN-LABEL "Kopi" FORMAT ">>>>>>9":U
            WIDTH 7
      KOrdreLinje.ByttetKOrdreLinjeNr COLUMN-LABEL "Byttet" FORMAT ">>>>>>>9":U
            WIDTH 7
      KOrdreLinje.Aktiv COLUMN-LABEL "Akt" FORMAT "J/N":U WIDTH 1.8
      KOrdreLinje.Returnert COLUMN-LABEL "Returnert" FORMAT "J/N":U
            WIDTH 3.8
      KOrdreLinje.Manko COLUMN-LABEL "Manko" FORMAT "*/":U WIDTH 3
      KOrdreLinje.VareNr COLUMN-LABEL "VareNr" FORMAT "X(20)":U
      KOrdreLinje.Varetekst COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      KOrdreLinje.kordrelinje_LevKod COLUMN-LABEL "Lev.art.nr" FORMAT "X(30)":U
            WIDTH 15
      KOrdreLinje.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(30)":U
            WIDTH 15
      KOrdreLinje.Storl COLUMN-LABEL "Str" FORMAT "x(10)":U
      KOrdreLinje.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      KOrdreLinje.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9":U
      KOrdreLinje.NettoPris COLUMN-LABEL "Nettopris" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Pris COLUMN-LABEL "Pris" FORMAT "->>,>>9.99":U
      KOrdreLinje.MvaKr COLUMN-LABEL "Mva" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Linjesum COLUMN-LABEL "Linjesum" FORMAT "->>>,>>9.99":U
      KOrdreLinje.OrgLinjesum COLUMN-LABEL "Org. linjesum" FORMAT "->>>,>>9.99":U
            WIDTH 11.4
      KOrdreLinje.BetRef COLUMN-LABEL "Bet.ref." FORMAT "x(20)":U
      KOrdreLinje.Notat COLUMN-LABEL "Notat" FORMAT "X(40)":U
      KOrdreLinje.UtleverButikk COLUMN-LABEL "Utlev. but" FORMAT ">>>>>9":U
      KOrdreLinje.PlukkButikk COLUMN-LABEL "Plukk but." FORMAT ">>>>>9":U
      KOrdreLinje.kordlinje_returkode COLUMN-LABEL "Returkode" FORMAT "X(25)":U
      KOrdreLinje.ReturKodeId COLUMN-LABEL "Ret" FORMAT ">>9":U
  ENABLE
      KOrdreLinje.KOrdreLinjeNr HELP "Linjenummer på faktura"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 163 BY 4.52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 18.33 COL 2.2 WIDGET-ID 30
     first_tbKOrdreHode AT ROW 1.33 COL 2 WIDGET-ID 12
     prev_tbKOrdreHode AT ROW 1.33 COL 6.8 WIDGET-ID 14
     next_tbKOrdreHode AT ROW 1.33 COL 11.4 WIDGET-ID 16
     last_tbKOrdreHode AT ROW 1.33 COL 16 WIDGET-ID 18
     refresh_tbKOrdreHode AT ROW 1.33 COL 20.6 WIDGET-ID 6
     filter_tbKOrdreHode AT ROW 1.33 COL 25.2 WIDGET-ID 28
     excel_tbKOrdreHode AT ROW 1.33 COL 29.8 WIDGET-ID 8
     multiSortBrowse_tbKOrdreHode AT ROW 1.33 COL 34.4 WIDGET-ID 20
     browseconfig_tbKOrdreHode AT ROW 1.33 COL 39 WIDGET-ID 24
     fiAntall AT ROW 2.67 COL 133 COLON-ALIGNED
     fiTotalVerdi AT ROW 2.67 COL 147.6 COLON-ALIGNED NO-LABEL
     BtnBlank AT ROW 3.86 COL 90.6 WIDGET-ID 62
     fiEkstOrdreNr AT ROW 3.91 COL 62 COLON-ALIGNED
     fiFraDato AT ROW 3.95 COL 144.4 COLON-ALIGNED
     tgVisPending AT ROW 4.05 COL 106.8 WIDGET-ID 64
     BrwKOrdreHode AT ROW 5.29 COL 2 WIDGET-ID 200
     BrwKOrdreLinje AT ROW 18.86 COL 2 WIDGET-ID 300
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.24 COL 45.8 WIDGET-ID 60
          FONT 6
     tbKOrdreHode AT ROW 1.24 COL 1 WIDGET-ID 2
     searchKOrdreHode AT ROW 4.24 COL 2 WIDGET-ID 26
     RECT-5 AT ROW 3.67 COL 44.2 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 165 BY 22.67 WIDGET-ID 100.


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
         HEIGHT             = 22.62
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
/* BROWSE-TAB BrwKOrdreHode tgVisPending DEFAULT-FRAME */
/* BROWSE-TAB BrwKOrdreLinje BrwKOrdreHode DEFAULT-FRAME */
ASSIGN 
       BrwKOrdreHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1
       BrwKOrdreHode:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500.

ASSIGN 
       KOrdreLinje.ReturKodeId:VISIBLE IN BROWSE BrwKOrdreLinje = FALSE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR FILL-IN fiAntall IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiAntall:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiTotalVerdi IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTotalVerdi:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       tbKOrdreHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh¤enable,filter;Filter,excel;Eksporter til E&xcel,multiSortBrowse;Sorter på flere kolonner,browseconfig;Column setupmaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreHode
/* Query rebuild information for BROWSE BrwKOrdreHode
     _TblList          = "SkoTex.KOrdreHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KOrdreHode.kordre_Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreHode.KOrdre_Id" "Ordrenummer" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreHode.kordre_cOpt1" "Gave" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreHode.kordre_PDF" "PDF" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreHode.kordre_LevStatus" "Leveringsstatus" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreHode.kordre_LevFTekst" "Leveringsform" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreHode.EkstOrdreNr" "Ekst.ordrenr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Ordrenummer fra eksternt system (Importert ordre)" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreHode.Totalt" "Totalt" "->>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Totalt beløp på faktura" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreHode.ShipmentSendt" "Shipment sendt" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreHode.kordre_webcurrentstatus" "Overf.status" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreHode.kordre_webSent" "Overf.dato/tid" "X(22)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreHode.WebBatchNr" "WBatchNr" ">>>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Peker til overføringsrecord nettbutikk." no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreHode.WebKode" "Transferstatus" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreHode.SendingsNr" "Sendingsnummer" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Sendingsnummer - for sporing." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KOrdreHode.ReturNr" "Retur nr." "x(30)" "CHARACTER" ? ? ? ? ? ? no "Returnr for sporing. Påført returetikett." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KOrdreHode.Leveringsdato" "Leveringsdato" "99/99/99" "DATE" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreHode.AntApnet" "Ant. pksdl" ">9" "INTEGER" ? ? ? ? ? ? no "Antall ganger ordren er åpnet etter første utlevering." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KOrdreHode.AntPPEti" "Antall etiketter" ">9" "INTEGER" ? ? ? ? ? ? no "Antall postpakke etiketter utskrivet." no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KOrdreHode.kordre_ReturFraBut" "Retur fra butikk" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreHode.VerkstedMerknad" "Notat" "X(150)" "CHARACTER" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KOrdreHode.MobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreHode.kordre_Butikk" "Butikk" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KOrdreHode.kordre_LevAdresse" "LevAdresse" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"KOrdreHode.LevPostNr" "Lev. PostNr" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Postnummer" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"KOrdreHode.LevPostSted" "Poststed" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Poststed" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"KOrdreHode.LevLand" "Lev. Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"KOrdreHode.LevStatus" "Lev.status" "x(2)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"KOrdreHode.LevFNr" "Leveringsform" ">9" "INTEGER" ? ? ? ? ? ? no "Leveringsvorm" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"KOrdreHode.ePostAdresse" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"KOrdreHode.Opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Ordre opphav. F.eks. 1-Manuell reg. 2-Nettbutikk." no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"KOrdreHode.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"KOrdreHode.Faktura_Id" "FakturaId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kobling mellom faktura og kundeordre" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"KOrdreHode.kordre_FakturaNr" "FakturaNr" "X(13)" "CHARACTER" ? ? ? ? ? ? no "" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"KOrdreHode.Kundeservice" "KS" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Flagg som settes hvis kundeservice skal behandle ordren." no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > "_<CALC>"
"KOrdreHode.DatoTidEndret" "Endret" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for siste endring på ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > "_<CALC>"
"KOrdreHode.DatoTidOpprettet" "DatoTidOpprettet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for opprettelse av ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > "_<CALC>"
"KOrdreHode.ButikkNr" "Butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk som rekrutterte kunden" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[38]   > "_<CALC>"
"KOrdreHode.RefKOrdre_Id" "KOId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Referanse til opprinnelig kundeordre." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[39]   > "_<CALC>"
"KOrdreHode.Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Navn eller firmanavn" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[40]   > "_<CALC>"
"KOrdreHode.RegistrertDato" "RDato" "99/99/9999" "date" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreLinje
/* Query rebuild information for BROWSE BrwKOrdreLinje
     _TblList          = "SkoTex.KOrdreLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KOrdreLinje.KOrdreLinjeNr" "KOrdreLinje" ">>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Linjenummer på faktura" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreLinje.KopiKOrdreLinjeNr" "Kopi" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Linjkepeker som peker på rad kopiert til eller fra." no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreLinje.ByttetKOrdreLinjeNr" "Byttet" ">>>>>>>9" "integer" ? ? ? ? ? ? no "Vare byttet på returordre på angitt varelinje." no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreLinje.Aktiv" "Akt" "J/N" "LOGICAL" ? ? ? ? ? ? no "Viser om raden er aktiv og skal tas med på faktura o.l." no no "1.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreLinje.Returnert" "Returnert" "J/N" "LOGICAL" ? ? ? ? ? ? no "Ordrelinje er returnert fra kunde." no no "3.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreLinje.Manko" "Manko" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Flagg som sier at det er manko på linjen." no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreLinje.VareNr" "VareNr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Varenummer" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreLinje.Varetekst" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Varetekst" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreLinje.kordrelinje_LevKod" "Lev.art.nr" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreLinje.LevFargKod" "LevFargKod" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreLinje.Storl" "Str" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Størrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreLinje.Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreLinje.Antall" "Antall" "->>,>>9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreLinje.NettoPris" "Nettopris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Nettopris. Pris eksklusive mva og rabatter." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KOrdreLinje.Pris" "Pris" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris (Til kunde)" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KOrdreLinje.MvaKr" "Mva" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreLinje.Linjesum" "Linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KOrdreLinje.OrgLinjesum" "Org. linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KOrdreLinje.BetRef" "Bet.ref." "x(20)" "CHARACTER" ? ? ? ? ? ? no "Betalingsreferanse på tilbakebetaling." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreLinje.Notat" "Notat" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Informasjon til kunde om varen." no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KOrdreLinje.UtleverButikk" "Utlev. but" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Utleveres fra butikk" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreLinje.PlukkButikk" "Plukk but." ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk som skal levere varen til nettbutikken." no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KOrdreLinje.kordlinje_returkode" "Returkode" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"KOrdreLinje.ReturKodeId" "Ret" ">>9" "INTEGER" ? ? ? ? ? ? no "Retur kode. Viser om linjen er returnert." no no "3.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME BtnBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnBlank C-Win
ON CHOOSE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank> */
DO:
  ASSIGN 
    fiEkstOrdreNr:SCREEN-VALUE = ''
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


&Scoped-define SELF-NAME fiFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato C-Win
ON RETURN OF fiFraDato IN FRAME DEFAULT-FRAME /* Opprettet etter */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiFraDato C-Win
ON TAB OF fiFraDato IN FRAME DEFAULT-FRAME /* Opprettet etter */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgVisPending
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgVisPending C-Win
ON VALUE-CHANGED OF tgVisPending IN FRAME DEFAULT-FRAME /* Vis pending */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BekreftRecord C-Win 
PROCEDURE BekreftRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
MESSAGE 'BekreftRecord'
VIEW-AS ALERT-BOX.

  PUBLISH 'OrdrebehandlingOpenQuery'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BekSpedRecord C-Win 
PROCEDURE BekSpedRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for valgt(e) post(er) som bekreftet speditør?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_BekreftetSpeditor.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for alle poster som bekreftet speditør?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_BekreftetSpeditor.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beregnTotaler C-Win 
PROCEDURE beregnTotaler :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcRetParam AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF JBoxServerApi:Instance:CallServerProc("kordrehode_totaler.p","") THEN 
      DO: 
        pcRetParam = JBoxServerApi:Instance:getCallReturnParam().
        IF NUM-ENTRIES(pcRetParam,'|') = 2 THEN 
          ASSIGN 
            fiAntall:SCREEN-VALUE = ENTRY(1,pcRetParam,'|')
            fiTotalVerdi:SCREEN-VALUE = ENTRY(2,pcRetParam,'|')
            .
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
RUN SUPER.

  CASE icParam:
      WHEN '50' OR WHEN '99' THEN otbKOrdreHode:disabledTools = "".
      OTHERWISE otbKOrdreHode:disabledTools = "skrivFaktura".
  END CASE.
  
  IF oBrwKOrdreHode:isCurrent THEN  
  DO WITH FRAME {&FRAME-NAME}:
      IF AVAILABLE KOrdreHode AND (KOrdreHode.kordre_cOpt1 <> '' OR KOrdreHode.LevFNr = 8) THEN
      DO:
          IF (KOrdreHode.kordre_cOpt1 <> '' OR KOrdreHode.LevFNr = 8) THEN       
              oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = IF (KOrdreHode.LevFNr = 8 AND KOrdreHode.kordre_cOpt1 <> "") THEN
                                                         'Pakken er en gave og skal utleveres i butikk.' /* Rød */
                                                     ELSE IF KOrdreHode.kordre_cOpt1 <> "" THEN
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
  DISPLAY fiAntall fiTotalVerdi fiEkstOrdreNr fiFraDato tgVisPending 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY tbKOrdreHode searchKOrdreHode RECT-5 first_tbKOrdreHode 
         prev_tbKOrdreHode next_tbKOrdreHode last_tbKOrdreHode 
         refresh_tbKOrdreHode filter_tbKOrdreHode excel_tbKOrdreHode 
         multiSortBrowse_tbKOrdreHode browseconfig_tbKOrdreHode BtnBlank 
         fiEkstOrdreNr fiFraDato tgVisPending BrwKOrdreHode BrwKOrdreLinje 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreKOrdreRecord C-Win 
PROCEDURE EndreKOrdreRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE KOrdreHode THEN 
    DO:
      RUN KOrdreDetalj.w (INPUT KOrdreHode.KOrdre_Id, OUTPUT bOk).
      IF bOk THEN 
        oBrwKOrdreHode:refreshRow().
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE IkkeLevertRecord C-Win 
PROCEDURE IkkeLevertRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for valgt(e) post(er) som 'Ikke levert'?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_IkkeLevert.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for alle poster som levert til speditør?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_IkkeLevert.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:

 
  IF ENTRY(1,icParam,'|') = '99' THEN 
    oBrwKOrdreHode:baseQuery = "WHERE ButikkNr = 15 AND Opphav = 10 AND LevStatus = '50' AND EkstOrdreNr >= '' AND KundeService = FALSE".
  ELSE 
    oBrwKOrdreHode:baseQuery = "WHERE ButikkNr = 15 AND Opphav = 10 AND LevStatus = '" + ENTRY(1,icParam,'|') + "' AND EkstOrdreNr >= '' AND KundeService = FALSE".

  oBrwKOrdreHode:setQuerySort("ButikkNr,Opphav,LevStatus,EkstOrdreNr;DESC").
  oBrwKOrdreHode:setSearchField(searchKOrdreHode:HANDLE,"EkstOrdreNr").
  oBrwKOrdreHode:setNoResizeY().
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
&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

DO WITH FRAME {&FRAME-NAME}:
  SUBSCRIBE TO 'OrdrebehandlingOpenQuery' ANYWHERE.
  rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.
      
  oBrwKOrdreHode = NEW JBoxBrowse(brwKOrdreHode:HANDLE).
  otbKOrdreHode = NEW JBoxToolbar(tbKOrdreHode:HANDLE).
  
  tgVisPending:HIDDEN = TRUE.
  CASE icParam:
    WHEN '10' THEN otbKOrdreHode:addToolGroup("Bekreft;Bekreft,SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller").
   /* Bekreftet, men ikke behandlet */ WHEN '30' THEN otbKOrdreHode:addToolGroup("SkrivPakkeliste;Skriv pakkeliste,SkrivPkSdl;Skriv pakkseddel,SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller,VisOrdreIUrl;Vis ordrepå web").
   /* Pakkseddel skrevet */            WHEN '35' THEN otbKOrdreHode:addToolGroup("SkrivPPetikett;Skriv postpakke etikett,SkrivPkSdl;Skriv pakkseddel,VisPPetikett;Vis postpakke etikett,SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller,VisOrdreIUrl;Vis ordrepå web").
   /* Postpakke etikett skrevet */     WHEN '40' THEN otbKOrdreHode:addToolGroup("LeverKunde;Utever til kunde,TilPlukk;Til elektronisk plukking,SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,VisPPetikett;Vis postpakke etikett,SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller,VisOrdreIUrl;Vis ordrepå web").
   /* Elektronisk plukking */          WHEN '42' THEN otbKOrdreHode:addToolGroup("SkrivPakkeliste;Skriv pakkeliste,SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,VisPPetikett;Vis postpakke etikett,SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller,VisOrdreIUrl;Vis ordrepå web").
   /* Lever speditør */                WHEN '45' THEN otbKOrdreHode:addToolGroup("SkrivPakkeliste;Skriv pakkeliste,SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,VisPPetikett;Vis postpakke etikett,BekSped;(Bekreftet speditør),LeverKunde;(Utlever til kunde),SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller,VisOrdreIUrl;Vis ordrepå web").
   /* Bekreftet mottatt speditør */    WHEN '47' THEN otbKOrdreHode:addToolGroup("SkrivPakkeliste;Skriv pakkeliste,SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,VisPPetikett;Vis postpakke etikett,LeverKunde;(Utlevert til kunde),IkkeLevert;(Ikke lever),SendOrdreTilKS;Send til kundeservice,Makuler;Kanseller,VisOrdreIUrl;Vis ordrepå web").
   /* Levert kunde og fakturert */     WHEN '50' THEN 
      DO:
        otbKOrdreHode:addToolGroup("SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,VisPPetikett;Vis postpakke etikett,sendEtiMail;Send epost til kunde,SkrivFaktura;Skriv faktura,EndreKOrdre;Endre kundeordreinfo,ResendPHX;Resend PHX,VisOrdreIUrl;Vis ordrepå web").
        tgVisPending:HIDDEN = FALSE.
      END.
   /* Ikke levert */                   WHEN '55' THEN otbKOrdreHode:addToolGroup("SkrivPkSdl;Skriv pakkseddel,SkrivPPetikett;Skriv postpakke etikett,VisPPetikett;Vis postpakke etikett,sendEtiMail;Send epost til kunde,TilSpeditor;Til speditør,LeverKunde;Utlevert til kunde,Makuler;Makuler,SendOrdreTilKS;Send til kundeservice,VisOrdreIUrl;Vis ordrepå web").
   /* Kanselerte 60 */
   /* Retur etikett */                 WHEN '99' THEN otbKOrdreHode:addToolGroup("SkrivPPEtikett;Skriv pakkseddel RETUR etikett,VisPPetikett;Vis postpakke etikett,sendEtiMail;Send epost til kunde,EndreKOrdre;Endre,VisOrdreIUrl;Vis ordrepå web").
  END CASE.
  
  oBrwKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.
  hKOrdre_IdColumn = oBrwKOrdreHode:getColumnHandle("KOrdre_Id").
  hEkstOrdreNrColumn = oBrwKOrdreHode:getColumnHandle("EkstOrdreNr").
    
  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20") THEN
    cNettButikkType = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1").
  
  RUN InitializeComponents.
  oBrwKOrdreLinje = NEW JBoxBrowse(brwKOrdreLinje:HANDLE).
  oBrwKOrdreLinje:setParentBrowseObject(oBrwKOrdreHode,"KOrdre_Id").
  hLinjeAktivColumn = oBrwKOrdreLinje:getColumnHandle("Aktiv").
  hAntallColumn = oBrwKOrdreLinje:getColumnHandle("Antall").
  hNettoPrisColumn = oBrwKOrdreLinje:getColumnHandle("NettoPris").

  IF CAN-DO('10,30,35,40,42,45,47,55',icParam) THEN 
  DO:
    opopupVare = NEW JBoxPopupMenu().
    opopupVare:AddToolGroup('Manko;Merk manko,OppslagModell;Vis i modell liste').
    oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
  END.
  ELSE DO:
    opopupVare = NEW JBoxPopupMenu().
    opopupVare:AddToolGroup('OppslagModell;Vis i modell liste').
    oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
  END.

  oContainer:setSplitBarY(btnSplitBarY:HANDLE).
  oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,150). /* 200 pixels from the top, 300 pixels from the bottom */
  oContainer:setFollowSplitBarY(STRING(BrwKOrdreHode:HANDLE) + ',' + STRING(BrwKOrdreLinje:HANDLE)).
  oContainer:setNoResizeY("BrwKOrdreHode,RECT-5").
  
  /* Her returneres ttKORdreLinje, og kan benyttes statis */
  IF NOT CAN-DO('50,60',icParam) THEN 
    IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_manko.p",'',httKOrdreLinjeBuffer) THEN 
    DO:
      hTempTable = JBoxServerAPI:Instance:getCallReturnTable().
/*      hTempTable:WRITE-JSON('file', 'konv\MankoKOrdreLinje.json', TRUE).*/
    END.
    
  /* Som utgangspunkt skal bare ordre opprettet siste 60 dager vises. */
  ASSIGN 
    fiFraDato = TODAY - 60
    fiFraDato:SCREEN-VALUE = STRING(fiFraDato)
    .
    
END.
RUN setFilter.
RUN beregnTotaler.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeverKundeRecord C-Win 
PROCEDURE LeverKundeRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for valgt(e) post(er) som levert til kunde?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_Lever.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for alle poster som levert til kunde?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processSet("kordrehode_Lever.p","") THEN 
        JBoxSession:Instance:ViewMessage("Feil pga. " + JBoxServerAPI:Instance:getCallMessage()).
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.
  RUN beregnTotaler.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerRecord C-Win 
PROCEDURE MakulerRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  /* For JF */    
  IF AVAILABLE KOrdreHode AND 
     KOrdreHode.Opphav = 10 AND 
     cNettButikkType = "2" /* PRS nettbutikk */ THEN 
  DO:
      IF NOT DYNAMIC-FUNCTION("runproc",
                              "kordre_sjekkmakuler.p",
                              STRING(KOrdreHode.KOrdre_id),
                              ?) THEN
      DO: 
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
          RETURN.
      END.  
  END.

  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO: 
    RUN makuleringsnotat.w (OUTPUT bOk, OUTPUT cTekst).
    
    IF bOk = FALSE OR cTekst = '' THEN 
      RETURN.   
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_makuler.p", cTekst) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Makulering er bare tillatt for en ordre ad gangen."). 
    RETURN.
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MankoRecord C-Win 
PROCEDURE MankoRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Markering av manko kan bare gjøres på en rad ad gangen. Marker bare en rad.").
      RETURN.      
    END.

    IF AVAIL KOrdreLinje THEN 
    DO: 
      IF (KOrdreLinje.VareNr = 'BETALT') THEN 
      RETURN.
    
      IF (KOrdreLinje.KopiKOrdreLinjeNr > 0 OR KOrdreLinje.Aktiv = FALSE) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Mank kan ikke endres på en rad som ikke er aktiv eller kopiert.").
        RETURN.      
      END. 

      IF KORdreLinje.Manko THEN 
        JBoxServerApi:Instance:Update("KOrdreLinje",
                                      KOrdreLinje.RowIdent1,
                                      "Manko",
                                      "FALSE",
                                      FALSE,
                                      "",
                                      TRUE).      
      ELSE           
        JBoxServerApi:Instance:Update("KOrdreLinje",
                                      KOrdreLinje.RowIdent1,
                                      "Manko",
                                      "TRUE",
                                      FALSE,
                                      "",
                                      TRUE).      
   
      oBrwKOrdreLinje:refresh().
    END.
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker rad som skal endres først.").
    RETURN.
  END.

  oBrwKOrdreLinje:refresh().
  PUBLISH 'OrdrebehandlingOpenQuery'.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    DEFINE VARIABLE hQuery   AS HANDLE NO-UNDO.
    
    RUN SUPER.
    
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

/*  IF NOT THIS-PROCEDURE = JBoxMainMenu:Instance:CurrentTabProcHandle THEN*/
    oBrwKOrdreHode:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RefreshRecord C-Win 
PROCEDURE RefreshRecord :
EMPTY TEMP-TABLE ttKOrdreHode.
  EMPTY TEMP-TABLE ttKORdreLinje.
  EMPTY TEMP-TABLE ttArtBas.
    
  /* Her returneres ttKORdreLinje, og kan benyttes statis */
  IF NOT CAN-DO('50,60',icParam) THEN 
    IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_manko.p",'',httKOrdreLinjeBuffer) THEN 
    DO:
      hTempTable = JBoxServerAPI:Instance:getCallReturnTable().
      hTempTable:WRITE-JSON('file', 'konv\MankoKOrdreLinje.json', TRUE).
    END.

  RUN SUPER.
  
  RUN beregnTotaler.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ResendPHXRecord C-Win 
PROCEDURE ResendPHXRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:    
    IF AVAILABLE KOrdreHode THEN 
    DO:
      IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_resendPHX.p",STRING(KOrdreHode.KOrdre_Id),?) THEN
      DO:
        JBoxSession:Instance:ViewMessage("Ordre er nå markert for resending.").
        oBrwKOrdreHode:refreshRow().
      END.
      ELSE 
        JBoxSession:Instance:ViewMessage("Markering av ordre for resendig feilet!!!!").
    END.
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker den ordren som skal resendes til Phønix.").
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
    DEFINE VARIABLE piLevFnr AS INTEGER     NO-UNDO.
    
    RUN SUPER.
    
    IF AVAILABLE KOrdreHode THEN 
    DO:
/*        piLevFnr = INT(DYNAMIC-FUNCTION("getFieldValues","KOrdreHode","WHERE KOrdre_id = " + STRING(KOrdreHode.KOrdre_Id),"LevFnr")).*/
        
        IF KOrdreHode.kordre_cOpt1 <> '' OR KOrdreHode.LevFNr = 8 THEN 
        DO: 
            hKOrdre_IdColumn:BGCOLOR = IF KOrdreHode.kordre_cOpt1 = "" THEN 10 /* GUL */ 
                                 ELSE IF KOrdreHode.LevFNr <> 8 THEN 14  /* Grøn */
                                 ELSE 12. /* Rød */
        END.
        IF NOT CAN-DO('50,60',KOrdreHode.LevStatus) THEN
        DO:
          IF CAN-FIND(FIRST ttKOrdreLinje WHERE
                      ttKOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id AND
                      ttKOrdreLinje.Manko = TRUE) THEN
              hEkstORdreNrColumn:BGCOLOR = 11.
        END.
    END. 

    IF AVAILABLE KOrdreLinje THEN 
    DO:
      IF KOrdreLinje.Aktiv = TRUE AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
        hLinjeAktivColumn:BGCOLOR = 2.
      ELSE IF KOrdreLinje.Aktiv = FALSE AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
        hLinjeAktivColumn:BGCOLOR = 13.
      IF KOrdreLinje.Linjesum <> KOrdreLinje.OrgLinjeSum THEN 
        hNettoPrisColumn:BGCOLOR = 14.

      IF KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND KOrdreLinje.Antall > 0 THEN 
        hLinjeAktivColumn:BGCOLOR = 14.
      ELSE IF KOrdreLinje.ByttetKOrdreLinjeNr > 0 AND KOrdreLinje.Antall < 0 THEN 
          hLinjeAktivColumn:BGCOLOR = 12.
        
      IF KOrdreLinje.Manko AND KOrdreLinje.Aktiv THEN   
        hAntallColumn:BGCOLOR = 11.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendOrdreTilKSRecord C-Win 
PROCEDURE SendOrdreTilKSRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send ordre til kundeservice?") THEN 
        RETURN.
    JBoxServerApi:Instance:Update("KOrdreHode",
                                  KOrdreHode.RowIdent1,
                                  "Kundeservice",
                                  "TRUE",
                                  FALSE,
                                  "",
                                  TRUE).                
    PUBLISH 'OrdrebehandlingOpenQuery'.
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Ordre ikke merkert! Marker en ordre. Bare en ordre kan behandles ad gangen.").
    RETURN.
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

    IF fiFraDato:SCREEN-VALUE <> ? THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'RegistrertDato'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiFraDato:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + '>='
        .
    IF fiEkstOrdreNr:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'EkstOrdreNr'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiEkstOrdreNr:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .
    IF tgVisPending:SCREEN-VALUE = 'YES' OR tgVisPending:SCREEN-VALUE = 'TRUE' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'kordre_webcurrentstatus'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + 'PE'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .
    
    oBrwKOrdreHode:setFilter(cFields,cOperator,cWhere).
    oBrwKOrdreHode:openQuery().
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivFakturaRecord C-Win 
PROCEDURE skrivFakturaRecord :
IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    JBoxSession:Instance:ViewMessage("Faktura kan bare skrives ut for en ordre ad gangen.").
    RETURN.
  END.
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:
    IF NOT AVAILABLE KOrdreHode OR KOrdreHode.Faktura_Id = 0 THEN
    DO:
        JBoxSession:Instance:ViewMessage("Ordre ikke tilgjengelig eller faktura er ikke produsert for ordren.").
        RETURN.
    END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv faktura for valgt ordre?") THEN
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_skrivfaktura.p", JBoxSession:Instance:UserId) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker ordre det skal skrives ut faktura for.").
    RETURN.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPakkelisteRecord C-Win 
PROCEDURE SkrivPakkelisteRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT JBoxServerAPI:Instance:CallServerProc("Kodrehode_pakkeliste.p",'15',?) THEN
    JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  

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
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_pakkseddel.p", JBoxSession:Instance:UserId) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Marker de ordrene som skal skrives ut før utskrift aktiveres.").
    RETURN. 
  END.
  
  PUBLISH 'OrdrebehandlingOpenQuery'.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPPetikettRecord C-Win 
PROCEDURE SkrivPPetikettRecord :
/*RUN SUPER.*/

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisOrdreIUrlRecord C-Win 
PROCEDURE VisOrdreIUrlRecord :
DEFINE VARIABLE pcURL AS CHARACTER NO-UNDO.
  IF AVAILABLE KOrdreHode THEN 
    DO:
      ASSIGN 
        pcURL = 'https://no.gant.com/admin/orders/view/' + LEFT-TRIM(LEFT-TRIM(KOrdreHode.EkstOrdreNr,'5'),'0') + '/'
        .
      DO ON ERROR UNDO, LEAVE:
        oContainer:StartTabWindow('BrowserGantOrdrestatus.w', 'GANT Ordrestatus', FALSE, YES).
        PUBLISH 'settURL' (pcURL).
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

