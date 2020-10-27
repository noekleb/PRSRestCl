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
DEFINE VARIABLE hLinjeAktivColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hEkstOrdreNrColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hAntallColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hLinjeRab%Column AS HANDLE NO-UNDO.
DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hNettoPrisColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hBetRefColumn AS HANDLE NO-UNDO.
DEF VAR opopupVare AS JBoxPopupMenu NO-UNDO.
DEF VAR hTempTable AS HANDLE NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.

DEF VAR httKOrdreLinjeBuffer AS HANDLE NO-UNDO.
{ttKOrdre.i}
httKOrdreLinjeBuffer = BUFFER ttKOrdreLinje:HANDLE.

/*** Start instance property definitions for JBoxBrowse object oBrwKOrdreHode ***/
DEFINE VARIABLE oBrwKOrdreHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KOrdreHode
    FIELD kordre_Navn AS CHARACTER
    FIELD Navn AS CHARACTER
    FIELD KOrdre_Id AS DECIMAL
    FIELD EkstOrdreNr AS CHARACTER
    FIELD kordre_PDF AS LOGICAL
    FIELD kordre_LevStatus AS CHARACTER
    FIELD kordre_LevFTekst AS CHARACTER
    FIELD Totalt AS DECIMAL
    FIELD ShipmentSendt AS CHARACTER
    FIELD kordre_webcurrentstatus AS CHARACTER
    FIELD kordre_webSent AS CHARACTER
    FIELD SendingsNr AS CHARACTER
    FIELD ReturNr AS CHARACTER
    FIELD AntApnet AS INTEGER
    FIELD AntPPEti AS INTEGER
    FIELD Leveringsdato AS DATE
    FIELD kordre_ReturFraBut AS CHARACTER
    FIELD VerkstedMerknad AS CHARACTER
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
    FIELD RefKOrdre_Id AS DECIMAL
    FIELD FirmaNavn AS CHARACTER
    FIELD KontNavn AS CHARACTER
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
     + ';Navn'
     + ';KOrdre_Id'
     + ';EkstOrdreNr'
     + ';Totalt'
     + ';SendingsNr'
     + ';ReturNr'
     + ';AntApnet'
     + ';AntPPEti'
     + ';Leveringsdato'
     + ';VerkstedMerknad'
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
     + ';RefKOrdre_Id'
     + ';FirmaNavn'
     + ';KontNavn'
     + ';+kordre_Navn|CHARACTER||kordre_Navn|Navn'
     + ';+kordre_PDF|LOGICAL||kordre_PDF|PDF'
     + ';+kordre_LevStatus|CHARACTER||kordre_LevStatus|Leveringsstatus'
     + ';+kordre_LevFTekst|CHARACTER||kordre_LevFTekst|Leveringsform'
     + ';+ShipmentSendt|CHARACTER||kordre_ShipmentSendt|Shipment sendt'
     + ';+kordre_webcurrentstatus|CHARACTER||kordre_webcurrentstatus|Overf.status'
     + ';+kordre_webSent|CHARACTER||kordre_webSent|Overf.dato/tid'
     + ';+kordre_ReturFraBut|CHARACTER||kordre_ReturFraBut|Retur fra butikk'
     + ';+kordre_Butikk|CHARACTER||kordre_Butikk|Butikk'
     + ';+kordre_FakturaNr|CHARACTER||kordre_FakturaNr|FakturaNr'
  + ',Kunde'
  + ',LeveringsForm'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKOrdreHode RETURNS CHARACTER():
  RETURN 'EACH Kunde OF KOrdreHode NO-LOCK,EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.EkstOrdreNr DESCENDING'.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKOrdreHode RETURNS CHARACTER():
  RETURN 
     'server/kordre_brwcalc.p' /* kordre_Navn */
   + ',server/kordre_brwcalc.p' /* kordre_PDF */
   + ',kordre_brwcalc.p' /* kordre_LevStatus */
   + ',kordre_brwcalc.p' /* kordre_LevFTekst */
   + ',kordre_brwcalc.p' /* kordre_ShipmentSendt */
   + ',kordre_brwcalc.p' /* kordre_webcurrentstatus */
   + ',kordre_brwcalc.p' /* kordre_webSent */
   + ',kordre_brwcalc.p' /* kordre_ReturFraBut */
   + ',kordre_brwcalc.p' /* kordre_Butikk */
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
    FIELD LinjeRab% AS DECIMAL
    FIELD NettoPris AS DECIMAL
    FIELD Pris AS DECIMAL
    FIELD MvaKr AS DECIMAL
    FIELD Linjesum AS DECIMAL
    FIELD OrgLinjesum AS DECIMAL
    FIELD BetRef AS CHARACTER
    FIELD PlukkButikk AS INTEGER
    FIELD UtleverButikk AS INTEGER
    FIELD KOrdre_Id AS DECIMAL
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
     + ';LinjeRab%'
     + ';NettoPris'
     + ';Pris'
     + ';MvaKr'
     + ';Linjesum'
     + ';OrgLinjesum'
     + ';BetRef'
     + ';PlukkButikk'
     + ';UtleverButikk'
     + ';KOrdre_Id'
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
KOrdreHode.Navn KOrdreHode.KOrdre_Id KOrdreHode.EkstOrdreNr ~
KOrdreHode.kordre_PDF KOrdreHode.kordre_LevStatus ~
KOrdreHode.kordre_LevFTekst KOrdreHode.Totalt KOrdreHode.ShipmentSendt ~
KOrdreHode.kordre_webcurrentstatus KOrdreHode.kordre_webSent ~
KOrdreHode.SendingsNr KOrdreHode.ReturNr KOrdreHode.AntApnet ~
KOrdreHode.AntPPEti KOrdreHode.Leveringsdato KOrdreHode.kordre_ReturFraBut ~
KOrdreHode.VerkstedMerknad KOrdreHode.MobilTlf KOrdreHode.kordre_Butikk ~
KOrdreHode.LevAdresse1 KOrdreHode.LevPostNr KOrdreHode.LevPostSted ~
KOrdreHode.LevLand KOrdreHode.LevStatus KOrdreHode.cOpt1 KOrdreHode.LevFNr ~
KOrdreHode.ePostAdresse KOrdreHode.Opphav KOrdreHode.KundeNr ~
KOrdreHode.Faktura_Id KOrdreHode.kordre_FakturaNr KOrdreHode.DatoTidEndret ~
KOrdreHode.DatoTidOpprettet KOrdreHode.RefKOrdre_Id KOrdreHode.FirmaNavn ~
KOrdreHode.KontNavn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreHode KOrdreHode.kordre_Navn ~
KOrdreHode.Navn 
&Scoped-define ENABLED-TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define QUERY-STRING-BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK, ~
    EACH Kunde OF KOrdreHode NO-LOCK, ~
    EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.EkstOrdreNr DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreHode OPEN QUERY BrwKOrdreHode FOR EACH KOrdreHode NO-LOCK, ~
    EACH Kunde OF KOrdreHode NO-LOCK, ~
    EACH LeveringsForm OF KOrdreHode NO-LOCK BY KOrdreHode.EkstOrdreNr DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreHode KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreHode KOrdreHode


/* Definitions for BROWSE BrwKOrdreLinje                                */
&Scoped-define FIELDS-IN-QUERY-BrwKOrdreLinje KOrdreLinje.KOrdreLinjeNr ~
KOrdreLinje.KopiKOrdreLinjeNr KOrdreLinje.ByttetKOrdreLinjeNr ~
KOrdreLinje.Aktiv KOrdreLinje.Returnert KOrdreLinje.Manko ~
KOrdreLinje.VareNr KOrdreLinje.Varetekst KOrdreLinje.kordrelinje_LevKod ~
KOrdreLinje.LevFargKod KOrdreLinje.Storl KOrdreLinje.Kode ~
KOrdreLinje.Antall KOrdreLinje.LinjeRab% KOrdreLinje.NettoPris ~
KOrdreLinje.Pris KOrdreLinje.MvaKr KOrdreLinje.Linjesum ~
KOrdreLinje.OrgLinjesum KOrdreLinje.BetRef KOrdreLinje.PlukkButikk ~
KOrdreLinje.UtleverButikk KOrdreLinje.KOrdre_Id 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKOrdreLinje ~
KOrdreLinje.KOrdreLinjeNr 
&Scoped-define QUERY-STRING-BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKOrdreLinje OPEN QUERY BrwKOrdreLinje FOR EACH KOrdreLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKOrdreLinje KOrdreLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKOrdreLinje KOrdreLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbKOrdreHode btnSplitBarY searchKOrdreHode ~
first_tbKOrdreHode-10 prev_tbKOrdreHode-10 next_tbKOrdreHode-10 ~
last_tbKOrdreHode-10 first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode ~
last_tbKOrdreHode refresh_tbKOrdreHode filter_tbKOrdreHode ~
excel_tbKOrdreHode UtskriftPkSdl_tbKOrdreHode SkrivPPEtikett_tbKOrdreHode ~
EndreKundeInfo_tbKOrdreHode LeverKunde_tbKOrdreHode ~
SendOrdreTilKOB_tbKOrdreHode Makuler_tbKOrdreHode BrwKOrdreHode ~
BrwKOrdreLinje 
&Scoped-Define DISPLAYED-OBJECTS fiAntall fiTotalVerdi 

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
     SIZE 202.2 BY .38.

DEFINE BUTTON EndreKundeInfo_tbKOrdreHode 
     LABEL "Endre kundeordre info." 
     SIZE 26 BY 1.1 TOOLTIP "Endre ordrens kundeinformasjon".

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

DEFINE BUTTON LeverKunde_tbKOrdreHode 
     LABEL "Utlever til kunde" 
     SIZE 18 BY 1.1 TOOLTIP "Utlever varer på ordre til kunde".

DEFINE BUTTON Makuler_tbKOrdreHode 
     LABEL "Kanseller" 
     SIZE 10.4 BY 1.1 TOOLTIP "Makulering av kundeordre".

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

DEFINE BUTTON SendOrdreTilKOB_tbKOrdreHode 
     LABEL "Send til ordrebehandling" 
     SIZE 25 BY 1.1 TOOLTIP "Sender ordre tilbake til ordrebehandlerne".

DEFINE BUTTON SkrivPPEtikett_tbKOrdreHode 
     LABEL "Skriv postpakke etikett" 
     SIZE 24 BY 1.1 TOOLTIP "Utskrift av postpakke etikett".

DEFINE BUTTON UtskriftPkSdl_tbKOrdreHode 
     LABEL "Utskrift av pakkseddel" 
     SIZE 23 BY 1.1 TOOLTIP "Skriver ut pakkseddel".

DEFINE VARIABLE fiAntall AS INTEGER FORMAT "->>>>>9":U INITIAL 0 
     LABEL "Antall og verdi på åpne kundeordre (Alle status)" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "Antall ubehandlede ordre fra nettbutikk." NO-UNDO.

DEFINE VARIABLE fiTotalVerdi AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 TOOLTIP "total verdi på ubehandlede ordre fra nettbutikk." NO-UNDO.

DEFINE RECTANGLE searchKOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY .91.

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 8  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 202.2 BY 1.29.

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
      KOrdreHode.Navn COLUMN-LABEL "Navn" FORMAT "X(40)":U
      KOrdreHode.KOrdre_Id COLUMN-LABEL "Ordrenummer" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.EkstOrdreNr COLUMN-LABEL "Ekst.ordrenr" FORMAT "X(15)":U
      KOrdreHode.kordre_PDF COLUMN-LABEL "PDF" FORMAT "*/":U WIDTH 3.2
      KOrdreHode.kordre_LevStatus COLUMN-LABEL "Leveringsstatus" FORMAT "X(30)":U
      KOrdreHode.kordre_LevFTekst COLUMN-LABEL "Leveringsform" FORMAT "X(30)":U
      KOrdreHode.Totalt COLUMN-LABEL "Totalt" FORMAT "->>>,>>>,>>9.99":U
      KOrdreHode.ShipmentSendt COLUMN-LABEL "Shipment sendt" FORMAT "X(22)":U
            WIDTH 18
      KOrdreHode.kordre_webcurrentstatus COLUMN-LABEL "Overf.status" FORMAT "X(40)":U
            WIDTH 15
      KOrdreHode.kordre_webSent COLUMN-LABEL "Overf.dato/tid" FORMAT "X(22)":U
      KOrdreHode.SendingsNr COLUMN-LABEL "Sendingsnummer" FORMAT "X(30)":U
      KOrdreHode.ReturNr COLUMN-LABEL "Retur nr." FORMAT "x(30)":U
      KOrdreHode.AntApnet COLUMN-LABEL "Ant. pksdl" FORMAT ">9":U
            WIDTH 9.8
      KOrdreHode.AntPPEti COLUMN-LABEL "Antall etiketter" FORMAT ">9":U
      KOrdreHode.Leveringsdato COLUMN-LABEL "Leveringsdato" FORMAT "99/99/99":U
      KOrdreHode.kordre_ReturFraBut COLUMN-LABEL "Retur fra butikk" FORMAT "X(25)":U
      KOrdreHode.VerkstedMerknad COLUMN-LABEL "Notat" FORMAT "X(40)":U
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
      KOrdreHode.RefKOrdre_Id COLUMN-LABEL "KOId" FORMAT ">>>>>>>>>>>>9":U
      KOrdreHode.FirmaNavn COLUMN-LABEL "Firma" FORMAT "X(30)":U
      KOrdreHode.KontNavn COLUMN-LABEL "Kontaktperson" FORMAT "X(40)":U
  ENABLE
      KOrdreHode.kordre_Navn
      KOrdreHode.Navn HELP "Navn eller firmanavn"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 202 BY 12.14 ROW-HEIGHT-CHARS .76 FIT-LAST-COLUMN.

DEFINE BROWSE BrwKOrdreLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKOrdreLinje C-Win _STRUCTURED
  QUERY BrwKOrdreLinje NO-LOCK DISPLAY
      KOrdreLinje.KOrdreLinjeNr COLUMN-LABEL "KOrdreLinje" FORMAT ">>>>>>9":U
      KOrdreLinje.KopiKOrdreLinjeNr COLUMN-LABEL "Kopi" FORMAT ">>>>>>9":U
            WIDTH 7
      KOrdreLinje.ByttetKOrdreLinjeNr COLUMN-LABEL "Byttet" FORMAT ">>>>>>>9":U
            WIDTH 7
      KOrdreLinje.Aktiv COLUMN-LABEL "Aktiv" FORMAT "J/N":U WIDTH 3
      KOrdreLinje.Returnert COLUMN-LABEL "Retur" FORMAT "J/N":U
            WIDTH 3
      KOrdreLinje.Manko COLUMN-LABEL "Manko" FORMAT "*/":U WIDTH 3
      KOrdreLinje.VareNr COLUMN-LABEL "VareNr" FORMAT "X(20)":U
      KOrdreLinje.Varetekst COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      KOrdreLinje.kordrelinje_LevKod COLUMN-LABEL "Lev.art.nr" FORMAT "X(15)":U
      KOrdreLinje.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(15)":U
      KOrdreLinje.Storl COLUMN-LABEL "Str" FORMAT "x(10)":U
      KOrdreLinje.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      KOrdreLinje.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9":U
      KOrdreLinje.LinjeRab% COLUMN-LABEL "Rabatt%" FORMAT "->>,>>9.99":U
            WIDTH 8
      KOrdreLinje.NettoPris COLUMN-LABEL "Nettopris" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Pris COLUMN-LABEL "Pris" FORMAT "->>,>>9.99":U
            WIDTH 12.2
      KOrdreLinje.MvaKr COLUMN-LABEL "Mva" FORMAT "->>>,>>9.99":U
      KOrdreLinje.Linjesum COLUMN-LABEL "Linjesum" FORMAT "->>>,>>9.99":U
      KOrdreLinje.OrgLinjesum COLUMN-LABEL "Org. linjesum" FORMAT "->>>,>>9.99":U
            WIDTH 11.4
      KOrdreLinje.BetRef COLUMN-LABEL "Bet.ref." FORMAT "x(20)":U
      KOrdreLinje.PlukkButikk COLUMN-LABEL "Plukk but." FORMAT ">>>>>9":U
      KOrdreLinje.UtleverButikk COLUMN-LABEL "Utlev. but" FORMAT ">>>>>9":U
      KOrdreLinje.KOrdre_Id COLUMN-LABEL "FId" FORMAT ">>>>>>>>>>>>9":U
  ENABLE
      KOrdreLinje.KOrdreLinjeNr HELP "Linjenummer pï¿½ faktura"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 202 BY 6.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 16.1 COL 1.8 WIDGET-ID 30
     first_tbKOrdreHode-10 AT ROW 1.33 COL 2 WIDGET-ID 12
     prev_tbKOrdreHode-10 AT ROW 1.33 COL 6.8 WIDGET-ID 14
     next_tbKOrdreHode-10 AT ROW 1.33 COL 11.4 WIDGET-ID 16
     last_tbKOrdreHode-10 AT ROW 1.33 COL 16 WIDGET-ID 18
     first_tbKOrdreHode AT ROW 1.33 COL 20.6 WIDGET-ID 32
     prev_tbKOrdreHode AT ROW 1.33 COL 25.4 WIDGET-ID 34
     next_tbKOrdreHode AT ROW 1.33 COL 30 WIDGET-ID 36
     last_tbKOrdreHode AT ROW 1.33 COL 34.6 WIDGET-ID 38
     refresh_tbKOrdreHode AT ROW 1.33 COL 39.2 WIDGET-ID 6
     filter_tbKOrdreHode AT ROW 1.33 COL 43.8 WIDGET-ID 28
     excel_tbKOrdreHode AT ROW 1.33 COL 48.4 WIDGET-ID 8
     UtskriftPkSdl_tbKOrdreHode AT ROW 1.33 COL 53 WIDGET-ID 42
     SkrivPPEtikett_tbKOrdreHode AT ROW 1.33 COL 76 WIDGET-ID 44
     EndreKundeInfo_tbKOrdreHode AT ROW 1.33 COL 100 WIDGET-ID 50
     LeverKunde_tbKOrdreHode AT ROW 1.33 COL 126.2 WIDGET-ID 40
     SendOrdreTilKOB_tbKOrdreHode AT ROW 1.33 COL 144.4 WIDGET-ID 46
     Makuler_tbKOrdreHode AT ROW 1.33 COL 169.4 WIDGET-ID 48
     fiAntall AT ROW 2.67 COL 106 COLON-ALIGNED
     fiTotalVerdi AT ROW 2.67 COL 120.6 COLON-ALIGNED NO-LABEL
     BrwKOrdreHode AT ROW 3.86 COL 2 WIDGET-ID 200
     BrwKOrdreLinje AT ROW 16.71 COL 2 WIDGET-ID 300
     tbKOrdreHode AT ROW 1.24 COL 1.8 WIDGET-ID 2
     searchKOrdreHode AT ROW 2.67 COL 2 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.8 BY 22.67 WIDGET-ID 100.


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
         WIDTH              = 203.8
         MAX-HEIGHT         = 32.71
         MAX-WIDTH          = 203.8
         VIRTUAL-HEIGHT     = 32.71
         VIRTUAL-WIDTH      = 203.8
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
/* BROWSE-TAB BrwKOrdreHode fiTotalVerdi DEFAULT-FRAME */
/* BROWSE-TAB BrwKOrdreLinje BrwKOrdreHode DEFAULT-FRAME */
ASSIGN 
       BrwKOrdreHode:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 1
       BrwKOrdreHode:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500.

ASSIGN 
       KOrdreHode.Navn:VISIBLE IN BROWSE BrwKOrdreHode = FALSE.

ASSIGN 
       KOrdreLinje.KOrdre_Id:VISIBLE IN BROWSE BrwKOrdreLinje = FALSE.

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
                "first;First,prev;Prev,next;Next,last;Last,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,UtskriftPkSdl;Utskrift av pakkseddel,SkrivPPEtikett;Skriv postpakke etikett,LeverKunde;Utlever til kunde,SendOrdreTilKOB;Send til ordrebehandling,Makuler;Kanseller,EndreKundeInfo;Endremaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreHode
/* Query rebuild information for BROWSE BrwKOrdreHode
     _TblList          = "SkoTex.KOrdreHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "skotex.KOrdreHode.EkstOrdreNr|no"
     _FldNameList[1]   > "_<CALC>"
"KOrdreHode.kordre_Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > KOrdreHode.Navn
"KOrdreHode.Navn" "Navn" "X(40)" "CHARACTER" ? ? ? ? ? ? yes "Navn eller firmanavn" no no "40" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > KOrdreHode.KOrdre_Id
"KOrdreHode.KOrdre_Id" "Ordrenummer" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt faktura id. Tildeles autmatisk av systemet." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > KOrdreHode.EkstOrdreNr
"KOrdreHode.EkstOrdreNr" "Ekst.ordrenr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Ordrenummer fra eksternt system (Importert ordre)" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreHode.kordre_PDF" "PDF" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreHode.kordre_LevStatus" "Leveringsstatus" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreHode.kordre_LevFTekst" "Leveringsform" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > KOrdreHode.Totalt
"KOrdreHode.Totalt" "Totalt" "->>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Totalt beløp på faktura" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreHode.ShipmentSendt" "Shipment sendt" "X(22)" "CHARACTER" ? ? ? ? ? ? no "" no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreHode.kordre_webcurrentstatus" "Overf.status" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreHode.kordre_webSent" "Overf.dato/tid" "X(22)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > KOrdreHode.SendingsNr
"KOrdreHode.SendingsNr" "Sendingsnummer" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Sendingsnummer - for sporing." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > KOrdreHode.ReturNr
"KOrdreHode.ReturNr" "Retur nr." "x(30)" "CHARACTER" ? ? ? ? ? ? no "Returnr for sporing. Pï¿½fï¿½rt returetikett." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > KOrdreHode.AntApnet
"KOrdreHode.AntApnet" "Ant. pksdl" ">9" "INTEGER" ? ? ? ? ? ? no "Antall ganger ordren er ï¿½pnet etter fï¿½rste utlevering." no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > KOrdreHode.AntPPEti
"KOrdreHode.AntPPEti" "Antall etiketter" ">9" "INTEGER" ? ? ? ? ? ? no "Antall postpakke etiketter utskrivet." no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > KOrdreHode.Leveringsdato
"KOrdreHode.Leveringsdato" "Leveringsdato" "99/99/99" "DATE" ? ? ? ? ? ? no "Leveringsdato" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreHode.kordre_ReturFraBut" "Retur fra butikk" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > KOrdreHode.VerkstedMerknad
"KOrdreHode.VerkstedMerknad" "Notat" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > KOrdreHode.MobilTlf
"KOrdreHode.MobilTlf" "Mobiltelefon" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Mobiltelefon" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreHode.kordre_Butikk" "Butikk" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > KOrdreHode.LevAdresse1
"KOrdreHode.LevAdresse1" "Leveringsadresse" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Kundens adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > KOrdreHode.LevPostNr
"KOrdreHode.LevPostNr" "Lev. PostNr" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Postnummer" no no "11.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > KOrdreHode.LevPostSted
"KOrdreHode.LevPostSted" "Poststed" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Poststed" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > KOrdreHode.LevLand
"KOrdreHode.LevLand" "Lev. Land" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Land" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > KOrdreHode.LevStatus
"KOrdreHode.LevStatus" "Lev.status" "x(2)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > KOrdreHode.cOpt1
"KOrdreHode.cOpt1" "cOpt1" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > KOrdreHode.LevFNr
"KOrdreHode.LevFNr" "Leveringsform" ">9" "INTEGER" ? ? ? ? ? ? no "Leveringsvorm" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > KOrdreHode.ePostAdresse
"KOrdreHode.ePostAdresse" "E-Post" "X(40)" "CHARACTER" ? ? ? ? ? ? no "E-Post adresse" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > KOrdreHode.Opphav
"KOrdreHode.Opphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Ordre opphav. F.eks. 1-Manuell reg. 2-Nettbutikk." no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > KOrdreHode.KundeNr
"KOrdreHode.KundeNr" "KundeNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kundenummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > KOrdreHode.Faktura_Id
"KOrdreHode.Faktura_Id" "FakturaId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Kobling mellom faktura og kundeordre" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"KOrdreHode.kordre_FakturaNr" "FakturaNr" "X(13)" "CHARACTER" ? ? ? ? ? ? no "" no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > KOrdreHode.DatoTidEndret
"KOrdreHode.DatoTidEndret" "Endret" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for siste endring pï¿½ ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > KOrdreHode.DatoTidOpprettet
"KOrdreHode.DatoTidOpprettet" "DatoTidOpprettet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato og klokkeslett for opprettelse av ordre." no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > KOrdreHode.RefKOrdre_Id
"KOrdreHode.RefKOrdre_Id" "KOId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Referanse til opprinnelig kundeordre." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > KOrdreHode.FirmaNavn
"KOrdreHode.FirmaNavn" "Firma" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Eget firmanavn" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > KOrdreHode.KontNavn
"KOrdreHode.KontNavn" "Kontaktperson" "X(40)" "CHARACTER" ? ? ? ? ? ? no "Navn på kontaktperson" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKOrdreHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKOrdreLinje
/* Query rebuild information for BROWSE BrwKOrdreLinje
     _TblList          = "SkoTex.KOrdreLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KOrdreLinje.KOrdreLinjeNr" "KOrdreLinje" ">>>>>>9" "INTEGER" ? ? ? ? ? ? yes "Linjenummer pï¿½ faktura" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KOrdreLinje.KopiKOrdreLinjeNr" "Kopi" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Linjkepeker som peker på rad kopiert til eller fra." no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KOrdreLinje.ByttetKOrdreLinjeNr" "Byttet" ">>>>>>>9" "INTEGER" ? ? ? ? ? ? no "Vare byttet på returordre på angitt varelinje." no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KOrdreLinje.Aktiv" "Aktiv" "J/N" "LOGICAL" ? ? ? ? ? ? no "Viser om raden er aktiv og skal tas med pï¿½ faktura o.l." no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KOrdreLinje.Returnert" "Retur" "J/N" "LOGICAL" ? ? ? ? ? ? no "Ordrelinje er returnert fra kunde." no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KOrdreLinje.Manko" "Manko" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Flagg som sier at det er manko på linjen." no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KOrdreLinje.VareNr" "VareNr" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Varenummer" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KOrdreLinje.Varetekst" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Varetekst" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KOrdreLinje.kordrelinje_LevKod" "Lev.art.nr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KOrdreLinje.LevFargKod" "LevFargKod" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Leverandï¿½rens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KOrdreLinje.Storl" "Str" "x(10)" "CHARACTER" ? ? ? ? ? ? no "Stï¿½rrelse" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KOrdreLinje.Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "Strekkode inklusive sjekksiffer." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KOrdreLinje.Antall" "Antall" "->>,>>9" "DECIMAL" ? ? ? ? ? ? no "Antall" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KOrdreLinje.LinjeRab%" "Rabatt%" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjerabatt%" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KOrdreLinje.NettoPris" "Nettopris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Nettopris. Pris eksklusive mva og rabatter." no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KOrdreLinje.Pris" "Pris" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris (Til kunde)" no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KOrdreLinje.MvaKr" "Mva" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KOrdreLinje.Linjesum" "Linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KOrdreLinje.OrgLinjesum" "Org. linjesum" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Linjesum eks. mva" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KOrdreLinje.BetRef" "Bet.ref." "x(20)" "CHARACTER" ? ? ? ? ? ? no "Betalingsreferanse på tilbakebetaling." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KOrdreLinje.PlukkButikk" "Plukk but." ">>>>>9" "integer" ? ? ? ? ? ? no "Butikk som skal levere varen til nettbutikken." no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KOrdreLinje.UtleverButikk" "Utlev. but" ">>>>>9" "integer" ? ? ? ? ? ? no "Utleveres fra butikk" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KOrdreLinje.KOrdre_Id" "FId" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt ordre id. Kobler linjen til KOrdreHode." no no "15.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttVareRecord C-Win 
PROCEDURE ByttVareRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Endring kan bare gjøres på en rad ad gangen. Marker bare en rad.").
      RETURN.      
    END.
    
    IF AVAILABLE KOrdreHode AND CAN-DO('50,60',KOrdrEHode.LevStatus) THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Ordre utlevert. Kan ikke endres!").
      RETURN.      
    END.

    IF AVAIL KOrdreLinje AND KOrdreLinje.VareNr = 'BETALT' THEN 
      RETURN.
    
    IF AVAILABLE KOrdreLinje AND (KOrdreLinje.KopiKOrdreLinjeNr > 0) THEN
    DO:
      JBoxSession:Instance:ViewMessage("Vare kan ikke endres på en rad som ikke er aktiv eller kopiert.").
      RETURN.      
    END. 
   
    CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN LagerListeButArtStr.w ('16|20|' + STRING(KOrdreLinje.KOrdre_Id) + '|' + STRING(KOrdreLinje.KOrdreLinjeNr) + '|50|' + STRING(KOrdreLinje.Pris) + '||' + KOrdreLinje.Kode).
    CURRENT-WINDOW:SENSITIVE = TRUE.
/*    oBrwKOrdreLinje:refreshSelectedRows().*/
    oBrwKOrdreLinje:refresh().
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker rad som skal endres først.").
    RETURN.
  END.

  oBrwKOrdreLinje:refresh().
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
DO WITH FRAME {&FRAME-NAME}:
    IF DYNAMIC-FUNCTION("getCurrentObject") = oBrwKOrdreHode:BROWSE-HANDLE  THEN 
    DO:
    END.
    IF DYNAMIC-FUNCTION("getCurrentObject") = oBrwKOrdreLinje:BROWSE-HANDLE  THEN 
    DO:    
      IF AVAIL KOrdreLinje AND (KOrdreLinje.VareNr = 'BETALT' OR KOrdreLinje.Aktiv = FALSE) THEN 
        RETURN.
    END.    
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF DYNAMIC-FUNCTION("getCurrentObject") = oBrwKOrdreHode:BROWSE-HANDLE  THEN 
    DO:
    END.
    IF DYNAMIC-FUNCTION("getCurrentObject") = oBrwKOrdreLinje:BROWSE-HANDLE  THEN 
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
RUN SUPER.

  IF oBrwKOrdreHode:isCurrent THEN
  DO WITH FRAME {&FRAME-NAME}:
      IF AVAILABLE KOrdreHode AND (KOrdreHode.cOpt1 <> '' OR KOrdreHode.LevFNr = 8) THEN
      DO:
          IF (KOrdreHode.cOpt1 <> '' OR KOrdreHode.LevFNr = 8) THEN
              oBrwKOrdreHode:BROWSE-HANDLE:TOOLTIP = IF (KOrdreHode.LevFNr = 8 AND KOrdreHode.cOpt1 <> "") THEN
                                                         'Pakken er en gave og skal utleveres i butikk.' /* Rï¿½d */
                                                     ELSE IF KOrdreHode.cOpt1 <> "" THEN
                                                         'Pakken skal være en gave.' /* GUL */
                                                     ELSE IF KOrdreHode.LevFNr = 8 THEN
                                                         'Utlevering i butikk.'  /* Grønn */
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
  DISPLAY fiAntall fiTotalVerdi 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tbKOrdreHode btnSplitBarY searchKOrdreHode first_tbKOrdreHode-10 
         prev_tbKOrdreHode-10 next_tbKOrdreHode-10 last_tbKOrdreHode-10 
         first_tbKOrdreHode prev_tbKOrdreHode next_tbKOrdreHode 
         last_tbKOrdreHode refresh_tbKOrdreHode filter_tbKOrdreHode 
         excel_tbKOrdreHode UtskriftPkSdl_tbKOrdreHode 
         SkrivPPEtikett_tbKOrdreHode EndreKundeInfo_tbKOrdreHode 
         LeverKunde_tbKOrdreHode SendOrdreTilKOB_tbKOrdreHode 
         Makuler_tbKOrdreHode BrwKOrdreHode BrwKOrdreLinje 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreKundeInfoRecord C-Win 
PROCEDURE EndreKundeInfoRecord :
DO WITH FRAME {&FRAME-NAME}:
  IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
    DO:    
      RUN KOrdreDetalj.w (INPUT KOrdreHode.KOrdre_Id, OUTPUT bOk).
      IF bOk THEN 
        oBrwKOrdreHode:refreshRow().
    END.
  ELSE DO: 
      JBoxSession:Instance:ViewMessage("Ingen ordre markert. Marker ordren som skal endres.").
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DEFINE VARIABLE pcLevStatusLst AS CHARACTER NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

  pcLevStatusLst = '30,35,40,45,47,50,55,60'.
  oBrwKOrdreHode:baseQuery = "WHERE LevStatus >= '30' AND Levstatus <= '60' AND Opphav = '10'  AND KundeService = 'TRUE'".

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
DEF VAR oBrwFillInLinjeRab% AS JBoxBrowseFillIn NO-UNDO.
DEF VAR oBrwFillInNettoPris AS JBoxBrowseFillIn NO-UNDO.
DEF VAR oBrwFillInBetRef AS JBoxBrowseFillIn NO-UNDO.

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
  oBrwKOrdreHode:setQuerySort("EkstOrdreNr;DESC").
  oBrwKOrdreHode:setSearchField(searchKOrdreHode:HANDLE,"EkstOrdreNr").
  oBrwKOrdreHode:setNoResizeY().

  oBrwKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.
  hKOrdre_IdColumn = oBrwKOrdreHode:getColumnHandle("KOrdre_Id").
  hEkstOrdreNrColumn = oBrwKOrdreHode:getColumnHandle("EkstOrdreNr").

  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20") THEN
    cNettButikkType = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1").

  RUN InitializeComponents.

  oBrwKOrdreLinje = NEW JBoxBrowse(brwKOrdreLinje:HANDLE).
  oBrwKOrdreLinje:setParentBrowseObject(oBrwKOrdreHode,"KOrdre_Id").
  hLinjeAktivColumn = oBrwKOrdreLinje:getColumnHandle("Aktiv").

  CASE icParam:
    WHEN '10' THEN DO:
        opopupVare = NEW JBoxPopupMenu().
        opopupVare:AddToolGroup('ByttVare;Bytt vare/størrelse,SlettKopi;Slett kopiert rad,Manko;Merk manko,OppslagModell;Vis i modell liste').
        oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
      END.
    OTHERWISE DO:
        opopupVare = NEW JBoxPopupMenu().
        opopupVare:AddToolGroup('OppslagModell;Vis i modell liste').
        oBrwKOrdreLinje:POPUP-MENU-OBJECT = opopupVare.
    END.
  END CASE.

  hAntallColumn = oBrwKOrdreLinje:getColumnHandle("Antall").

  CASE icParam:
    WHEN '10' THEN DO:
            oBrwFillInAntall = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"Antall",TRUE).
/*            hLinjeRab%Column = oBrwKOrdreLinje:getColumnHandle("LinjeRab%").             */
/*            oBrwFillInLinjeRab% = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"LinjeRab%",TRUE).*/
/*            oBrwKOrdreLinje:enabledColumns = "NettoPris,Antall,LinjeRab%".*/
            oBrwKOrdreLinje:enabledColumns = "Antall".
            hNettoPrisColumn = oBrwKOrdreLinje:getColumnHandle("NettoPris").
        END.
    WHEN '20' THEN DO:
            hNettoPrisColumn = oBrwKOrdreLinje:getColumnHandle("NettoPris").
            oBrwFillInNettoPris = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"NettoPris",TRUE).
            hBetRefColumn = oBrwKOrdreLinje:getColumnHandle("BetRef").
            oBrwFillInBetRef = NEW JBoxBrowseFillIn(oBrwKOrdreLinje,"BetRef",TRUE).
            oBrwKOrdreLinje:enabledColumns = "NettoPris,LinjeRab%,BetRef".            
        END.
  END CASE.

  oBrwKOrdreHode:setReadOnlyOnReturn = YES.
  oBrwKOrdreHode:enableOnDblClick = YES.
  oBrwKOrdreLinje:setReadOnlyOnReturn = YES.
  oBrwKOrdreLinje:enableOnDblClick = YES.

  oContainer:setSplitBarY(btnSplitBarY:HANDLE).
  oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,150). /* 200 pixels from the top, 300 pixels from the bottom */
  oContainer:setFollowSplitBarY(STRING(BrwKOrdreHode:HANDLE) + ',' + STRING(BrwKOrdreLinje:HANDLE)).
  oContainer:setNoResizeY("BrwKOrdreHode,").
  
    /* Her returneres ttKORdreLinje, og kan benyttes statis */
    IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_manko.p",'',httKOrdreLinjeBuffer) THEN
      hTempTable = JBoxServerAPI:Instance:getCallReturnTable().

/*    hTempTable:WRITE-JSON('file', 'C:\NSoft\Polygon\PRS\konv\MankoKOrdreLinje.json', TRUE).*/

  
END.
oBrwKOrdreHode:OpenQuery().
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MakulerRecord C-Win 
PROCEDURE MakulerRecord :
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
  RUN beregnTotaler.
  
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

    IF AVAILABLE KOrdreHode AND CAN-DO('50,60',KOrdrEHode.LevStatus) THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Ordre utlevert. Kan ikke endres!").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MySaveBrowseFillIn C-Win 
PROCEDURE MySaveBrowseFillIn :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
  DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
  DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO INIT YES.

  IF ihBuffer:BUFFER-FIELD("VareNr"):BUFFER-VALUE = 'BETALT' OR ihBuffer:BUFFER-FIELD("Aktiv"):BUFFER-VALUE = FALSE THEN
  DO:
      JBoxSession:Instance:ViewMessage("Det kan ikke gøres endringer på en kopiert rad eller betalingsrad.").
      RETURN NO-APPLY.
  END.
  IF icParam = '10' AND hAntallColumn:NAME = ihFillIn:NAME THEN
  DO:
      IF NOT CAN-DO('0,1',ihFillIn:SCREEN-VALUE) THEN
      DO:
          JBoxSession:Instance:ViewMessage("Ugyldig verdi. Antall kan bare settes til 1 eller 0.").
          RETURN NO-APPLY.
      END.
  END.
  ELSE IF icParam = '10' AND hLinjeRab%Column:NAME = ihFillIn:NAME THEN
  DO:
      IF ihFillIn:INPUT-VALUE < 0 OR ihFillIn:INPUT-VALUE > 100 THEN
      DO:
          JBoxSession:Instance:ViewMessage("Ugyldig verdi. Rabatt kan bare settes i område 0% til 100%.").
          RETURN NO-APPLY.
      END.
  END.
  ELSE IF icParam = '20' AND hNettoPrisColumn:NAME = ihFillIn:NAME THEN
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
  /* Gør refresh på moder raden - Kordrehode. */
  DYNAMIC-FUNCTION("RefreshRowids",oBrwKOrdreHode:BROWSE-HANDLE,STRING(oBrwKOrdreLinje:PARENT-BUFFER-HANDLE:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)).
/*  oBrwKOrdreLinje:refreshRow().*/
  oBrwKOrdreLinje:refresh().
  PUBLISH 'OrdrebehandlingOpenQuery'.

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
    IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_manko.p",'',httKOrdreLinjeBuffer) THEN
      hTempTable = JBoxServerAPI:Instance:getCallReturnTable().

/*    hTempTable:WRITE-JSON('file', 'C:\NSoft\Polygon\PRS\konv\MankoKOrdreLinje.json', TRUE).*/
  RUN SUPER.
  RUN beregnTotaler.
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
            hKOrdre_IdColumn:BGCOLOR = IF KOrdreHode.cOpt1 = "" THEN 10 /* GRØNN */
                                 ELSE IF KOrdreHode.LevFNr <> 8 THEN 14  /* GUL */
                                 ELSE 12. /* Rï¿½d */
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
        hLinjeAktivColumn:BGCOLOR = 10.
      ELSE IF KOrdreLinje.Aktiv = FALSE AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
        hLinjeAktivColumn:BGCOLOR = 13.
      IF VALID-HANDLE(hNettoPrisColumn) AND KOrdreLinje.Linjesum <> KOrdreLinje.OrgLinjeSum THEN 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendOrdreTilKOBRecord C-Win 
PROCEDURE SendOrdreTilKOBRecord :
IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Send ordre tilbake til ordrebehandlerne?") THEN 
        RETURN.
    JBoxServerApi:Instance:Update("KOrdreHode",
                                  KOrdreHode.RowIdent1,
                                  "Kundeservice",
                                  "FALSE",
                                  FALSE,
                                  "",
                                  TRUE).                
    PUBLISH 'OrdrebehandlingOpenQuery'.
  END.
  ELSE DO: 
    JBoxSession:Instance:ViewMessage("Ordre ikke merkert! Marker ordre som skal flyttes. Bare en ordre kan flytes ad gangen.").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPPEtikettRecord C-Win 
PROCEDURE SkrivPPEtikettRecord :
IF oBrwKOrdreHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv postpakke etikett for valgt(e) post(er)?") THEN 
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_postpakke.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv postpakke etikett for alle poster?") THEN 
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_postpakke.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettKopiRecord C-Win 
PROCEDURE SlettKopiRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF oBrwKOrdreLinje:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Sletting kan bare gjøres på en rad ad gangen. Marker bare en rad.").
      RETURN.      
    END.

    IF AVAILABLE KOrdreHode AND CAN-DO('50,60',KOrdreHode.LevStatus) THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Ordre utlevert. Kan ikke endres!").
      RETURN.      
    END.

    IF AVAILABLE KOrdreLinje THEN 
    DO:
      IF KOrdreLinje.Aktiv AND KOrdreLinje.KopiKOrdreLinjeNr > 0 THEN 
      DO:
        IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal varelinjen slettes og opprinneling linje aktiveres igjen?") THEN
            RETURN.
        IF oBrwKOrdreLinje:processRowsNoMessage("kordrelinje_slettlinje.p", STRING(KOrdreHode.KOrdre_Id) + ',' + STRING(KOrdreLinje.KOrdreLinjeNr)) THEN     
          oBrwKOrdreLinje:refresh().
      END.
      ELSE DO:
        JBoxSession:Instance:ViewMessage("Bare kopierte linjer kan slettes.").
        RETURN.      
      END.
    END.
   
  END.
  ELSE DO:
    JBoxSession:Instance:ViewMessage("Marker rad som skal slettes først.").
    RETURN.
  END.

  oBrwKOrdreLinje:refresh().

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
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for valgt(e) post(er) som levert til speditï¿½r?") THEN
        RETURN.
    IF NOT oBrwKOrdreHode:processRowsNoMessage("kordrehode_LeverSpeditor.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
  END.
  ELSE DO:
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Sett ordre for alle poster som levert til speditï¿½r?") THEN
        RETURN.
    oBrwKOrdreHode:processSet("kordrehode_LeverSpeditor.p","").
  END.

  PUBLISH 'OrdrebehandlingOpenQuery'.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftPkSdlRecord C-Win 
PROCEDURE UtskriftPkSdlRecord :
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

