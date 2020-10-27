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
 
DEFINE VARIABLE bOk         AS LOG    NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBrowse     AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO. 
DEFINE VARIABLE hToolbar    AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE dDato AS DATE NO-UNDO.
DEFINE VARIABLE hSendtOutletColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE iNetButLager AS INTEGER NO-UNDO.
DEFINE VARIABLE hcPalleNrColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hLokasjonColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hVaretypeColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hAntallColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hPkSdlNrColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE hEkstIdColumn AS HANDLE NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lPkSdlId AS DECIMAL NO-UNDO.
DEFINE VARIABLE hSkannRader AS HANDLE NO-UNDO.
DEFINE VARIABLE cVareFelt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVareVerdier AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLevNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iantLinjer AS INTEGER NO-UNDO.
DEFINE VARIABLE iType AS INTEGER NO-UNDO.
DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE httImpFilLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE hLagerliste AS HANDLE NO-UNDO.
DEFINE VARIABLE cLagerliste AS CHARACTER NO-UNDO.
DEF VAR opopupPksdl AS JBoxPopupMenu NO-UNDO.
DEF VAR opopupLinje AS JBoxPopupMenu NO-UNDO.
DEFINE VARIABLE cFilterTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHovedKatLst AS CHARACTER NO-UNDO.

DEF VAR cAnv-KodRowIdList AS CHARACTER NO-UNDO.
DEF VAR cAnv-KodIdList AS CHARACTER NO-UNDO.
DEF VAR cKategoriRowIdList AS CHARACTER NO-UNDO.
DEF VAR cKategoriIdList AS CHARACTER NO-UNDO.
DEF VAR cButikkRowIdList AS CHARACTER NO-UNDO.
DEF VAR cButikkIdList AS CHARACTER NO-UNDO.
DEF VAR cSORowIdList AS CHARACTER NO-UNDO.
DEF VAR cSOIdList AS CHARACTER NO-UNDO.

SUBSCRIBE 'skipInitJukeBox' ANYWHERE.

{ cls\StdFunk\dsttImpFil.i }
{ ttLagerliste.i }
{ ttArtikkelliste.i }
{ windows.i }

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

ASSIGN 
  cLogg = 'PkSdlBehandling' + REPLACE(STRING(TODAY),'/','') 
  .
  
rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner( cLogg ) NO-ERROR.

/* huskelapp :) - undertrykke ordretype 1 og 12 ved bytte av butikknr. */
/*     + ';+pksdl_OrdreTypeSkip|CHARACTER||pksdl_OrdreTypeSkip(ROWID' + icParam + ')|pksdl_OrdreTypeSkip'*/

/*** Start instance property definitions for JBoxBrowse object oBrwPkSdlHode ***/
DEFINE VARIABLE oBrwPkSdlHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE PkSdlHode
    FIELD SendtDato AS DATE
    FIELD PkSdlId AS DECIMAL
    FIELD PkSdlNr AS CHARACTER
    FIELD EkstId AS CHARACTER
    FIELD ButikkNr AS INTEGER
    FIELD pksdl_FraButikk AS CHARACTER
    FIELD PkSdlStatus AS INTEGER
    FIELD pksdl_status AS CHARACTER
    FIELD PkSdlOpphav AS INTEGER
    FIELD OrdreType AS CHARACTER
    FIELD cPalleNr AS CHARACTER
    FIELD Lokasjon AS CHARACTER
    FIELD Varetype AS CHARACTER
    FIELD LagerSesong AS CHARACTER
    FIELD SendtOutlet AS INTEGER
    FIELD pksdl_SOTxt AS CHARACTER
    FIELD SendtFraLagerTilOutlet AS DATETIME
    FIELD pksdl_totbest AS CHARACTER
    FIELD pksdl_levverdi AS CHARACTER
    FIELD pksdl_InnlevDato AS DATE
    FIELD Sesongkode AS CHARACTER
    FIELD pksdl_LandedCost AS CHARACTER
    FIELD LandedCost AS DECIMAL
    FIELD pksdl_Rab1 AS DECIMAL
    FIELD FakturaNr AS DECIMAL
    FIELD pksdl_fakturaBelop AS CHARACTER
    FIELD pksdl_WholeSaleVerdiURab AS DECIMAL
    FIELD pksdl_WholeSaleVerdiMRab AS DECIMAL
    FIELD LeveringsDato AS DATE
    FIELD levnamn AS CHARACTER
    FIELD levnr AS INTEGER
    FIELD MeldingFraLev AS CHARACTER
    FIELD pksdl_Merknad AS CHARACTER
    FIELD Merknad AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD EDato AS DATE
    FIELD pksdl_OrdreTypeSkip AS CHARACTER
    FIELD pksdl_butlst AS CHARACTER
    FIELD pksdl_ArtGroup AS CHARACTER
    FIELD pksdl_MainGroup AS CHARACTER
    FIELD pksdl_ButikkNr AS INTEGER
    FIELD pksdl_LevKod AS CHARACTER
    FIELD pksdl_LevFargKod AS CHARACTER
    FIELD pksdl_Storl AS CHARACTER
    FIELD pksdl_SkipFraButikk AS CHARACTER
    FIELD pksdl_SkipSendtOutlet AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_PkSdlHode FOR TEMP-TABLE PkSdlHode.


FUNCTION getBuffersAndFieldsBrwPkSdlHode RETURNS CHARACTER():
  RETURN
    'PkSdlHode'
     + ';SendtDato'
     + ';PkSdlId'
     + ';PkSdlNr'
     + ';EkstId'
     + ';ButikkNr'
     + ';PkSdlStatus'
     + ';PkSdlOpphav'
     + ';OrdreType'
     + ';cPalleNr'
     + ';Lokasjon'
     + ';Varetype'
     + ';LagerSesong'
     + ';SendtOutlet'
     + ';SendtFraLagerTilOutlet'
     + ';Sesongkode'
     + ';LandedCost'
     + ';FakturaNr'
     + ';LeveringsDato'
     + ';levnamn'
     + ';levnr'
     + ';MeldingFraLev'
     + ';Merknad'
     + ';RegistrertDato'
     + ';EDato'
     + ';+pksdl_FraButikk|CHARACTER||pksdl_FraButikk(ROWID)|Fra butikk'
     + ';+pksdl_status|CHARACTER||pksdl_status|Status'
     + ';+pksdl_SOTxt|CHARACTER||pksdl_SOTxt|SO Tekst'
     + ';+pksdl_totbest|CHARACTER||pksdl_totbest|Antall'
     + ';+pksdl_levverdi|CHARACTER||pksdl_levverdi|Verdi'
     + ';+pksdl_InnlevDato|DATE||pksdl_InnlevDato(ROWID)|Innlevert'
     + ';+pksdl_LandedCost|CHARACTER||pksdl_LandedCost(ROWID)|LandedCost'
     + ';+pksdl_Rab1|DECIMAL||pksdl_Rab1|Rabatt%'
     + ';+pksdl_fakturaBelop|CHARACTER||pksdl_fakturaBelop(ROWID)|FakturaBelop'
     + ';+pksdl_WholeSaleVerdiURab|DECIMAL||pksdl_WholeSaleVerdiURab|WholeSaleVerdiURab'
     + ';+pksdl_WholeSaleVerdiMRab|DECIMAL||pksdl_WholeSaleVerdiMRab|WholeSaleVerdiMRab'
     + ';+pksdl_Merknad|CHARACTER||pksdl_Merknad(ROWID)|Fra butikk'
     + ';+pksdl_OrdreTypeSkip|CHARACTER||pksdl_OrdreTypeSkip(ROWID)|pksdl_OrdreTypeSkip'
     + ';+pksdl_butlst|CHARACTER||pksdl_butlst(ROWID)|Butikk'
     + ';+pksdl_ArtGroup|CHARACTER||pksdl_ArtGroup(ROWID)|ArtGroup'
     + ';+pksdl_MainGroup|CHARACTER||pksdl_MainGroup(ROWID)|MainGroup'
     + ';+pksdl_ButikkNr|INTEGER||pksdl_ButikkNr(ROWID)|Butikknr'
     + ';+pksdl_LevKod|CHARACTER||pksdl_LevKod(ROWID)|LevKod'
     + ';+pksdl_LevFargKod|CHARACTER||pksdl_LevFargKod(ROWID)|LevFargKod'
     + ';+pksdl_Storl|CHARACTER||pksdl_Storl(ROWID)|Str'
     + ';+pksdl_SkipFraButikk|CHARACTER||pksdl_SkipFraButikk(ROWID)|SkipFraButikk'
     + ';+pksdl_SkipSendtOutlet|CHARACTER||pksdl_SkipSendtOutlet(ROWID)|SkipSendtOutlet'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwPkSdlHode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwPkSdlHode RETURNS CHARACTER():
  RETURN 
     'server/pksdl_brwcalc.p' /* pksdl_FraButikk(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_status */
   + ',server/pksdl_brwcalc.p' /* pksdl_SOTxt */
   + ',server/pksdl_brwcalc.p' /* pksdl_totbest */
   + ',server/pksdl_brwcalc.p' /* pksdl_levverdi */
   + ',server/pksdl_brwcalc.p' /* pksdl_InnlevDato(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_LandedCost(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_Rab1 */
   + ',server/pksdl_brwcalc.p' /* pksdl_fakturaBelop(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_WholeSaleVerdiURab */
   + ',server/pksdl_brwcalc.p' /* pksdl_WholeSaleVerdiMRab */
   + ',server/pksdl_brwcalc.p' /* pksdl_Merknad(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_OrdreTypeSkip(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_butlst(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_ArtGroup(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_MainGroup(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_ButikkNr(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_LevKod(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_LevFargKod(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_Storl(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_SkipFraButikk(ROWID) */
   + ',server/pksdl_brwcalc.p' /* pksdl_SkipSendtOutlet(ROWID) */
     .
END FUNCTION.
DEFINE VARIABLE otbPkSdlHode AS JBoxToolbar NO-UNDO.
DEFINE VARIABLE otbPkSdlLinje AS JBoxToolbar NO-UNDO.

/*** Start instance property definitions for JBoxBrowse object oBrwPkSdlLinje ***/
DEFINE VARIABLE oBrwPkSdlLinje AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE PkSdlLinje
    FIELD ButikkNr AS INTEGER
    FIELD Linjenr AS INTEGER
    FIELD PakkeNr AS INTEGER
    FIELD ArtikkelNr AS DECIMAL
    FIELD Beskr AS CHARACTER
    FIELD LevKod AS CHARACTER
    FIELD LevFargKod AS CHARACTER
    FIELD pksdllinje_Storl AS CHARACTER
    FIELD pksdllinje_Sasong AS INTEGER
    FIELD pksdllinje_NyInnkjopsPris AS CHARACTER
    FIELD pksdllinje_NyRab1% AS CHARACTER
    FIELD pksdllinje_NyVareKost AS DECIMAL
    FIELD pksdllinje_NyPris AS DECIMAL
    FIELD Antall AS DECIMAL
    FIELD AntLevert AS DECIMAL
    FIELD AntRest AS DECIMAL
    FIELD Kode AS CHARACTER
    FIELD pksdllinje_gyldig_kode AS LOGICAL
    FIELD pksdllinje_feilkoblet_kode AS LOGICAL
    FIELD pksdllinje_opphav AS INTEGER
    FIELD pksdllinje_pksdlstatus AS INTEGER
    FIELD EDato AS DATE
    FIELD pksdllinje_eTid AS CHARACTER
    FIELD PkSdlId AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
DEFINE BUFFER v_PkSdlLinje FOR TEMP-TABLE PkSdlLinje.


FUNCTION getBuffersAndFieldsBrwPkSdlLinje RETURNS CHARACTER():
  RETURN
    'PkSdlLinje'
     + ';ButikkNr'
     + ';Linjenr'
     + ';PakkeNr'
     + ';ArtikkelNr'
     + ';Beskr'
     + ';LevKod'
     + ';LevFargKod'
     + ';Antall'
     + ';AntLevert'
     + ';AntRest'
     + ';Kode'
     + ';EDato'
     + ';PkSdlId'
     + ';+pksdllinje_Storl|CHARACTER||pksdllinje_Storl|Str'
     + ';+pksdllinje_Sasong|INTEGER||pksdllinje_Sasong|Sesong'
     + ';+pksdllinje_NyInnkjopsPris|CHARACTER||pksdllinje_NyInnkjopsPris|InnkjopsPris'
     + ';+pksdllinje_NyRab1%|CHARACTER||pksdllinje_NyRab1%|Rab%'
     + ';+pksdllinje_NyVareKost|DECIMAL||pksdllinje_NyVareKost|Varekost'
     + ';+pksdllinje_NyPris|DECIMAL||pksdllinje_NyPris|Pris'
     + ';+pksdllinje_gyldig_kode|LOGICAL||pksdllinje_gyldig_kode|Gyldig'
     + ';+pksdllinje_feilkoblet_kode|LOGICAL||pksdllinje_feilkoblet_kode|F.kobl'
     + ';+pksdllinje_opphav|INTEGER||pksdllinje_opphav|Opphav'
     + ';+pksdllinje_pksdlstatus|INTEGER||pksdllinje_pksdlstatus|Ordrestatus'
     + ';+pksdllinje_eTid|CHARACTER||pksdllinje_eTid|Kl'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwPkSdlLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwPkSdlLinje RETURNS CHARACTER():
  RETURN 
     'server/pksdllinje_brwcalc.p' /* pksdllinje_Storl */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_Sasong */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_NyInnkjopsPris */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_NyRab1% */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_NyVareKost */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_NyPris */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_gyldig_kode */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_feilkoblet_kode */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_opphav */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_pksdlstatus */
   + ',server/pksdllinje_brwcalc.p' /* pksdllinje_eTid */
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
&Scoped-define BROWSE-NAME BrwPkSdlHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PkSdlHode PkSdlLinje

/* Definitions for BROWSE BrwPkSdlHode                                  */
&Scoped-define FIELDS-IN-QUERY-BrwPkSdlHode PkSdlHode.SendtDato ~
PkSdlHode.PkSdlId PkSdlHode.PkSdlNr PkSdlHode.EkstId PkSdlHode.ButikkNr ~
PkSdlHode.pksdl_FraButikk PkSdlHode.PkSdlStatus PkSdlHode.pksdl_status ~
PkSdlHode.PkSdlOpphav PkSdlHode.OrdreType PkSdlHode.cPalleNr ~
PkSdlHode.Lokasjon PkSdlHode.Varetype PkSdlHode.LagerSesong ~
PkSdlHode.SendtOutlet PkSdlHode.pksdl_SOTxt ~
PkSdlHode.SendtFraLagerTilOutlet PkSdlHode.pksdl_totbest ~
PkSdlHode.pksdl_levverdi PkSdlHode.pksdl_InnlevDato PkSdlHode.Sesongkode ~
PkSdlHode.pksdl_LandedCost PkSdlHode.LandedCost PkSdlHode.pksdl_Rab1 ~
PkSdlHode.FakturaNr PkSdlHode.pksdl_fakturaBelop ~
PkSdlHode.pksdl_WholeSaleVerdiURab PkSdlHode.pksdl_WholeSaleVerdiMRab ~
PkSdlHode.LeveringsDato PkSdlHode.levnamn PkSdlHode.levnr ~
PkSdlHode.MeldingFraLev PkSdlHode.pksdl_Merknad PkSdlHode.Merknad ~
PkSdlHode.RegistrertDato PkSdlHode.EDato PkSdlHode.pksdl_OrdreTypeSkip ~
PkSdlHode.pksdl_butlst PkSdlHode.pksdl_ArtGroup PkSdlHode.pksdl_MainGroup ~
PkSdlHode.pksdl_ButikkNr PkSdlHode.pksdl_LevKod PkSdlHode.pksdl_LevFargKod ~
PkSdlHode.pksdl_Storl PkSdlHode.pksdl_SkipFraButikk ~
PkSdlHode.pksdl_SkipSendtOutlet 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwPkSdlHode PkSdlHode.SendtDato ~
PkSdlHode.PkSdlId 
&Scoped-define QUERY-STRING-BrwPkSdlHode FOR EACH PkSdlHode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwPkSdlHode OPEN QUERY BrwPkSdlHode FOR EACH PkSdlHode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwPkSdlHode PkSdlHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwPkSdlHode PkSdlHode


/* Definitions for BROWSE BrwPkSdlLinje                                 */
&Scoped-define FIELDS-IN-QUERY-BrwPkSdlLinje PkSdlLinje.ButikkNr ~
PkSdlLinje.Linjenr PkSdlLinje.PakkeNr PkSdlLinje.ArtikkelNr ~
PkSdlLinje.Beskr PkSdlLinje.LevKod PkSdlLinje.LevFargKod ~
PkSdlLinje.pksdllinje_Storl PkSdlLinje.pksdllinje_Sasong ~
PkSdlLinje.pksdllinje_NyInnkjopsPris PkSdlLinje.pksdllinje_NyRab1% ~
PkSdlLinje.pksdllinje_NyVareKost PkSdlLinje.pksdllinje_NyPris ~
PkSdlLinje.Antall PkSdlLinje.AntLevert PkSdlLinje.AntRest PkSdlLinje.Kode ~
PkSdlLinje.pksdllinje_gyldig_kode PkSdlLinje.pksdllinje_feilkoblet_kode ~
PkSdlLinje.pksdllinje_opphav PkSdlLinje.pksdllinje_pksdlstatus ~
PkSdlLinje.EDato PkSdlLinje.pksdllinje_eTid PkSdlLinje.PkSdlId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwPkSdlLinje PkSdlLinje.ButikkNr 
&Scoped-define QUERY-STRING-BrwPkSdlLinje FOR EACH PkSdlLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwPkSdlLinje OPEN QUERY BrwPkSdlLinje FOR EACH PkSdlLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwPkSdlLinje PkSdlLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwPkSdlLinje PkSdlLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSplitBarY tbPkSdlHode tbPkSdlLinje ~
searchPkSdlHode RECT-4 RECT-5 first_tbPkSdlHode prev_tbPkSdlHode ~
next_tbPkSdlHode last_tbPkSdlHode new_tbPkSdlHode delete_tbPkSdlHode ~
refresh_tbPkSdlHode filter_tbPkSdlHode browseconfig_tbPkSdlHode ~
excel_tbPkSdlHode btnUtvalgSO fiPkSdlNr fiLevFargKod fiSOLst ~
btnUtvalgKategori fiSesong fiLevKod fiHovedKatLst btnUtvalgAnv-Kod ~
fiLokasjon fiMainGroup fiAnv-IdLst btnUtvalgButikk fiPalleNr fiStorl ~
fiButikkLst fiVaretype fiArtGroup cbPkSdlStatus BtnBlank btnSum ~
BrwPkSdlHode first_tbPkSdlLinje prev_tbPkSdlLinje next_tbPkSdlLinje ~
last_tbPkSdlLinje copy_tbPkSdlLinje delete_tbPkSdlLinje excel_tbPkSdlLinje ~
LeggTilVare_tbPkSdlLinje fiStrekkode BrwPkSdlLinje 
&Scoped-Define DISPLAYED-OBJECTS fiPkSdlNr fiLevFargKod fiSOLst fiAntPkSdl ~
fiVerdiPkSdl fiSesong fiLevKod fiHovedKatLst fiWholeSalePkSdl fiLokasjon ~
fiMainGroup fiAnv-IdLst fiLCVerdi fiPalleNr fiStorl fiButikkLst fiVaretype ~
fiArtGroup cbPkSdlStatus fiStrekkode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON browseconfig_tbPkSdlHode 
     IMAGE-UP FILE "bmp/table.bmp":U
     LABEL "Column setup" 
     SIZE 4.6 BY 1.1 TOOLTIP "Column setup (ALT-C)".

DEFINE BUTTON BtnBlank 
     LABEL "<Blank filter>" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 142 BY .43.

DEFINE BUTTON btnSum 
     LABEL "Summer" 
     SIZE 16.6 BY 1.14.

DEFINE BUTTON btnUtvalgAnv-Kod 
     LABEL "Brukskode..." 
     SIZE 18.4 BY 1.1.

DEFINE BUTTON btnUtvalgButikk 
     LABEL "Fra butikk..." 
     SIZE 18.4 BY 1.1.

DEFINE BUTTON btnUtvalgKategori 
     LABEL "Hovedategori" 
     SIZE 18.4 BY 1.1.

DEFINE BUTTON btnUtvalgSO 
     LABEL "Send outlet (SO)" 
     SIZE 18.4 BY 1.1.

DEFINE BUTTON copy_tbPkSdlLinje 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbPkSdlHode 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON delete_tbPkSdlLinje 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON excel_tbPkSdlHode 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON excel_tbPkSdlLinje 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbPkSdlHode 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbPkSdlHode 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON first_tbPkSdlLinje 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON last_tbPkSdlHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON last_tbPkSdlLinje 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON LeggTilVare_tbPkSdlLinje 
     LABEL "Legg til vare" 
     SIZE 15.6 BY 1.1 TOOLTIP "Legge til varer via vares�k.".

DEFINE BUTTON new_tbPkSdlHode 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbPkSdlHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON next_tbPkSdlLinje 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbPkSdlHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON prev_tbPkSdlLinje 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbPkSdlHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE VARIABLE cbPkSdlStatus AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Pksdl.status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Ny",10,
                     "Innlevert",20
     DROP-DOWN-LIST
     SIZE 24.6 BY 1 TOOLTIP "Valg av status som skal vises." NO-UNDO.

DEFINE VARIABLE fiAntPkSdl AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Antall vare enheter" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1 TOOLTIP "Antall varer i pakkseddlene" NO-UNDO.

DEFINE VARIABLE fiAnv-IdLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Brukskode lst" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med brukskoder som skal v�re med" NO-UNDO.

DEFINE VARIABLE fiArtGroup AS CHARACTER FORMAT "X(256)":U 
     LABEL "Art.group" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiButikkLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra butikk" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med butikker som skal v�re med" NO-UNDO.

DEFINE VARIABLE fiHovedKatLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hovedkat.lst" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med hovedkategorier so mskal v�re med." NO-UNDO.

DEFINE VARIABLE fiLCVerdi AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "LC verdi" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1 TOOLTIP "LC verdi av varene p� pakkseddlene" NO-UNDO.

DEFINE VARIABLE fiLevFargKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Farge" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiLokasjon AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lokasjon" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 TOOLTIP "Lokasjoner som begynner med ..." NO-UNDO.

DEFINE VARIABLE fiMainGroup AS CHARACTER FORMAT "X(256)":U 
     LABEL "Main group" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiPalleNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pallenr" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 TOOLTIP "Pallenr som begynner med..." NO-UNDO.

DEFINE VARIABLE fiPkSdlNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pakksedelnr" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 TOOLTIP "Filter p� pakkseddelnummer." NO-UNDO.

DEFINE VARIABLE fiSesong AS CHARACTER FORMAT "X(256)":U 
     LABEL "LSesong" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiSOLst AS CHARACTER FORMAT "X(256)":U 
     LABEL "SO lst" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1 TOOLTIP "Liste med SO koder" NO-UNDO.

DEFINE VARIABLE fiStorl AS CHARACTER FORMAT "X(256)":U 
     LABEL "Str" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiStrekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 28 BY 1 TOOLTIP "Registrering av ny rad" NO-UNDO.

DEFINE VARIABLE fiVaretype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Varetype" 
     VIEW-AS FILL-IN 
     SIZE 21.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiVerdiPkSdl AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Wholesale m/rab" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1 TOOLTIP "Sum wholesale med rabatt" NO-UNDO.

DEFINE VARIABLE fiWholeSalePkSdl AS DECIMAL FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "u/rab" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 17 BY 1 TOOLTIP "Sum wholesale uten rabatter" NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147.2 BY 5.86.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.4 BY 5.86.

DEFINE RECTANGLE searchPkSdlHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 29.4 BY .91.

DEFINE RECTANGLE tbPkSdlHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 219 BY 1.29.

DEFINE RECTANGLE tbPkSdlLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 69.2 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwPkSdlHode FOR 
      PkSdlHode SCROLLING.

DEFINE QUERY BrwPkSdlLinje FOR 
      PkSdlLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwPkSdlHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwPkSdlHode C-Win _STRUCTURED
  QUERY BrwPkSdlHode NO-LOCK DISPLAY
      PkSdlHode.SendtDato COLUMN-LABEL "Sendt dato" FORMAT "99/99/9999":U
      PkSdlHode.PkSdlId COLUMN-LABEL "Pakkseddel id" FORMAT ">>>>>>>>>>>>9":U
      PkSdlHode.PkSdlNr COLUMN-LABEL "Pk.sdl.nr" FORMAT "X(15)":U
      PkSdlHode.EkstId COLUMN-LABEL "EkstId" FORMAT "X(15)":U
      PkSdlHode.ButikkNr COLUMN-LABEL "Butikknr" FORMAT ">>>>>9":U
      PkSdlHode.pksdl_FraButikk COLUMN-LABEL "Fra butikk" FORMAT "X(8)":U
      PkSdlHode.PkSdlStatus COLUMN-LABEL "St" FORMAT ">9":U WIDTH 4
      PkSdlHode.pksdl_status COLUMN-LABEL "Status" FORMAT "X(8)":U
      PkSdlHode.PkSdlOpphav COLUMN-LABEL "Opphav" FORMAT ">9":U
      PkSdlHode.OrdreType COLUMN-LABEL "OType" FORMAT "x(5)":U
      PkSdlHode.cPalleNr COLUMN-LABEL "PalleNr" FORMAT "x(8)":U
      PkSdlHode.Lokasjon COLUMN-LABEL "Lokasjon" FORMAT "x(50)":U
      PkSdlHode.Varetype COLUMN-LABEL "Varetype" FORMAT "x(30)":U
      PkSdlHode.LagerSesong COLUMN-LABEL "LSesong" FORMAT "x(15)":U
      PkSdlHode.SendtOutlet COLUMN-LABEL "SO" FORMAT ">>>>>9":U
      PkSdlHode.pksdl_SOTxt COLUMN-LABEL "SO Tekst" FORMAT "X(15)":U
      PkSdlHode.SendtFraLagerTilOutlet COLUMN-LABEL "Sendt outlet" FORMAT "99/99/9999 HH:MM:SS.SSS":U
            WIDTH 27.8
      PkSdlHode.pksdl_totbest COLUMN-LABEL "Antall" FORMAT "X(8)":U
      PkSdlHode.pksdl_levverdi COLUMN-LABEL "Verdi" FORMAT "X(8)":U
      PkSdlHode.pksdl_InnlevDato COLUMN-LABEL "Innlevert" FORMAT "99/99/99":U
            WIDTH 8.2
      PkSdlHode.Sesongkode COLUMN-LABEL "Sesong" FORMAT "x(8)":U
      PkSdlHode.pksdl_LandedCost COLUMN-LABEL "LandedCost" FORMAT "X(10)":U
      PkSdlHode.LandedCost COLUMN-LABEL "LandedCost" FORMAT "->>,>>>,>>>,>>9.99":U
      PkSdlHode.pksdl_Rab1 COLUMN-LABEL "Rabatt%" FORMAT "->>,>>9.9":U
            WIDTH 8
      PkSdlHode.FakturaNr COLUMN-LABEL "FakturaNr" FORMAT ">>>>>>>>>>>>9":U
      PkSdlHode.pksdl_fakturaBelop COLUMN-LABEL "FakturaBelop" FORMAT "X(8)":U
      PkSdlHode.pksdl_WholeSaleVerdiURab COLUMN-LABEL "WholeSaleVerdiURab" FORMAT "->,>>>,>>9.99":U
      PkSdlHode.pksdl_WholeSaleVerdiMRab COLUMN-LABEL "WholeSaleVerdiMRab" FORMAT "->,>>>,>>9.99":U
      PkSdlHode.LeveringsDato COLUMN-LABEL "Faktura utskrevet" FORMAT "99/99/99":U
            WIDTH 13.4
      PkSdlHode.levnamn COLUMN-LABEL "Navn" FORMAT "x(30)":U
      PkSdlHode.levnr COLUMN-LABEL "LevNr" FORMAT "zzzzz9":U WIDTH 6.2
      PkSdlHode.MeldingFraLev COLUMN-LABEL "Merkn. fra lev." FORMAT "X(30)":U
      PkSdlHode.pksdl_Merknad COLUMN-LABEL "Fra butikk" FORMAT "X(25)":U
      PkSdlHode.Merknad COLUMN-LABEL "Merknad" FORMAT "X(30)":U
      PkSdlHode.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      PkSdlHode.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      PkSdlHode.pksdl_OrdreTypeSkip COLUMN-LABEL "pksdl_OrdreTypeSkip" FORMAT "X(8)":U
      PkSdlHode.pksdl_butlst COLUMN-LABEL "Butikk" FORMAT "X(10)":U
            WIDTH 6
      PkSdlHode.pksdl_ArtGroup COLUMN-LABEL "ArtGroup" FORMAT "X(250)":U
            WIDTH 25
      PkSdlHode.pksdl_MainGroup COLUMN-LABEL "MainGroup" FORMAT "X(250)":U
            WIDTH 25
      PkSdlHode.pksdl_ButikkNr COLUMN-LABEL "Butikknr" FORMAT ">>>>>>9":U
      PkSdlHode.pksdl_LevKod COLUMN-LABEL "LevKod" FORMAT "X(250)":U
            WIDTH 25
      PkSdlHode.pksdl_LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(250)":U
            WIDTH 25
      PkSdlHode.pksdl_Storl COLUMN-LABEL "Str" FORMAT "X(250)":U
            WIDTH 25
      PkSdlHode.pksdl_SkipFraButikk COLUMN-LABEL "SkipFraButikk" FORMAT "X(8)":U
      PkSdlHode.pksdl_SkipSendtOutlet COLUMN-LABEL "SkipSendtOutlet" FORMAT "X(8)":U
  ENABLE
      PkSdlHode.SendtDato HELP "Dato da varene er sendt fra leverandør."
      PkSdlHode.PkSdlId HELP "Internt pakkseddelid."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE DROP-TARGET SIZE 220 BY 8.91 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.

DEFINE BROWSE BrwPkSdlLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwPkSdlLinje C-Win _STRUCTURED
  QUERY BrwPkSdlLinje NO-LOCK DISPLAY
      PkSdlLinje.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
      PkSdlLinje.Linjenr COLUMN-LABEL "Linjenr" FORMAT ">>>9":U
      PkSdlLinje.PakkeNr COLUMN-LABEL "Pk.nr" FORMAT ">>>>>9":U
      PkSdlLinje.ArtikkelNr COLUMN-LABEL "Artikkelnr" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 13.2
      PkSdlLinje.Beskr COLUMN-LABEL "Varetekst" FORMAT "x(30)":U
      PkSdlLinje.LevKod COLUMN-LABEL "LevArtNr" FORMAT "x(20)":U
      PkSdlLinje.LevFargKod COLUMN-LABEL "LevFargKod" FORMAT "X(15)":U
      PkSdlLinje.pksdllinje_Storl COLUMN-LABEL "Str" FORMAT "X(8)":U
      PkSdlLinje.pksdllinje_Sasong COLUMN-LABEL "Sesong" FORMAT ">>>>>>9":U
      PkSdlLinje.pksdllinje_NyInnkjopsPris COLUMN-LABEL "InnkjopsPris" FORMAT "X(12)":U
      PkSdlLinje.pksdllinje_NyRab1% COLUMN-LABEL "Rab%" FORMAT "X(10)":U
      PkSdlLinje.pksdllinje_NyVareKost COLUMN-LABEL "Varekost" FORMAT "->>>,>>9.99":U
      PkSdlLinje.pksdllinje_NyPris COLUMN-LABEL "Pris" FORMAT "->>>,>>9.99":U
      PkSdlLinje.Antall COLUMN-LABEL "Antall" FORMAT "->>,>>9.999":U
      PkSdlLinje.AntLevert COLUMN-LABEL "Ant.levert" FORMAT "->>,>>9.999":U
      PkSdlLinje.AntRest COLUMN-LABEL "Ant.rest" FORMAT "->>,>>9.999":U
      PkSdlLinje.Kode COLUMN-LABEL "Strekkode" FORMAT "X(20)":U
      PkSdlLinje.pksdllinje_gyldig_kode COLUMN-LABEL "Gyldig" FORMAT "*/":U
            WIDTH 2.2
      PkSdlLinje.pksdllinje_feilkoblet_kode COLUMN-LABEL "F.kobl" FORMAT "Rydd/":U
            WIDTH 3.2
      PkSdlLinje.pksdllinje_opphav COLUMN-LABEL "Opphav" FORMAT ">>9":U
            WIDTH 4
      PkSdlLinje.pksdllinje_pksdlstatus COLUMN-LABEL "Ordrestatus" FORMAT ">>9":U
      PkSdlLinje.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      PkSdlLinje.pksdllinje_eTid COLUMN-LABEL "Kl" FORMAT "X(8)":U
            WIDTH 3.2
      PkSdlLinje.PkSdlId COLUMN-LABEL "Pakkseddel id" FORMAT ">>>>>>>>>>>>9":U
  ENABLE
      PkSdlLinje.ButikkNr HELP "Butikknummer"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 220 BY 4.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnSplitBarY AT ROW 18.43 COL 30.2 WIDGET-ID 32
     first_tbPkSdlHode AT ROW 1.57 COL 2.2 WIDGET-ID 4
     prev_tbPkSdlHode AT ROW 1.57 COL 7 WIDGET-ID 6
     next_tbPkSdlHode AT ROW 1.57 COL 11.6 WIDGET-ID 8
     last_tbPkSdlHode AT ROW 1.57 COL 16.2 WIDGET-ID 10
     new_tbPkSdlHode AT ROW 1.57 COL 20.8 WIDGET-ID 34
     delete_tbPkSdlHode AT ROW 1.57 COL 25.4 WIDGET-ID 50
     refresh_tbPkSdlHode AT ROW 1.57 COL 30 WIDGET-ID 30
     filter_tbPkSdlHode AT ROW 1.57 COL 34.6 WIDGET-ID 12
     browseconfig_tbPkSdlHode AT ROW 1.57 COL 39.2 WIDGET-ID 14
     excel_tbPkSdlHode AT ROW 1.57 COL 43.8 WIDGET-ID 16
     btnUtvalgSO AT ROW 3.38 COL 144.6 WIDGET-ID 86
     fiPkSdlNr AT ROW 3.48 COL 45.4 COLON-ALIGNED
     fiLevFargKod AT ROW 3.48 COL 81 COLON-ALIGNED
     fiSOLst AT ROW 3.48 COL 118 COLON-ALIGNED
     fiAntPkSdl AT ROW 3.48 COL 200.4 COLON-ALIGNED
     btnUtvalgKategori AT ROW 4.48 COL 144.6 WIDGET-ID 76
     fiVerdiPkSdl AT ROW 4.48 COL 200.4 COLON-ALIGNED
     fiSesong AT ROW 4.52 COL 45.4 COLON-ALIGNED
     fiLevKod AT ROW 4.52 COL 81 COLON-ALIGNED
     fiHovedKatLst AT ROW 4.52 COL 118 COLON-ALIGNED
     fiWholeSalePkSdl AT ROW 5.48 COL 200.4 COLON-ALIGNED
     btnUtvalgAnv-Kod AT ROW 5.52 COL 144.6 WIDGET-ID 78
     fiLokasjon AT ROW 5.57 COL 45.4 COLON-ALIGNED
     fiMainGroup AT ROW 5.57 COL 81 COLON-ALIGNED
     fiAnv-IdLst AT ROW 5.57 COL 118 COLON-ALIGNED
     fiLCVerdi AT ROW 6.52 COL 200.4 COLON-ALIGNED
     btnUtvalgButikk AT ROW 6.57 COL 144.6 WIDGET-ID 82
     fiPalleNr AT ROW 6.62 COL 45.4 COLON-ALIGNED
     fiStorl AT ROW 6.62 COL 81 COLON-ALIGNED
     fiButikkLst AT ROW 6.62 COL 118 COLON-ALIGNED
     fiVaretype AT ROW 7.67 COL 45.4 COLON-ALIGNED
     fiArtGroup AT ROW 7.67 COL 81 COLON-ALIGNED
     cbPkSdlStatus AT ROW 7.67 COL 118 COLON-ALIGNED WIDGET-ID 88
     BtnBlank AT ROW 7.67 COL 163.2 WIDGET-ID 62
     btnSum AT ROW 7.67 COL 202.6 WIDGET-ID 72
     BrwPkSdlHode AT ROW 9.33 COL 2 WIDGET-ID 200
     first_tbPkSdlLinje AT ROW 19 COL 2 WIDGET-ID 38
     prev_tbPkSdlLinje AT ROW 19 COL 6.8 WIDGET-ID 40
     next_tbPkSdlLinje AT ROW 19 COL 11.4 WIDGET-ID 42
     last_tbPkSdlLinje AT ROW 19 COL 16 WIDGET-ID 44
     copy_tbPkSdlLinje AT ROW 19 COL 20.6 WIDGET-ID 52
     delete_tbPkSdlLinje AT ROW 19 COL 25.2 WIDGET-ID 48
     excel_tbPkSdlLinje AT ROW 19 COL 29.8 WIDGET-ID 74
     LeggTilVare_tbPkSdlLinje AT ROW 19 COL 34.4 WIDGET-ID 64
     fiStrekkode AT ROW 19.1 COL 81 COLON-ALIGNED
     BrwPkSdlLinje AT ROW 20.38 COL 2 WIDGET-ID 300
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.71 COL 34.6 WIDGET-ID 60
          FONT 6
     "Sum" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.71 COL 182.4 WIDGET-ID 70
          FONT 6
     tbPkSdlHode AT ROW 1.48 COL 2 WIDGET-ID 2
     tbPkSdlLinje AT ROW 18.91 COL 1.8 WIDGET-ID 36
     searchPkSdlHode AT ROW 8.14 COL 2.6 WIDGET-ID 54
     RECT-4 AT ROW 3.24 COL 32.8 WIDGET-ID 58
     RECT-5 AT ROW 3.24 COL 180.6 WIDGET-ID 68
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.05
         SIZE 225.6 BY 24.33 WIDGET-ID 100.


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
         HEIGHT             = 24.38
         WIDTH              = 222.4
         MAX-HEIGHT         = 24.38
         MAX-WIDTH          = 248.4
         VIRTUAL-HEIGHT     = 24.38
         VIRTUAL-WIDTH      = 248.4
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
/* BROWSE-TAB BrwPkSdlHode btnSum DEFAULT-FRAME */
/* BROWSE-TAB BrwPkSdlLinje fiStrekkode DEFAULT-FRAME */
ASSIGN 
       PkSdlHode.LandedCost:VISIBLE IN BROWSE BrwPkSdlHode = FALSE
       PkSdlHode.pksdl_butlst:VISIBLE IN BROWSE BrwPkSdlHode = FALSE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR FILL-IN fiAntPkSdl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiLCVerdi IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiVerdiPkSdl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiWholeSalePkSdl IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbPkSdlHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,delete;Slett,refresh;Refresh,filter;Filter,browseconfig;Column setup,excel;Eksporter til E&xcelmaxborder".

ASSIGN 
       tbPkSdlLinje:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,copy;Kopier,delete;Slett,excel;Eksporter til E&xcel,LeggTilVare;Legg til vare�enablemaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwPkSdlHode
/* Query rebuild information for BROWSE BrwPkSdlHode
     _TblList          = "SkoTex.PkSdlHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"PkSdlHode.SendtDato" "Sendt dato" "99/99/9999" "DATE" ? ? ? ? ? ? yes "Dato da varene er sendt fra leverandør." no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"PkSdlHode.PkSdlId" "Pakkseddel id" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? yes "Internt pakkseddelid." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"PkSdlHode.PkSdlNr" "Pk.sdl.nr" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Pakkseddelnummer" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"PkSdlHode.EkstId" "EkstId" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Kobllingsfelt for å koble til ekstern ordre." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"PkSdlHode.ButikkNr" "Butikknr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Butikk ordren er stilet til. Settes til 0  hvis den g�r til fle" no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"PkSdlHode.pksdl_FraButikk" "Fra butikk" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"PkSdlHode.PkSdlStatus" "St" ">9" "INTEGER" ? ? ? ? ? ? no "Pakkseddel status" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"PkSdlHode.pksdl_status" "Status" "X(8)" "CHARACTER" ? ? ? ? ? ? no "Pakkseddelens status" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"PkSdlHode.PkSdlOpphav" "Opphav" ">9" "INTEGER" ? ? ? ? ? ? no "Pakkseddelopphav. 1-Internt,2-ERP,3-Pda/Ht" no no "7.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"PkSdlHode.OrdreType" "OType" "x(5)" "CHARACTER" ? ? ? ? ? ? no "Ordretype." no no "6.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"PkSdlHode.cPalleNr" "PalleNr" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"PkSdlHode.Lokasjon" "Lokasjon" "x(50)" "CHARACTER" ? ? ? ? ? ? no "Viser hvilken lokasjon pakkseddelens varer er plassert i." no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"PkSdlHode.Varetype" "Varetype" "x(30)" "CHARACTER" ? ? ? ? ? ? no "Varetype" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"PkSdlHode.LagerSesong" "LSesong" "x(15)" "CHARACTER" ? ? ? ? ? ? no "Sesongkode for pakkseddel med overskuddsvarer." no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"PkSdlHode.SendtOutlet" "SO" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Pakkseddelens varer er sendt til Outlet." no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"PkSdlHode.pksdl_SOTxt" "SO Tekst" "X(15)" "CHARACTER" ? ? ? ? ? ? no "" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"PkSdlHode.SendtFraLagerTilOutlet" "Sendt outlet" "99/99/9999 HH:MM:SS.SSS" "DATETIME" ? ? ? ? ? ? no "Dato/tid n�r pakkseddel ble sendt fra lager til outlet" no no "27.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"PkSdlHode.pksdl_totbest" "Antall" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"PkSdlHode.pksdl_levverdi" "Verdi" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"PkSdlHode.pksdl_InnlevDato" "Innlevert" "99/99/99" "DATE" ? ? ? ? ? ? no "" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"PkSdlHode.Sesongkode" "Sesong" "x(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"PkSdlHode.pksdl_LandedCost" "LandedCost" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"PkSdlHode.LandedCost" "LandedCost" "->>,>>>,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "18.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"PkSdlHode.pksdl_Rab1" "Rabatt%" "->>,>>9.9" "DECIMAL" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"PkSdlHode.FakturaNr" "FakturaNr" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Fakturanummer" no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"PkSdlHode.pksdl_fakturaBelop" "FakturaBelop" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "12.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"PkSdlHode.pksdl_WholeSaleVerdiURab" "WholeSaleVerdiURab" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "20.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"PkSdlHode.pksdl_WholeSaleVerdiMRab" "WholeSaleVerdiMRab" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"PkSdlHode.LeveringsDato" "Faktura utskrevet" "99/99/99" "DATE" ? ? ? ? ? ? no "Dato når faktura ble skrevet ut" no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"PkSdlHode.levnamn" "Navn" "x(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"PkSdlHode.levnr" "LevNr" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "" no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"PkSdlHode.MeldingFraLev" "Merkn. fra lev." "X(30)" "CHARACTER" ? ? ? ? ? ? no "Merknad fra leverandør." no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"PkSdlHode.pksdl_Merknad" "Fra butikk" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"PkSdlHode.Merknad" "Merknad" "X(30)" "CHARACTER" ? ? ? ? ? ? no "Merknad" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > "_<CALC>"
"PkSdlHode.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > "_<CALC>"
"PkSdlHode.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > "_<CALC>"
"PkSdlHode.pksdl_OrdreTypeSkip" "pksdl_OrdreTypeSkip" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "20.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[38]   > "_<CALC>"
"PkSdlHode.pksdl_butlst" "Butikk" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Viser butikk som pakkseddelen er koblet til" no no "6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[39]   > "_<CALC>"
"PkSdlHode.pksdl_ArtGroup" "ArtGroup" "X(250)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[40]   > "_<CALC>"
"PkSdlHode.pksdl_MainGroup" "MainGroup" "X(250)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[41]   > "_<CALC>"
"PkSdlHode.pksdl_ButikkNr" "Butikknr" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[42]   > "_<CALC>"
"PkSdlHode.pksdl_LevKod" "LevKod" "X(250)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[43]   > "_<CALC>"
"PkSdlHode.pksdl_LevFargKod" "LevFargKod" "X(250)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[44]   > "_<CALC>"
"PkSdlHode.pksdl_Storl" "Str" "X(250)" "CHARACTER" ? ? ? ? ? ? no "" no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[45]   > "_<CALC>"
"PkSdlHode.pksdl_SkipFraButikk" "SkipFraButikk" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[46]   > "_<CALC>"
"PkSdlHode.pksdl_SkipSendtOutlet" "SkipSendtOutlet" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "15.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwPkSdlHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwPkSdlLinje
/* Query rebuild information for BROWSE BrwPkSdlLinje
     _TblList          = "SkoTex.PkSdlLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"PkSdlLinje.ButikkNr" "Butikk" ">>>>>9" "INTEGER" ? ? ? ? ? ? yes "Butikknummer" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"PkSdlLinje.Linjenr" "Linjenr" ">>>9" "INTEGER" ? ? ? ? ? ? no "Linjenummer fra pakkseddel." no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"PkSdlLinje.PakkeNr" "Pk.nr" ">>>>>9" "INTEGER" ? ? ? ? ? ? no "Pakkenummer (Medlemm i pakke)" no no "7.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"PkSdlLinje.ArtikkelNr" "Artikkelnr" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? no "Artikkelnummer" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"PkSdlLinje.Beskr" "Varetekst" "x(30)" "CHARACTER" ? ? ? ? ? ? no "Varetekst" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"PkSdlLinje.LevKod" "LevArtNr" "x(20)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens artikkelnummer (Bestillingsnummer)" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"PkSdlLinje.LevFargKod" "LevFargKod" "X(15)" "CHARACTER" ? ? ? ? ? ? no "Leverandørens fargekode" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"PkSdlLinje.pksdllinje_Storl" "Str" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"PkSdlLinje.pksdllinje_Sasong" "Sesong" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"PkSdlLinje.pksdllinje_NyInnkjopsPris" "InnkjopsPris" "X(12)" "CHARACTER" ? ? ? ? ? ? no "" no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"PkSdlLinje.pksdllinje_NyRab1%" "Rab%" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"PkSdlLinje.pksdllinje_NyVareKost" "Varekost" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"PkSdlLinje.pksdllinje_NyPris" "Pris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"PkSdlLinje.Antall" "Antall" "->>,>>9.999" "DECIMAL" ? ? ? ? ? ? no "Antall sendt fra leverandør" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"PkSdlLinje.AntLevert" "Ant.levert" "->>,>>9.999" "DECIMAL" ? ? ? ? ? ? no "Antall levert" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"PkSdlLinje.AntRest" "Ant.rest" "->>,>>9.999" "DECIMAL" ? ? ? ? ? ? no "Antall som står i rest (Ikke levert))" no no "11.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"PkSdlLinje.Kode" "Strekkode" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"PkSdlLinje.pksdllinje_gyldig_kode" "Gyldig" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "2.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"PkSdlLinje.pksdllinje_feilkoblet_kode" "F.kobl" "Rydd/" "LOGICAL" ? ? ? ? ? ? no "" no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"PkSdlLinje.pksdllinje_opphav" "Opphav" ">>9" "INTEGER" ? ? ? ? ? ? no "" no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"PkSdlLinje.pksdllinje_pksdlstatus" "Ordrestatus" ">>9" "INTEGER" ? ? ? ? ? ? no "" no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"PkSdlLinje.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"PkSdlLinje.pksdllinje_eTid" "Kl" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "3.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"PkSdlLinje.PkSdlId" "Pakkseddel id" ">>>>>>>>>>>>9" "DECIMAL" ? ? ? ? ? ? no "Internt pakkseddelid." no no "15.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwPkSdlLinje */
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
ON CHOOSE OF BtnBlank IN FRAME DEFAULT-FRAME /* <Blank filter> */
DO:
  ASSIGN 
    fiPkSdlNr:SCREEN-VALUE = ''
    fiPalleNr:SCREEN-VALUE = ''
    fiLokasjon:SCREEN-VALUE = ''
    fiSesong:SCREEN-VALUE = ''
    fiArtGroup:SCREEN-VALUE = ''
    fiMainGroup:SCREEN-VALUE = ''
    fiVaretype:SCREEN-VALUE = ''
    fiAnv-IdLst:SCREEN-VALUE = ''
    fiAnv-IdLst = ''
    fiHovedKatLst:SCREEN-VALUE = ''
    fiSOLst:SCREEN-VALUE = ''
    fiHovedKatLst = ''
    cAnv-KodRowIdList = ''
    cAnv-KodIdList = ''
    cKategoriRowIdList = ''
    cKategoriIdList = ''
    cButikkRowIdList = ''
    cButikkIdList = ''
    cSORowIdList = ''
    cSOIdList = ''
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


&Scoped-define SELF-NAME btnSum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSum C-Win
ON CHOOSE OF btnSum IN FRAME DEFAULT-FRAME /* Summer */
DO:
  oBrwPkSdlHode:processSet("pksdl_totsum.p","" + '@' + cAnv-KodIdList + '@' + cKategoriIdList + '@' + cButikkIdList).
  cTekst = JBoxServerAPI:Instance:getCallMessage().
  IF NUM-ENTRIES(cTekst,'|') >= 4 THEN 
  DO  WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      fiAntPkSdl:SCREEN-VALUE       = ENTRY(1,cTekst,'|')
      fiVerdiPkSdl:SCREEN-VALUE     = ENTRY(2,cTekst,'|')
      fiLCVerdi:SCREEN-VALUE        = ENTRY(3,cTekst,'|')
      fiWholeSalePkSdl:SCREEN-VALUE = ENTRY(4,cTekst,'|')
      .
  END.
  ELSE 
  DO  WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      fiAntPkSdl:SCREEN-VALUE       = ''
      fiVerdiPkSdl:SCREEN-VALUE     = ''
      fiLCVerdi:SCREEN-VALUE        = ''
      fiWholeSalePkSdl:SCREEN-VALUE = ''
      .
  END.         
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgAnv-Kod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgAnv-Kod C-Win
ON CHOOSE OF btnUtvalgAnv-Kod IN FRAME DEFAULT-FRAME /* Brukskode... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Anv-Kod;Anv-Id;AnvBeskr|Beskrivelse",
                      "where true",
                      INPUT-OUTPUT cAnv-KodRowIdList,
                      "Anv-Id",
                      INPUT-OUTPUT cAnv-KodIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiAnv-IdLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cAnv-KodIdList,'|'))
    .
  IF bOk THEN 
    RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgButikk C-Win
ON CHOOSE OF btnUtvalgButikk IN FRAME DEFAULT-FRAME /* Fra butikk... */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Butiker;Butik;ButNamn;harButikksystem",
                      "where Butiker.harButikksystem = 'TRUE'",
                      INPUT-OUTPUT cButikkRowIdList,
                      "Butik",
                      INPUT-OUTPUT cButikkIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiButikkLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cButikkIdList,'|'))
    .
  IF bOk THEN 
    RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgKategori
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgKategori C-Win
ON CHOOSE OF btnUtvalgKategori IN FRAME DEFAULT-FRAME /* Hovedategori */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "HovedKategori;HovedKatNr||>>>>>9;HovedKatTekst",
                      "WHERE true",
                      INPUT-OUTPUT cKategoriRowIdList,
                      "HovedKatNr",
                      INPUT-OUTPUT cKategoriIdList,
                      "","",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiHovedKatLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cKategoriIdList,'|'))
    .
  IF bOk THEN 
    RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnUtvalgSO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnUtvalgSO C-Win
ON CHOOSE OF btnUtvalgSO IN FRAME DEFAULT-FRAME /* Send outlet (SO) */
DO:
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  /*
<Alle>,,
0 Ikke tildelt,0,
1 Tilgjengelig,1,
2 Skal ikke sendes,2,
3 Sendt Vestby,3,
4 Sendt Algard,4,
5 Sendt eCom,5,
6 Avvent,6,
10 Bestill Vestby,10,
11 Bestill Algard,11  */
  
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "SysPara;Parameter1||x(5);Beskrivelse;!ParaNr",
                      "WHERE SysPara.SysHId = 22 AND SysPara.SysGr  = 30",
                      INPUT-OUTPUT cSORowIdList,
                      "Parameter1",
                      INPUT-OUTPUT cSOIdList,
                      "",
                      "",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  ASSIGN 
    fiSOLst:SCREEN-VALUE = 'Ant.valgt: ' + STRING(NUM-ENTRIES(cSOIdList,'|'))
    .
  IF bOk THEN 
    RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbPkSdlStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbPkSdlStatus C-Win
ON VALUE-CHANGED OF cbPkSdlStatus IN FRAME DEFAULT-FRAME /* Pksdl.status */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiArtGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiArtGroup C-Win
ON RETURN OF fiArtGroup IN FRAME DEFAULT-FRAME /* Art.group */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiArtGroup C-Win
ON TAB OF fiArtGroup IN FRAME DEFAULT-FRAME /* Art.group */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevFargKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON RETURN OF fiLevFargKod IN FRAME DEFAULT-FRAME /* Farge */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevFargKod C-Win
ON TAB OF fiLevFargKod IN FRAME DEFAULT-FRAME /* Farge */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON RETURN OF fiLevKod IN FRAME DEFAULT-FRAME /* Lev.art.nr */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLevKod C-Win
ON TAB OF fiLevKod IN FRAME DEFAULT-FRAME /* Lev.art.nr */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiLokasjon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLokasjon C-Win
ON RETURN OF fiLokasjon IN FRAME DEFAULT-FRAME /* Lokasjon */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiLokasjon C-Win
ON TAB OF fiLokasjon IN FRAME DEFAULT-FRAME /* Lokasjon */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiMainGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMainGroup C-Win
ON RETURN OF fiMainGroup IN FRAME DEFAULT-FRAME /* Main group */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiMainGroup C-Win
ON TAB OF fiMainGroup IN FRAME DEFAULT-FRAME /* Main group */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPalleNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPalleNr C-Win
ON RETURN OF fiPalleNr IN FRAME DEFAULT-FRAME /* Pallenr */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPalleNr C-Win
ON TAB OF fiPalleNr IN FRAME DEFAULT-FRAME /* Pallenr */
DO:
  RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPkSdlNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPkSdlNr C-Win
ON RETURN OF fiPkSdlNr IN FRAME DEFAULT-FRAME /* Pakksedelnr */
DO:
  RUN setfilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPkSdlNr C-Win
ON TAB OF fiPkSdlNr IN FRAME DEFAULT-FRAME /* Pakksedelnr */
DO:
 RUN setFilter.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiSesong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSesong C-Win
ON RETURN OF fiSesong IN FRAME DEFAULT-FRAME /* LSesong */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiSesong C-Win
ON TAB OF fiSesong IN FRAME DEFAULT-FRAME /* LSesong */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStorl C-Win
ON RETURN OF fiStorl IN FRAME DEFAULT-FRAME /* Str */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStorl C-Win
ON TAB OF fiStorl IN FRAME DEFAULT-FRAME /* Str */
DO:
  RUN setFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiStrekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiStrekkode C-Win
ON LEAVE OF fiStrekkode IN FRAME DEFAULT-FRAME /* Strekkode */
DO:
  IF AVAILABLE PkSdlHode AND PkSdlHode.PkSdlStatus <> 10 THEN 
  DO:
    JBoxSession:Instance:ViewMessage("Pakkseddelen er allerede innlevert. Nye linjer kan ikke registreres.").
    RETURN.
  END.
  IF fiStrekkode:SCREEN-VALUE <> '' AND AVAILABLE PkSdlHode THEN 
  DO:
    IF NOT JBoxServerAPI:Instance:Find("Strekkode", "WHERE Kode = '"+ fiStrekkode:SCREEN-VALUE + "'") THEN
    DO:
      JBoxSession:Instance:ViewMessage("Ukjent strekkode!").
      RETURN.
    END.  
    ELSE DO:
      lArtikkelNr = DEC(JBoxServerAPI:Instance:FieldValue("Strekkode.ArtikkelNr")).
      iLevNr = 0.
      IF JBoxServerAPI:Instance:Find("ArtBas", "WHERE ArtikkelNr = '" + STRING(lArtikkelNr) + "'") THEN
        iLevNr = INT(JBoxServerAPI:Instance:FieldValue("ArtBas.LevNr")).
      IF iLevNr <> PkSdlHode.LevNr THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Feil leverandør på artikkelen. Den har leverandør " + STRING(iLevNr) + ", mens pakkseddelen har " + STRING(PkSdlHode.LevNr) + ".").
          fiStrekkode:SCREEN-VALUE = ''.
          RETURN.
        END.
    END.
    
    IF JBoxServerApi:Instance:CallServerProc("pksdllinje_NyttLinjeNr.p", 
                                             STRING(PkSdlHode.PkSdlId)
                                             ) THEN 
      iLinjeNr = INT(JBoxServerApi:Instance:getCallReturnParam()).
    IF NOT JBoxServerApi:Instance:CallServerProc("pksdllinje_NyFraStrekkode.p",  
                                             STRING(PkSdlHode.PkSdlId) + "|" + fiStrekkode:SCREEN-VALUE + '|' + STRING(iLinjeNr) + '|' + STRING(PkSdlHode.ButikkNr)
                                             ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved opprettelse av linje: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END.
    IF NUM-ENTRIES(JBoxServerApi:Instance:getCallReturnParam(),'|') = 2 THEN 
    DO:
      ASSIGN 
        lPkSdlId  = INT(ENTRY(1,JBoxServerApi:Instance:getCallReturnParam(),'|'))
        iLinjeNr = INT(ENTRY(2,JBoxServerApi:Instance:getCallReturnParam(),'|'))
        .
      /* Akkumulerer hvis varen finnes på an varelinje fra før. */
      FIND FIRST v_PkSdlLinje NO-LOCK WHERE 
        v_PkSdlLinje.PkSdlId = lPkSdlId AND 
        v_PkSdlLinje.LinjeNr = iLinjeNr NO-ERROR.
      /* Akkumulerer */  
      IF AVAILABLE v_PkSdlLinje THEN
      DO:
        oBrwPkSdlLinje:BROWSE-HANDLE:QUERY:REPOSITION-TO-ROWID(ROWID(v_PkSdlLinje)).        
        oBrwPkSdlLinje:refreshRow(). /* for å friske opp raden. */
        oBrwPkSdlLinje:displayRecord(). /* For å friske opp update feltet hvis dette er aktivt på raden. */
      END.
      /* Viser ny linje. */
      ELSE DO:
        /* Sjekker og eventuelt legger opp pris. */
        JBoxServerApi:Instance:CallServerProc("pksdllinje_opprett_pris.p", 
                                              STRING(lPkSdlId) + '|' + STRING(iLinjeNr)
                                              ).
        oBrwPkSdlLinje:OpenQuery().
      END.
    END.
    ELSE
      oBrwPkSdlLinje:OpenQuery().
    ASSIGN 
      fiStrekkode:SCREEN-VALUE = ''
      fiStrekkode = ''
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


&Scoped-define SELF-NAME fiVaretype
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiVaretype C-Win
ON RETURN OF fiVaretype IN FRAME DEFAULT-FRAME /* Varetype */
DO:
  RUN setFilter.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiVaretype C-Win
ON TAB OF fiVaretype IN FRAME DEFAULT-FRAME /* Varetype */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkellistePkSdlRecord C-Win 
PROCEDURE ArtikkellistePkSdlRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEFINE VARIABLE piValg       AS INTEGER NO-UNDO.
  DEFINE VARIABLE pcRapport    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcArtikkelLst   AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE ttArtikkelListe.
  
  ASSIGN 
    pcRapport     = rclStandardFunksjoner:getTmpFileName()
    pcArtikkelLst = 'konv\ttArtikkelListe' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json'
    .
  ENTRY(NUM-ENTRIES(pcRapport,'.'),pcRapport,'.') = 'csv'.
  
  /* Kaller her programmet som legger ut en json fil med liste over artikkel og antall som ligger på pakkseddlene. */
  /* Filen de lagrer data i er pcArtikkelLst.                                                                       */
  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Eksport av lageliste for markerte pakkseddler?") THEN 
        RETURN.
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_Artikkelliste.p",pcArtikkelLst + '|' + 
                                                                      fiLevKod:SCREEN-VALUE + '|' + 
                                                                      fiLevFargKod:screen-value + '|' + 
                                                                      fiStorl:SCREEN-VALUE  + '@' + cAnv-KodIdList + '@' + cKategoriIdList + '@' + cButikkIdList
                                                                      ) THEN
        JBoxSession:Instance:ViewMessage("Feil ved bygging av artikkelliste " + JBoxServerAPI:Instance:getCallMessage()). 
    ELSE DO:
      pcArtikkelLst = JBoxServerAPI:Instance:getCallMessage().
      piValg = 1.
    END.  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Eksport av artikkelliste for ALLE pakkseddler i spørring?") THEN 
        RETURN.
    oBrwPkSdlHode:processSet("pksdl_Artikkelliste.p",pcArtikkelLst + '|' + 
                                                     fiLevKod:SCREEN-VALUE + '|' + 
                                                     fiLevFargKod:screen-value + '|' + 
                                                     fiStorl:SCREEN-VALUE  + '@' + cAnv-KodIdList + '@' + cKategoriIdList + '@' + cButikkIdList
                                                     ).
    pcArtikkelLst = JBoxServerAPI:Instance:getCallMessage().
  END.  

  /* Henter JSOn filen og legger den opp i en temp tabell som så returneres. */
  hLagerliste = BUFFER ttArtikkelListe:HANDLE.
  SESSION:SET-WAIT-STATE("GENERAL").
  IF JBoxServerAPI:Instance:CallServerProc("pksdl_artikkelPksdl.p",pcArtikkelLst,hLagerListe) THEN
    hLagerListe = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
    
  /* Eksporterer data hvis det ble funnet noe. */  
  IF CAN-FIND(FIRST ttArtikkelListe) THEN 
  DO:  
    /* TEST TEST */
/*    TEMP-TABLE ttArtikkelListe:WRITE-JSON('file','konv\ttArtikkelListe.json').*/
    
    OUTPUT TO VALUE(pcRapport).
      PUT UNFORMATTED  
        'ArtikkelNr;'  
        'Varetekst;'
        'LevKod;'
        'LevFargKod;'
        'Storl;'
        'Sesong;'
        'MainGroup;'
        'MainGrpTekst;'
        'ArtGroup;'
        'ArtGrpTekst;'
        'PkSdlNr;'
        'SendtDato;'
        'Innlevert;'
        'PalleNr;'
        'Lokasjon;'
        'Varetype;'
        'LSesong;'
        'Opphav;'
        'SO;'
        'InnkjopsPris;'
        'WholeSalePris;'
        'AntPkSdl;'
        'VerdiPkSdl;'
        'LC;'
        'VerdiLC;'
        'VerdiWholeSale'
      SKIP.
    FOR EACH ttArtikkelListe:
      PUT UNFORMATTED
        ttArtikkelListe.ArtikkelNr ';'  
        ttArtikkelListe.Varetekst ';'
        ttArtikkelListe.LevKod ';'
        ttArtikkelListe.LevFargKod ';'
        ttArtikkelListe.Storl ';'
        ttArtikkelListe.Sesong ';'
        ttArtikkelListe.MainGroup ';'
        ttArtikkelListe.MainGrpTekst ';'
        ttArtikkelListe.ArtGroup ';'
        ttArtikkelListe.ArtGrpTekst ';'
        ttArtikkelListe.PkSdlNr ';'
        ttArtikkelListe.SendtDato ';'
        ttArtikkelListe.Innlevert ';'
        ttArtikkelListe.cPalleNr ';'
        ttArtikkelListe.Lokasjon ';'
        ttArtikkelListe.Varetype ';'
        ttArtikkelListe.LagerSesong ';'
        ttArtikkelListe.PkSdlOpphav ';'
        ttArtikkelListe.SO ';'
        ttArtikkelListe.InnkjopsPris ';'
        ttArtikkelListe.WholeSalePris ';'
        ttArtikkelListe.AntPkSdl ';'
        ttArtikkelListe.VerdiPkSdl ';'
        ttArtikkelListe.LC ';'
        ttArtikkelListe.VerdiLC ';'
        ttArtikkelListe.VerdiWholeSale
      SKIP.
    END.
    OUTPUT CLOSE. 
/*    JBoxSession:Instance:ViewMessage("Lageliste lagre i fil konv\totlagerliste.json.").*/
    RUN OpenExcelDocument (pcRapport,'').
    EMPTY TEMP-TABLE ttArtikkelListe.
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Fant ingen data.").
END. /* FRAME */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE PkSdlHode AND AVAILABLE PkSdlLinje THEN 
    DO:
      IF PkSdlHode.PkSdlOpphav <> 8 THEN 
      DO:      
        JBoxSession:Instance:ViewMessage("Nye rader kan bare legges opp på pakksedler med status 'Ny' og Opphav = 8.").
        RETURN.
      END.
      ELSE IF PkSdlHode.PkSdlStatus <> 10 THEN 
      DO:      
        JBoxSession:Instance:ViewMessage("Nye rader kan bare legges opp på pakksedler med status 'Ny' og Opphav = 8.").
        RETURN.
      END.
    END.
    IF NOT JBoxServerApi:Instance:CallServerProc("pksdllinje_NyttLinjeNr.p", 
                                             STRING(PkSdlHode.PkSdlId)
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
    IF NOT JBoxServerApi:Instance:CallServerProc("pksdllinje_kopier.p", 
                                             STRING(PkSdlHode.PkSdlId) + "|" + STRING(PkSdlLinje.LinjeNr) + "|" + STRING(iLinjeNr)
                                             ) THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Feil ved kopiering av linje: " + JBoxServerAPI:Instance:getCallMessage()).
        RETURN.
    END. 
  END.
/*  RUN SUPER.*/
  DO WITH FRAME {&FRAME-NAME}:
  END.

  IF AVAILABLE PkSdlHode THEN 
  DO:
    JBoxServerApi:Instance:CallServerProc("PkSdlSetLandedCost.p", STRING(PkSdlHode.PkSdlId)).
    oBrwPkSdlHode:refreshRow().
  END.   
  
  oBrwPkSdlLinje:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE PksdlLinje THEN
      DO:
        IF (PkSdlLinje.PkSdlLinje_opphav <> 8 OR PkSdlLinje.pksdllinje_pksdlstatus <> 10) THEN
          RETURN.
      END.
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    IF DYNAMIC-FUNCTION("getCurrentObject") = oBrwPkSdlHode:BROWSE-HANDLE  THEN 
    DO:
/*      MESSAGE 'gurre var her'*/
/*      VIEW-AS ALERT-BOX.     */
    END.
    IF DYNAMIC-FUNCTION("getCurrentObject") = oBrwPkSdlLinje:BROWSE-HANDLE  THEN 
    DO:
/*      MESSAGE 'gurre var her'*/
/*      VIEW-AS ALERT-BOX.     */
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
DO WITH FRAME {&FRAME-NAME}:
    /* Ny hode. */
    IF otbPkSdlHode:isCurrent THEN 
    DO:
      IF AVAILABLE PkSdlHode THEN 
      DO:
        IF PkSdlHode.PkSdlStatus = 20 THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Pakkseddelen er innlevert og kan ikke slettes. ").
          RETURN.
        END.
        ELSE IF PkSdlHode.OrdreType <> '90' THEN 
        DO: 
          JBoxSession:Instance:ViewMessage("Pakkseddelen er ikke opprettet fra eCom lager og kan ikke slettes. ").
          RETURN.
        END.
      END.
    END.
    /* Ny rad. */
    ELSE IF otbPkSdlLinje:isCurrent THEN 
    DO:
      IF AVAILABLE PkSdlLinje THEN 
      DO:
        IF PkSdlLinje.pksdllinje_pksdlstatus = 20 THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Pakkseddelen er innlevert og kan ikke slettes. ").
          RETURN.
        END.
        IF PkSdlHode.OrdreType <> '90' THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Pakkseddelen er ikke opprettet fra eCom lager og kan ikke slettes. ").
          RETURN.
        END.
      END.
    END.
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    /* Ny hode. */
    IF otbPkSdlHode:isCurrent THEN 
    DO:
    END.
    /* Ny rad. */
    ELSE IF otbPkSdlLinje:isCurrent THEN 
    DO:
    END.
  END.
  
  IF AVAILABLE PkSdlHode THEN 
  DO:
    JBoxServerApi:Instance:CallServerProc("PkSdlSetLandedCost.p", STRING(PkSdlHode.PkSdlId)).
    oBrwPkSdlHode:refreshRow().
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
    IF oBrwPkSdlHode:isCurrent THEN
    HODE:
    DO:

    END. /* HODE */
    
    CASE icParam:
      WHEN '10' THEN /* Bytte butikknr. på pakkseddel */
        DO:
          IF NOT CAN-DO(otbPkSdlLinje:disabledTools,'Delete') THEN
            otbPkSdlLinje:disabledTools = otbPkSdlLinje:disabledTools + (IF otbPkSdlLinje:disabledTools <> '' THEN ',' ELSE '') + 'Delete'.
          IF NOT CAN-DO(otbPkSdlLinje:disabledTools,'Copy') THEN
            otbPkSdlLinje:disabledTools = otbPkSdlLinje:disabledTools + (IF otbPkSdlLinje:disabledTools <> '' THEN ',' ELSE '') + 'Copy'.
          IF NOT CAN-DO(otbPkSdlLinje:disabledTools,'LeggTilVare') THEN
            otbPkSdlLinje:disabledTools = otbPkSdlLinje:disabledTools + (IF otbPkSdlLinje:disabledTools <> '' THEN ',' ELSE '') + 'LeggTilVare'.
        END.
      WHEN '20' THEN /* Varemottak nettbutiks lager */ 
        DO:
          
        END.
      WHEN '30' THEN /* Fakturakontroll */
        DO:
          IF NOT CAN-DO(otbPkSdlHode:disabledTools,'New') THEN
            otbPkSdlHode:disabledTools = otbPkSdlHode:disabledTools + (IF otbPkSdlHode:disabledTools <> '' THEN ',' ELSE '') + 'New'.
          IF NOT CAN-DO(otbPkSdlHode:disabledTools,'Delete') THEN
            otbPkSdlHode:disabledTools = otbPkSdlHode:disabledTools + (IF otbPkSdlHode:disabledTools <> '' THEN ',' ELSE '') + 'Delete'.

          IF NOT CAN-DO(otbPkSdlLinje:disabledTools,'Delete') THEN
            otbPkSdlLinje:disabledTools = otbPkSdlLinje:disabledTools + (IF otbPkSdlLinje:disabledTools <> '' THEN ',' ELSE '') + 'Delete'.
          IF NOT CAN-DO(otbPkSdlLinje:disabledTools,'Copy') THEN
            otbPkSdlLinje:disabledTools = otbPkSdlLinje:disabledTools + (IF otbPkSdlLinje:disabledTools <> '' THEN ',' ELSE '') + 'Copy'.
          IF NOT CAN-DO(otbPkSdlLinje:disabledTools,'LeggTilVare') THEN
            otbPkSdlLinje:disabledTools = otbPkSdlLinje:disabledTools + (IF otbPkSdlLinje:disabledTools <> '' THEN ',' ELSE '') + 'LeggTilVare'.
        END.
    END CASE.
    
    /* Tar bort tomme entries. */
    otbPkSdlHode:disabledTools = REPLACE(otbPkSdlHode:disabledTools,',,',',').
    /* Tar bort løse ender. */
    otbPkSdlHode:disabledTools = TRIM(otbPkSdlHode:disabledTools,',').
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF oBrwPkSdlHode:isCurrent THEN
    DO:
    
    END.
    IF AVAILABLE PkSdlHode THEN 
    DO:
/*        hPalleNrColumn:SCREEN-VALUE = STRING(PkSdlHode.PalleNr).*/
          
        IF PkSdlHode.OrdreType = '90' AND PkSdlHode.PkSdlStatus = 10 THEN
          fiStrekkode:SENSITIVE = TRUE.
        ELSE
          fiStrekkode:SENSITIVE = FALSE.

    END.
      
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
DEF VAR cFileNames AS CHAR   NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:

    DEFINE VARIABLE cFilNavn AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE bNyFil AS LOG NO-UNDO.
    
    IF SELF:NUM-DROPPED-FILES = 1 THEN DO:
        ASSIGN cFilNavn = SELF:GET-DROPPED-FILE(1).
        IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
            MESSAGE "Tillatte filtyper: '.xls,.xlsx'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
          IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal fil " + cFilNavn + " importeres?") THEN 
              RETURN.
          
          /* Importer fil */
          RUN importerFil (INPUT cFilNavn, 
                           OUTPUT iAntLinjer,
                           OUTPUT iType, 
                           OUTPUT bOk,
                           OUTPUT cMsg). /* Les inn filen i en temp tabell/Datasett. */
          IF NOT bOk THEN 
          DO:
            JBoxSession:Instance:ViewMessage(cMsg).
            RETURN.
          END.

          /* Henter ny info til SendtOutlet. */
          ASSIGN
            bNyFil = TRUE /* hantering av temptablar i LesExcel */
            .
          
          /* Opprette kampanjelinjene på server. */ 
          IF CAN-FIND(FIRST ttImpFil) THEN 
          DO:
            httImpFilLinje = BUFFER ttImpFilLinje:HANDLE.
            
            JBoxServerAPI:Instance:CallServerProc("pksdl_sendtimport.p",
                                                  STRING(iType)+ '|' + cLogg,
                                                  httImpFilLinje
                                                  ).
            oBrwPkSdlHode:openQuery().
            
            JBoxSession:Instance:ViewMessage("Fil " + cFilNavn + "importert.").
          END.
          
        END.
    END.
    ELSE DO:
        JBoxSession:Instance:ViewMessage("Bare en fil er tillatt importer ad gangen!").
        RETURN NO-APPLY.
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
  DISPLAY fiPkSdlNr fiLevFargKod fiSOLst fiAntPkSdl fiVerdiPkSdl fiSesong 
          fiLevKod fiHovedKatLst fiWholeSalePkSdl fiLokasjon fiMainGroup 
          fiAnv-IdLst fiLCVerdi fiPalleNr fiStorl fiButikkLst fiVaretype 
          fiArtGroup cbPkSdlStatus fiStrekkode 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnSplitBarY tbPkSdlHode tbPkSdlLinje searchPkSdlHode RECT-4 RECT-5 
         first_tbPkSdlHode prev_tbPkSdlHode next_tbPkSdlHode last_tbPkSdlHode 
         new_tbPkSdlHode delete_tbPkSdlHode refresh_tbPkSdlHode 
         filter_tbPkSdlHode browseconfig_tbPkSdlHode excel_tbPkSdlHode 
         btnUtvalgSO fiPkSdlNr fiLevFargKod fiSOLst btnUtvalgKategori fiSesong 
         fiLevKod fiHovedKatLst btnUtvalgAnv-Kod fiLokasjon fiMainGroup 
         fiAnv-IdLst btnUtvalgButikk fiPalleNr fiStorl fiButikkLst fiVaretype 
         fiArtGroup cbPkSdlStatus BtnBlank btnSum BrwPkSdlHode 
         first_tbPkSdlLinje prev_tbPkSdlLinje next_tbPkSdlLinje 
         last_tbPkSdlLinje copy_tbPkSdlLinje delete_tbPkSdlLinje 
         excel_tbPkSdlLinje LeggTilVare_tbPkSdlLinje fiStrekkode BrwPkSdlLinje 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FilterRecord C-Win 
PROCEDURE FilterRecord :
DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttPkSdlRecord C-Win 
PROCEDURE FlyttPkSdlRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN 
      cTekst = 'Velg butikk som pakkseddelen skal overføres til.'.
    ELSE 
      cTekst = 'Velg butikk som de valgte ' + STRING(oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS) + 'pakkseddler skal overføres til.'.
    RUN velgButikk.w (INPUT cTekst, OUTPUT iButNr).
    IF iButNr = 0 THEN 
      RETURN.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Bekreft flytting av valgte pakkseddler til varemottak i butikk " + STRING(iButNr) + "?") THEN 
        RETURN.
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_ByttButNr.p", STRING(iButNr)) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    cTekst = 'Velg butikk som pakkseddlene skal overføres til.'.
    RUN velgButikk.w (INPUT cTekst, OUTPUT iButNr).
    IF iButNr = 0 THEN 
      RETURN.    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Bekreft flytting av alle pakksedler til varemottak i butikk " + STRING(iButNr) + "?") THEN 
        RETURN.
    oBrwPkSdlHode:processSet("pksdl_ByttButNr.p",STRING(iButNr)).
  END.
    
  PUBLISH 'PkSdlOpenQuery'.  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttTilOutletRecord C-Win 
PROCEDURE FlyttTilOutletRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  
  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 22 and SysGr = 5 and ParaNr = 2") THEN
     iButNr = INT(ENTRY(1,JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1"))).
  IF iButNr = 0 THEN 
    RETURN.
  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Bekreft flytting av valgte pakkseddler tilbake til Outlet?") THEN 
        RETURN.
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_ByttButNr.p", STRING(iButNr)) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Bekreft flytting av alle pakksedler tilbake til Outlet") THEN 
        RETURN.
    oBrwPkSdlHode:processSet("pksdl_ByttButNr.p",STRING(iButNr)).
  END.

  PUBLISH 'PkSdlOpenQuery'.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importerFil C-Win 
PROCEDURE importerFil :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcFilNavn AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pianLinjer AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER piType AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
  DEFINE OUTPUT PARAMETER pcMsg AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE pcLinje AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTempFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cJSonFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType1 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType2 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType3 AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType4 AS CHARACTER NO-UNDO.

  SESSION:SET-WAIT-STATE("GENERAL").
  
  ASSIGN 
    /* Pksdl eksport        */ cType1 = "Sendt dato;"
    /* Excel ark Gant */       cType2 = "PALL NR;"
    .

  rclStandardFunksjoner:konvExcel2csv( INPUT pcFilNavn,
                                       INPUT '',
                                       OUTPUT pcTempFil ).
                                       
  IF SEARCH(pcTempFil) <> ? THEN 
  IMPORTER:
  DO:
    rclStandardFunksjoner:importerImpFil( INPUT pcTempFil,
                                          INPUT 1,
                                          INPUT-OUTPUT DATASET dsttImpFil ).
/*    ASSIGN                                                                 */
/*      cJSonFil = pcTempFil                                                 */
/*      cJSonFil = REPLACE(pcTempFil,                                        */
/*                         ENTRY( NUM-ENTRIES(pcTempFil,'.') ,pcTempFil,'.'),*/
/*                         'JSon'                                            */
/*                        )                                                  */
/*      .                                                                    */
/*    IF CAN-FIND(FIRST ttImpFil) THEN                                       */
/*      DATASET dsttImpFil:WRITE-JSON("file", cJSonFil, TRUE).               */
      
    rclStandardFunksjoner:SkrivTilLogg(cLogg, 'PkSdl - Importert fil ' + pcTempFil).
    rclStandardFunksjoner:SkrivTilLogg(cLogg, 'PkSdl - JSonFil   fil ' + cJSonFil).
    
    FIND FIRST ttImpFilLinje WHERE 
      ttImpFilLinje.LinjeNr = 1 NO-ERROR.
    IF AVAILABLE ttImpFilLinje THEN 
    DO:      
      IF ttImpfilLinje.Record BEGINS cType1 THEN 
        piType = 1.
      ELSE IF ttImpfilLinje.Record BEGINS cType2 THEN 
        piType = 2.

      rclStandardFunksjoner:SkrivTilLogg(cLogg, '  Record: ' + ttImpfilLinje.Record).
      CASE pitype:
        WHEN 1 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType1: ' + cType1).
        WHEN 2 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType2: ' + cType2).
        OTHERWISE DO:
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  Ingen match på type.').
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType1: ' + cType1).
        END.
      END CASE.
    END.
    
    pianLinjer = 0.
    FOR EACH ttImpFilLinje:
      pianLinjer = pianLinjer + 1.
    END.
    
    IF CAN-DO('1,2',STRING(piType)) THEN 
      ASSIGN 
        pbOk  = TRUE 
        pcMsg = 'Fil ' + pcFilNavn + ' importert i pakkseddel register.'
        .
    ELSE 
      ASSIGN 
        pbOk  = FALSE 
        pcMsg = '** Feil format på fil. Kan ikke importeres.'
        .
  END. /* IMPORTER */
  ELSE DO:
    ASSIGN 
      pbOk  = FALSE 
      pcMsg = '** Finner ikke filen ' + pcTempFil + ' (Konvertert fra: '+ pcFilNavn + '). '.
      .
  END.
  
  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:
    DEF VAR oBrwFillInPkSdlNr AS JBoxBrowseFillIn NO-UNDO.
    DEF VAR oBrwFillInEkstId AS JBoxBrowseFillIn NO-UNDO.
    DEF VAR oBrwFillInPalleNr AS JBoxBrowseFillIn NO-UNDO.
    DEF VAR oBrwFillInLokasjon AS JBoxBrowseFillIn NO-UNDO.
    DEF VAR oBrwFillInAntall AS JBoxBrowseFillIn NO-UNDO.
    DEF VAR oBrwFillInVaretype AS JBoxBrowseFillIn NO-UNDO.

  CASE icParam:
    WHEN '10' OR WHEN '40' THEN /* 10 = "Bytte butikknr. på pakkseddel", 40 = "Marker pakkseddel som sendt Outlet" */
      DO:
        oBrwFillInPalleNr = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"cPalleNr",TRUE).
        oBrwFillInLokasjon = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"Lokasjon",TRUE).
        oBrwFillInVaretype = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"Varetype",TRUE).
        hcPalleNrColumn = oBrwPkSdlHode:getColumnHandle("cPalleNr").
        hLokasjonColumn = oBrwPkSdlHode:getColumnHandle("Lokasjon").
        hVaretypeColumn = oBrwPkSdlHode:getColumnHandle("Varetype").
      END.
    WHEN '20' THEN /* 20 = "Varemottak nettbutiks lager" */
      DO:
        oBrwFillInPkSdlNr = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"PkSdlNr",TRUE).
        oBrwFillInekstId = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"EkstId",TRUE).
        oBrwFillInPalleNr = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"cPalleNr",TRUE).
        oBrwFillInLokasjon = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"Lokasjon",TRUE).
        oBrwFillInVaretype = NEW JBoxBrowseFillIn(oBrwPkSdlHode,"Varetype",TRUE).
        oBrwFillInAntall = NEW JBoxBrowseFillIn(oBrwPkSdlLinje,"Antall",TRUE).
        hPkSdlNrColumn = oBrwPkSdlLinje:getColumnHandle("PkSdlNr").
        hEkstIdColumn = oBrwPkSdlLinje:getColumnHandle("EkstId").
        hcPalleNrColumn = oBrwPkSdlHode:getColumnHandle("cPalleNr").
        hLokasjonColumn = oBrwPkSdlHode:getColumnHandle("Lokasjon").
        hVaretypeColumn = oBrwPkSdlHode:getColumnHandle("Varetype").
        hAntallColumn = oBrwPkSdlLinje:getColumnHandle("Antall").
      END. 
    WHEN '30' THEN /* "Fakturakontroll" */ 
      DO:
      END.
  END CASE.
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
  SUBSCRIBE TO 'PkSdlOpenQuery' ANYWHERE.
  SUBSCRIBE "msgFraLagerListeButArtStr" ANYWHERE.
  SUBSCRIBE TO "OpenQueryPkSdlLinje" ANYWHERE.

  oBrwPkSdlHode = NEW JBoxBrowse(brwPkSdlHode:HANDLE). 
  oBrwPkSdlHode:setNoResizeY().
  otbPkSdlHode = NEW JBoxToolbar(tbPkSdlHode:HANDLE).
  IF CAN-DO('10,30,40',icParam) THEN 
    otbPkSdlHode:disabledTools = 'New,Delete'.
  oBrwPkSdlHode:TOOLBAR-OBJECT = otbPkSdlHode.
  oBrwPkSdlHode:customDeleteValProc = "ignore". /* Fjerner vlaidering på om det ligger Ovbuffer poster under PkSdlHode. */
  oBrwPkSdlHode:calcFieldProc = "pksdl_brwcalc.p".
  oBrwPkSdlHode:postUpdateProc = "pksdlhode_post_update.p".
  oBrwPkSdlHode:setCalcFieldParam('pksdl_OrdreTypeSkip', ENTRY(1,icParam,'|')).

/* Eksempel på en bred join og søk på felt i underliggende tabell.                                                                  */
/*  oBrwPkSdlHode:setPreScanQuery("artbas","pksdllnje of artbas no-lock,first ..").                                                 */
/* Eksempel på å dra det inn i filtere.                                                                                             */
/*  oBrwPkSdlHode:setExtraFilterField("artbas", "huvgr", "hg", "pksdllnje of artbas no-lock,first pksdlhode of pksdllinje no-lock").*/
/*  oBrwPkSdlHode:allowCan-DoFilterOperator = "huvgr".                                                                              */

  hSendtOutletColumn = oBrwPkSdlHode:getColumnHandle("SendtOutlet").
   
  oBrwPkSdlLinje = NEW JBoxBrowse(brwPkSdlLinje:HANDLE).
  oBrwPkSdlLinje:setParentBrowseObject(oBrwPkSdlHode,"PkSdlId").
  oBrwPkSdlLinje:postUpdateProc = "pksdllinje_post_update.p".
  
  otbPkSdlLinje = NEW JBoxToolbar(tbPkSdlLinje:HANDLE).  
/*  IF CAN-DO('10,30,40',icParam) THEN           */
/*    otbPkSdlLinje:disabledTools = 'New,Delete'.*/
  oBrwPkSdlLinje:TOOLBAR-OBJECT = otbPkSdlLinje.
  
  CASE icParam:
    WHEN '10' THEN 
        DO:
            otbPkSdlHode:addToolGroup("FlyttPkSdl;Bytt but.nr og flytt pakkseddel til varemottak,SkrivPkSdl;Pakkseddel,TildelPalleNr;Tildel pallenr.").
        END.
    WHEN '20' THEN 
        DO:
            otbPkSdlHode:addToolGroup("Varemottak;INNLEVER pakkseddel,FlyttTilOutlet;Flytt pakkseddel tilbake til outlet,SkrivPkSdl;Pakkseddel,TildelPalleNr;Tildel pallenr.").
/*            otbPkSdlLinje:addToolGroup("SkannRader;Skann in rader").*/
            .
        END.
    WHEN '30' THEN 
      DO:
        otbPkSdlHode:addToolGroup("SkrivFaktura;Skriv faktura,SkrivPkSdl;Pakkseddel").
      END.
    WHEN '40' THEN 
        DO:
          otbPkSdlHode:addToolGroup("MarkerSendtOutlet;Marker som sendt til Outlet" +
                        ",TildelPalleNr;Tildel pallenr." +
                        ",SkrivPkSdl;Pakkseddel" +
                        ",sokPkSdl;Pakkseddel oppslag" +
                        ",LagerlistePkSdl;Lagerliste pakksedler" +
                        ",ArtikkellistePkSdl;Artikkelliste pakksedler" +
                        ",LagerlisteOutlet;Lagerliste outlet" +
                        ",SalglisteOutlet;Salg outlet"
                        ).

          opopupPksdl = NEW JBoxPopupMenu().
          opopupPksdl:AddToolGroup('MarkerSendtOutlet;Marker som sendt Outlet' + 
                                   ',MarkerSendtOutletVestby;Bestill varer til Vestby' + 
                                   ',MarkerSendtOutletStavanger;Bestill varer til Algard' + 
                                   ',MarkerAvbestill;Avbestill varer til Outlet' 
                                   ).
          oBrwPkSdlHode:POPUP-MENU-OBJECT = opopupPksdl.
                          
        END.
  END CASE.
  
  RUN InitializeComponents.
  
  CASE icParam:       
      WHEN '10' THEN DO: /* "Bytte butikknr. på pakkseddel" */
          cbPkSdlStatus = 10.
          oBrwPkSdlHode:baseQuery  = "WHERE PkSdlStatus = 10 AND ButikkNr = 10" + cTekst.
          oBrwPkSdlHode:setQuerySort('pksdlid;DESC').
        END.
      WHEN '20' THEN DO: /* "Varemottak nettbutiks lager" */       
          cbPkSdlStatus = 10.
          oBrwPkSdlHode:baseQuery  = "WHERE PkSdlStatus = 10 AND ButikkNr = 16 ".
          oBrwPkSdlHode:setQuerySort('pksdlid;DESC').
        END. 
      WHEN '30' THEN DO: /* "Fakturakontroll" */
          cbPkSdlStatus = 20.
          oBrwPkSdlHode:baseQuery  = "WHERE PkSdlStatus = '20' AND ButikkNr = '16' AND RegistrertDato >= '01/01/2018'".
          oBrwPkSdlHode:setQuerySort('pksdlid;DESC').
        END.
      WHEN '40' THEN DO: /* "Marker pakkseddel som sendt Outlet" */
          cbPkSdlStatus = 10.
/*          oBrwPkSdlHode:baseQuery  = "WHERE PkSdlStatus = '10' AND ButikkNr = '10' ".*/
          oBrwPkSdlHode:baseQuery  = "WHERE ButikkNr = '10' ".
          oBrwPkSdlHode:setQuerySort('pksdlid;DESC').
        END.
  END CASE.   

  oBrwPkSdlHode:setReadOnlyOnReturn = TRUE.
/*  oBrwPkSdlHode:enableOnDblClick = TRUE.*/
  oBrwPkSdlLinje:setReadOnlyOnReturn = TRUE.
/*  oBrwPkSdlLinje:enableOnDblClick = TRUE.*/

  oContainer:setSplitBarY(btnSplitBarY:HANDLE).
  oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,150). /* 200 pixels from the top, 300 pixels from the bottom */
  
  oContainer:setFollowSplitBarY(STRING(BrwPkSdlHode:HANDLE) + ',' + 
                              STRING(BrwPkSdlLinje:HANDLE) + ',' +
                              STRING(first_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(prev_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(next_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(last_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(copy_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(delete_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(excel_tbPkSdlLinje:HANDLE) + ',' +
                              STRING(LeggTilVare_tbPkSdlLinje:HANDLE) + ',' +
/*                              STRING(otbPkSdlLinje:getJBoxAttribute('buttonSkannRader')) + ',' +*/
                              STRING(fiStrekkode:HANDLE)
                              ).
  
  oContainer:setNoResizeY("BrwPkSdlHode,RECT-4,RECT-5").
  oContainer:setNoResizeX("RECT-4").
  IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 3") THEN
   iNetButLager = INT(ENTRY(1,JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1"))).
      
  CASE icParam:
    WHEN '20' THEN /* Varemottak */
      DO:
        /* Nye pakkseddler kan bare opprettes i varemottaket. */
        IF otbPkSdlHode:disabledTools = '' THEN. /* Gjør ingenting */
        ELSE IF CAN-DO(otbPkSdlHode:disabledTools,'New') THEN
          otbPkSdlHode:disabledTools = REPLACE(otbPkSdlHode:disabledTools,'New','').
        ELSE  
          IF NOT CAN-DO(otbPkSdlHode:disabledTools,'New') THEN
            otbPkSdlHode:disabledTools = otbPkSdlHode:disabledTools + (IF otbPkSdlHode:disabledTools <> '' THEN ',' ELSE '') + 'New'.
      END.
  END CASE.
  oBrwPkSdlHode:setSearchField(searchPkSdlHode:HANDLE,"").
    
  DO:
    opopupLinje = NEW JBoxPopupMenu().
    opopupLinje:AddToolGroup('OppslagModell;Vis i modell liste').
    oBrwPksdlLinje:POPUP-MENU-OBJECT = opopupLinje.
  END.

  cbPkSdlStatus:SENSITIVE = FALSE.
  CASE icParam:       
      WHEN '10' THEN DO: /* "Bytte butikknr. på pakkseddel" */
          cbPkSdlStatus:SCREEN-VALUE = STRING(cbPkSdlStatus).
        END.
      WHEN '20' THEN DO: /* "Varemottak nettbutiks lager" */
          cbPkSdlStatus:SCREEN-VALUE = STRING(cbPkSdlStatus).
        END. 
      WHEN '30' THEN DO: /* "Fakturakontroll" */
          cbPkSdlStatus:SCREEN-VALUE = STRING(cbPkSdlStatus).
        END.
      WHEN '40' THEN DO: /* "Marker pakkseddel som sendt Outlet" */
          cbPkSdlStatus:SCREEN-VALUE = STRING(cbPkSdlStatus).
          cbPkSdlStatus:SENSITIVE = TRUE.
        END.
  END CASE.   
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerlisteOutletRecord C-Win 
PROCEDURE LagerlisteOutletRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEFINE VARIABLE pcRapport AS CHARACTER NO-UNDO.
    
  EMPTY TEMP-TABLE ttLagerListe.

  IF NOT JBoxSession:Instance:ViewQuestionOkCancel('Skal lagerliste for Outlet butikkene skrives ut?') THEN 
    RETURN.

  pcRapport = rclStandardFunksjoner:getTmpFileName().
  ENTRY(NUM-ENTRIES(pcRapport,'.'),pcRapport,'.') = 'csv'.

  hLagerliste = BUFFER ttLagerListe:HANDLE.
  SESSION:SET-WAIT-STATE("GENERAL").
  IF JBoxServerAPI:Instance:CallServerProc("pksdl_lagerOutlet.p",cAnv-KodIdList + '@' + cKategoriIdList + '@' + cButikkIdList,hLagerListe) THEN
    hLagerListe = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
  
  IF CAN-FIND(FIRST ttLagerListe) THEN 
  OUTLETTLISTE:
  DO:  
    OUTPUT TO VALUE(pcRapport).
      PUT UNFORMATTED  
        'ArtikkelNr;'  
        'Varetekst;'
        'LevKod;'
        'LevFargKod;'
        'Sesong;'
        'MainGroup;'
        'MainGrpTekst;'
        'ArtGroup;'
        'ArtGrpTekst;'
        'VVarekostL10;'
        'VerdiWholeSale;'
        'AntL10;'        
        'VerdiL10;'
        'VerdiWholeSale10;'
        'VVarekostL40;'
        'AntL40;'
        'VerdiL40;'
        'VerdiWholeSale40;'
        'TotAnt;'
        'TotVerdi;'
        'TotVerdiWholeSale;'
        'LC;'
        'VerdiLCL10;'
        'VerdiLCL40'
      SKIP.
    FOR EACH ttLagerListe:
      PUT UNFORMATTED
        ttLagerListe.ArtikkelNr ';'  
        ttLagerListe.Varetekst ';'
        ttLagerListe.LevKod ';'
        ttLagerListe.LevFargKod ';'
        ttLagerliste.Sesong ';'
        ttLagerListe.MainGroup ';'
        ttLagerListe.MainGrpTekst ';'
        ttLagerListe.ArtGroup ';'
        ttLagerListe.ArtGrpTekst ';'
        ttLagerListe.VVarekostL10 ';'
        ttLagerListe.VerdiWholeSale ';'
        ttLagerListe.AntL10 ';'
        ttLagerListe.VerdiL10 ';'
        ttLagerListe.VerdiWholeSale10 ';'
        ttLagerListe.VVarekostL40 ';'
        ttLagerListe.AntL40 ';'
        ttLagerListe.VerdiL40 ';'
        ttLagerListe.VerdiWholeSale40 ';'
        ttLagerListe.TotAnt ';'
        ttLagerListe.TotVerdi ';'
        ttLagerliste.TotVerdiWholeSale ';'
        ttLagerListe.LC ';'
        ttLagerListe.VerdiLCL10 ';'
        ttLagerListe.VerdiLCL40
      SKIP.
    END.
    OUTPUT CLOSE. 
    RUN OpenExcelDocument (pcRapport,'').
    EMPTY TEMP-TABLE ttLagerListe.
  END. /* OUTLETTLISTE*/
  ELSE 
    JBoxSession:Instance:ViewMessage("Fant ingen data.").
END. /* FRAME */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerlistePkSdlRecord C-Win 
PROCEDURE LagerlistePkSdlRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEFINE VARIABLE piValg       AS INTEGER NO-UNDO.
  DEFINE VARIABLE pcRapport    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcLagerliste AS CHARACTER NO-UNDO.

  EMPTY TEMP-TABLE ttLagerListe.
  
  ASSIGN 
    pcRapport    = rclStandardFunksjoner:getTmpFileName()
    pcLagerListe = 'konv\ttLagerListe' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.json'
    .
  ENTRY(NUM-ENTRIES(pcRapport,'.'),pcRapport,'.') = 'csv'.
  
  /* Kaller her programmet som legger ut en json fil med liste over artikkel og antall som ligger på pakkseddlene. */
  /* Filen de lagrer data i er pcLagerListe.                                                                       */
  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Eksport av lageliste for markerte pakkseddler?") THEN 
        RETURN.
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_Lagerliste.p",pcLagerListe + '@' + cAnv-KodIdList + '@' + cKategoriIdList + '@' + cButikkIdList) THEN
        JBoxSession:Instance:ViewMessage("Feil ved bygging av lagerliste " + JBoxServerAPI:Instance:getCallMessage()). 
    ELSE DO:
      cLagerliste = JBoxServerAPI:Instance:getCallMessage().
      piValg = 1.
    END.  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Eksport av lageliste for ALLE pakkseddler i spørring?") THEN 
        RETURN.
    oBrwPkSdlHode:processSet("pksdl_Lagerliste.p",pcLagerListe + '@' + cAnv-KodIdList + '@' + cKategoriIdList + '@' + cButikkIdList).
    cLagerliste = JBoxServerAPI:Instance:getCallMessage().
  END.  

  /* Henter JSOn filen og legger den opp i en temp tabell som så returneres. */
  hLagerliste = BUFFER ttLagerListe:HANDLE.
  SESSION:SET-WAIT-STATE("GENERAL").
  IF JBoxServerAPI:Instance:CallServerProc("pksdl_lagerPksdl.p",cLagerliste,hLagerListe) THEN
    hLagerListe = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
    
  /* Eksporterer data hvis det ble funnet noe. */  
  IF CAN-FIND(FIRST ttLagerListe) THEN 
  DO:  
    OUTPUT TO VALUE(pcRapport).
      PUT UNFORMATTED  
        'ArtikkelNr;'  
        'Varetekst;'
        'LevKod;'
        'LevFargKod;'
        'Sesong;'
        'MainGroup;'
        'MainGrpTekst;'
        'ArtGroup;'
        'ArtGrpTekst;'
        'PkSdlNr;'
        'SendtDato;'
        'Innlevert;'
        'Opphav;'
        'SO;'
        'PalleNr;'
        'Lokasjon;'
        'Varetype;'
        'LSesong;'
        'Varekost;'
        'WholeSalePris;'
        'AntPkSdl;'
        'VerdiPkSdl;'
        'VerdiWholeSale;'
        'LC;'
        'VerdiLC'
      SKIP.
    FOR EACH ttLagerListe:
      PUT UNFORMATTED
        ttLagerListe.ArtikkelNr ';'  
        ttLagerListe.Varetekst ';'
        ttLagerListe.LevKod ';'
        ttLagerListe.LevFargKod ';'
        ttLagerliste.Sesong ';'
        ttLagerListe.MainGroup ';'
        ttLagerListe.MainGrpTekst ';'
        ttLagerListe.ArtGroup ';'
        ttLagerListe.ArtGrpTekst ';'
        ttLagerListe.PkSdlNr ';'
        ttLagerListe.SendtDato ';'
        ttLagerListe.Innlevert ';'
        ttLagerliste.PkSdlOpphav ';'
        ttLagerListe.SO ';'
        ttLagerListe.cPalleNr ';'
        ttLagerListe.Lokasjon ';'
        ttLagerListe.Varetype ';'
        ttLagerListe.LagerSesong ';'
        ttLagerListe.InnkjopsPris ';'
        ttLagerListe.WholeSalePris ';'
        ttLagerListe.AntPkSdl ';'
        ttLagerListe.VerdiPkSdl ';'
        ttLagerListe.VerdiWholeSale ';'
        ttLagerListe.LC ';'
        ttLagerListe.VerdiLC
      SKIP.
    END.
    OUTPUT CLOSE. 
/*    JBoxSession:Instance:ViewMessage("Lageliste lagre i fil konv\totlagerliste.json.").*/
    RUN OpenExcelDocument (pcRapport,'').
    EMPTY TEMP-TABLE ttLagerListe.
  END.
  ELSE 
    JBoxSession:Instance:ViewMessage("Fant ingen data.").
END. /* FRAME */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggTilVareRecord C-Win 
PROCEDURE LeggTilVareRecord :
IF AVAILABLE PkSdlHode THEN
  DO:
    IF PkSdlHode.PkSdlStatus <> 10 THEN
      DO:
        JBoxSession:Instance:ViewMessage("Pakkseddelen er oppdatert. Kan ikke endres.").
        RETURN NO-APPLY.
      END.
      
    IF otbPkSdlLinje:isCurrent AND AVAILABLE PkSdlHode THEN 
    DO:

      RAD: 
      DO:
        RUN LagerListeButArtStr.w ('16|60|' + STRING(PkSdlHode.PkSdlId) + '|' + STRING(PkSdlHode.LevNr)).
        oBrwPkSdlLinje:openQuery().
      END. /* RAD */
    END.  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MarkerAvbestillRecord C-Win 
PROCEDURE MarkerAvbestillRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Linjen(e) med den/de pakkseddler skal avbestilles, må markeres ").
      RETURN.
    END.

  IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal markerte ordre avbestilles?") THEN 
      RETURN.

  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",'1|AVBESTILL') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE DO:
        oBrwPkSdlHode:refreshRow().
    END.    
  END.
  ELSE IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",'1|AVBESTILL') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE 
        oBrwPkSdlHode:REFRESH().    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MarkerSendtOutletRecord C-Win 
PROCEDURE MarkerSendtOutletRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  RUN velgOutlet.w (OUTPUT cTekst).
  IF cTekst = ? OR cTekst = '' THEN 
    RETURN.
  
  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",cTekst) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE DO:
        oBrwPkSdlHode:refreshRow().
    END.    
  END.
  ELSE IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",cTekst) THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE 
        oBrwPkSdlHode:REFRESH().    
  END.
  ELSE DO: 
      JBoxSession:Instance:ViewMessage("Linjen(e) med den/de pakkseddler som hvor dato/tid skal settes, må markeres ").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MarkerSendtOutletStavangerRecord C-Win 
PROCEDURE MarkerSendtOutletStavangerRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  IF NOT oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Linjen(e) med den/de pakkseddler som skal settes i bestilling, må markeres ").
      RETURN.
    END.

  IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal markerte ordre settes i bestilling?") THEN 
      RETURN.

  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",'11') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE DO:
        oBrwPkSdlHode:refreshRow().
    END.    
  END.
  ELSE IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",'11') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE 
        oBrwPkSdlHode:REFRESH().    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MarkerSendtOutletVestbyRecord C-Win 
PROCEDURE MarkerSendtOutletVestbyRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  IF NOT oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN 
    DO: 
        JBoxSession:Instance:ViewMessage("Linjen(e) med den/de pakkseddler som skal settes i bestilling, må markeres ").
      RETURN.
    END.

  IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal markerte ordre settes i bestilling?") THEN 
      RETURN.

  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",'10') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE DO:
        oBrwPkSdlHode:refreshRow().
    END.    
  END.
  ELSE IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS > 1 THEN
  DO:
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_SendtOutlet.p",'10') THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
    ELSE 
        oBrwPkSdlHode:REFRESH().    
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
    cVareFelt = icVareFelt
    cVareVerdier = icVareVerdier
    . 

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
DEF INPUT  PARAM ihFillIn AS HANDLE NO-UNDO.
DEF INPUT  PARAM ihBuffer AS HANDLE NO-UNDO.
DEF OUTPUT PARAM obOk     AS LOG    NO-UNDO.

  CASE icParam:
    WHEN '10' OR WHEN '20' OR WHEN '40' THEN 
        DO:
          obOK = DYNAMIC-FUNCTION("DoUpdate",ihBuffer:NAME,"",
                      "",
                      ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE,
                      DYNAMIC-FUNCTION("getAttribute",ihFillIn,"buffercolumn"),
                      ihFillIn:SCREEN-VALUE,
                      TRUE).
                      
          IF ihBuffer:NAME = 'PkSdlLinje' THEN 
            JBoxServerApi:Instance:Update("PkSdlLinje",
                                          PkSdlLinje.RowIdent1,
                                          'Antall,AntLevert',
                                          STRING(ihFillIn:INPUT-VALUE) + '|' + STRING(ihFillIn:INPUT-VALUE),
                                          FALSE,
                                          "pksdllinje_post_update.p",
                                          TRUE).
        END.
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
DO WITH FRAME {&FRAME-NAME}:
    /* Ny hode. */
    IF otbPkSdlHode:isCurrent THEN 
    DO:
    END.
    /* Ny rad. */
    ELSE IF otbPkSdlLinje:isCurrent THEN 
    DO:
      IF AVAILABLE PkSdlHode THEN 
      DO:
        IF PkSdlHode.pksdlstatus = 20 THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Pakkseddelen er innlevert. Nye rader kan ikke registreres. ").
          RETURN.
        END.
        IF PkSdlHode.PkSdlOpphav <> 8 THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Pakkseddelen er ikke opprettet fra eCom lager. Kan ikke redigeres. ").
          RETURN.
        END.
      END.
    END.
  END.
  
  RUN SUPER.

  DO WITH FRAME {&FRAME-NAME}:
    
    /* Ny hode. */
    IF otbPkSdlHode:isCurrent THEN
    DO:
      IF AVAILABLE PkSdlHode THEN 
      DO:
        RUN velgLeverandor.w (OUTPUT cTekst).
        IF cTekst = ? OR cTekst = '' OR INT(cTekst) = 0 THEN
        DO:
          iLevNr = 0.
          IF NOT JBoxServerApi:Instance:Update("PkSdlHode",
                                  PkSdlHode.RowIdent1,
                                  "LevNr",
                                  STRING(iLevNr),
                                  FALSE,
                                  "pksdl_post_delete.p",
                                  TRUE) THEN
          DO:
              JBoxSession:Instance:ViewMessage("Feil ved sletting av pakkseddel pga. " + JBoxServerAPI:Instance:getCallMessage()).
              RETURN.
          END.
          oBrwPkSdlHode:OpenQuery().
        END.
        ELSE DO: 
          iLevNr = INT(cTekst).
          IF NOT JBoxServerApi:Instance:Update("PkSdlHode",
                                      PkSdlHode.RowIdent1,
                                      "LevNr,SendtDato,Leveringsdato",
                                      STRING(iLevNr) + '|' + STRING(TODAY) + '|' + STRING(TODAY),
                                      FALSE,
                                      "pksdlhode_post_update.p",
                                      TRUE) THEN
          DO:
              JBoxSession:Instance:ViewMessage("Feil ved opprettelse av rad pga " + JBoxServerAPI:Instance:getCallMessage()).
              RETURN.
          END.
        END.
        oBrwPkSdlHode:OpenQuery().
      END.
    END.
    /* Ny rad. */
    ELSE IF otbPkSdlLinje:isCurrent THEN
    NyRAD: 
    DO:
      IF AVAILABLE PkSdlHode THEN 
      DO:
        CURRENT-WINDOW:SENSITIVE = FALSE.
        ASSIGN 
          cVareFelt    = ''
          cVareVerdier = ''
          .
        RUN LagerListeButArtStr.w ('16|40||||' + STRING(PkSdlHode.LevNr)).
        CURRENT-WINDOW:SENSITIVE = TRUE.
        IF cVareFelt = '' OR cVarefelt = ? THEN 
        DO:
          IF NOT JBoxServerApi:Instance:Update("PkSdlLinje",
                                        PkSdlLinje.RowIdent1,
                                        'Antall',
                                        '',
                                        FALSE,
                                        "pksdllinje_post_delete.p",
                                        TRUE) THEN
/*          IF NOT JBoxServerApi:Instance:Delete("PkSdlLinje", PkSdlLinje.RowIdent1) THEN*/
            DO:
                JBoxSession:Instance:ViewMessage("Feil ved sletting av rad pga " + JBoxServerAPI:Instance:getCallMessage()).
                RETURN.
            END.
          oBrwPkSdlLinje:OpenQuery().
        END. 
        ELSE DO: 
          IF NOT JBoxServerApi:Instance:Update("PkSdlLinje",
                                      PkSdlLinje.RowIdent1,
                                      'Antall,AntLevert,ButikkNr,' + cVareFelt,
                                      '1|1|16|' + cVareVerdier,
                                      FALSE,
                                      "pksdllinje_post_update.p",
                                      TRUE) THEN
          DO:
              JBoxSession:Instance:ViewMessage("Feil ved opprettelse av rad pga " + JBoxServerAPI:Instance:getCallMessage()).
              RETURN.
          END.
          oBrwPkSdlLinje:refreshRow().
        END.
      END.
    END. /* NyRAD */
  END.
  
  IF AVAILABLE PkSdlHode THEN 
  DO:
    JBoxServerApi:Instance:CallServerProc("PkSdlSetLandedCost.p", STRING(PkSdlHode.PkSdlId)).
    oBrwPkSdlHode:refreshRow().
  END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenExcelDocument C-Win 
PROCEDURE OpenExcelDocument :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipFilnavn AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipKatalog AS CHARACTER NO-UNDO.

  DEF VAR hInstance   AS INT NO-UNDO.
    
  RUN ShellExecute{&A} IN hpApi(0,
                                "open",
                                "Excel.exe",
                                ipFilnavn,
                                ipKatalog,
                                1,
                                OUTPUT hInstance).
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryPkSdlLinje C-Win 
PROCEDURE OpenQueryPkSdlLinje :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    oBrwPkSdlLinje:OpenQuery().

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
    IF AVAILABLE PksdlLinje THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',PkSdlLinje.LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',PkSdlLinje.LevFargKod)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PkSdlOpenQuery C-Win 
PROCEDURE PkSdlOpenQuery :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    oBrwPkSdlHode:OpenQuery().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RegistrerLinjerRecord C-Win 
PROCEDURE RegistrerLinjerRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.

  DO WITH FRAME {&FRAME-NAME}: 
    IF AVAILABLE PkSdlHode THEN
    DO:
        IF PkSdlHode.SendtOutlet = 1 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 11.
        ELSE IF PkSdlHode.SendtOutlet = 2 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 12.
        ELSE IF PkSdlHode.SendtOutlet = 3 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 14.
        ELSE IF PkSdlHode.SendtOutlet = 4 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 10.
        ELSE IF PkSdlHode.SendtOutlet = 5 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 13.
        ELSE IF PkSdlHode.SendtOutlet = 6 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 8.

        ELSE IF PkSdlHode.SendtOutlet = 10 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 2.
        ELSE IF PkSdlHode.SendtOutlet = 11 AND PkSdlHode.SendtFraLagerTilOutlet <> ? THEN
          hSendtOutletColumn:BGCOLOR = 4.

        ELSE IF PkSdlHode.SendtOutlet = 0 AND PkSdlHode.SendtFraLagerTilOutlet = ? THEN
          hSendtOutletColumn:BGCOLOR = ?.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowsToBatchRecord C-Win 
PROCEDURE rowsToBatchRecord :
RUN SUPER.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SalglisteOutletRecord C-Win 
PROCEDURE SalglisteOutletRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  DEFINE VARIABLE pcRapport AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pdFraDato AS DATE NO-UNDO.
  DEFINE VARIABLE pdTilDato AS DATE NO-UNDO.
    
  EMPTY TEMP-TABLE ttLagerListe.


  ASSIGN 
    pdFraDato = DATE(1,1,YEAR(TODAY))
    pdTilDato = TODAY
    .

  IF NOT JBoxSession:Instance:ViewQuestionOkCancel('Skal salgsliste for Outlet butikkene for perioden ' + STRING(pdFraDato) + ' til ' + STRING(pdTilDato) + ' skrives ut?') THEN 
    RETURN.

  pcRapport = rclStandardFunksjoner:getTmpFileName().
  ENTRY(NUM-ENTRIES(pcRapport,'.'),pcRapport,'.') = 'csv'.

  hLagerliste = BUFFER ttLagerListe:HANDLE.
  SESSION:SET-WAIT-STATE("GENERAL").
  IF JBoxServerAPI:Instance:CallServerProc("pksdl_salgOutlet.p",STRING(pdFraDato) + '|' + STRING(pdTilDato) + '@' + cAnv-KodIdList + '@' + cKategoriIdList,hLagerListe) THEN
    hLagerListe = JBoxServerAPI:Instance:getCallReturnTable().
  SESSION:SET-WAIT-STATE("").
  
  IF CAN-FIND(FIRST ttLagerListe) THEN 
  OUTLETTLISTE:
  DO:  
    OUTPUT TO VALUE(pcRapport).
      PUT UNFORMATTED  
        'ArtikkelNr;'  
        'Varetekst;'
        'LevKod;'
        'LevFargKod;'
        'Sesong;'
        'MainGroup;'
        'MainGrpTekst;'
        'ArtGroup;'
        'ArtGrpTekst;'
        'VVarekostL10;'
        'AntSolgtL10;'        
        'VerdiSolgtL10;'
        'VVarekostL40;'
        'AntSolgtL40;'
        'VerdiSolgtL40;'
        'TotAntSolgt;'
        'TotVerdiSolgt;'
        'LC;'
        'VerdiSolgtLCL10;'
        'VerdiSolgtLCL40'
      SKIP.
    FOR EACH ttLagerListe:
      PUT UNFORMATTED
        ttLagerListe.ArtikkelNr ';'  
        ttLagerListe.Varetekst ';'
        ttLagerListe.LevKod ';'
        ttLagerListe.LevFargKod ';'
        ttLagerliste.Sesong ';'
        ttLagerListe.MainGroup ';'
        ttLagerListe.MainGrpTekst ';'
        ttLagerListe.ArtGroup ';'
        ttLagerListe.ArtGrpTekst ';'
        ttLagerListe.VVarekostL10 ';'
        ttLagerListe.AntL10 ';'
        ttLagerListe.VerdiL10 ';'
        ttLagerListe.VVarekostL40 ';'
        ttLagerListe.AntL40 ';'
        ttLagerListe.VerdiL40 ';'
        ttLagerListe.TotAnt ';'
        ttLagerListe.TotVerdi ';'
        ttLagerListe.LC ';'
        ttLagerListe.VerdiLCL10 ';'
        ttLagerListe.VerdiLCL40
      SKIP.
    END.
    OUTPUT CLOSE. 
    RUN OpenExcelDocument (pcRapport,'').
    EMPTY TEMP-TABLE ttLagerListe.
  END. /* OUTLETTLISTE*/
  ELSE 
    JBoxSession:Instance:ViewMessage("Fant ingen data.").
END. /* FRAME */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
DO WITH FRAME {&FRAME-NAME}:
  
    RETURN.
    
/*    /* Ny hode. */                      */
/*    IF otbPkSdlHode:isCurrent THEN      */
/*    DO:                                 */
/*    END.                                */
/*    /* Ny rad. */                       */
/*    ELSE IF otbPkSdlLinje:isCurrent THEN*/
/*    DO:                                 */
/*    END.                                */
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    /* Ny hode. */
    IF otbPkSdlHode:isCurrent THEN 
    DO:
    END.
    /* Ny rad. */
    ELSE IF otbPkSdlLinje:isCurrent THEN 
    DO:
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
        
    /* Sender med parameter verdi inn i icParam i pksdl_brwcalc.p (Andre input parameteren. */    
    oBrwPkSdlHode:setCalcFieldParam("pksdl_SkipFraButikk", REPLACE(cButikkIDList,'|',CHR(1))).
    oBrwPkSdlHode:setCalcFieldParam("pksdl_SkipSendtOutlet", REPLACE(cSOIDList,'|',CHR(1))).

    IF cKategoriIdList = "" AND cAnv-KodIdList = "" THEN
      oBrwPkSdlHode:preScanBaseQuery = "".
    ELSE DO:    
      IF cKategoriIdList NE "" THEN
        oBrwPkSdlHode:preScanBaseQuery = "LOOKUP(STRING(ArtBas.HovedKatNr),'" + REPLACE(cKategoriIdList,"|",",") + "') > 0 ".

      IF cAnv-KodIdList NE "" THEN
        oBrwPkSdlHode:preScanBaseQuery = oBrwPkSdlHode:preScanBaseQuery
                                       + (IF oBrwPkSdlHode:preScanBaseQuery NE "" THEN " AND " ELSE "") 
                                       + "LOOKUP(STRING(ArtBas.anv-id),'" + REPLACE(cAnv-KodIdList,"|",",") + "') > 0 ".

      IF oBrwPkSdlHode:preScanBaseQuery NE "" THEN
        oBrwPkSdlHode:preScanBaseQuery = oBrwPkSdlHode:preScanBaseQuery  
                                       + ",EACH PkSdlLinje NO-LOCK WHERE PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr,FIRST PkSdlHode NO-LOCK OF PkSdlLinje".

      IF oBrwPkSdlHode:preScanBaseQuery NE "" THEN 
        oBrwPkSdlHode:preScanBaseQuery = "ArtBas WHERE " + oBrwPkSdlHode:preScanBaseQuery.                                 
    END.                                        

    /* Det er bare ved 'Pakkseddel sendt Outlet' at dette skal kunne justeres. */
    IF icParam = '40' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'PkSdlStatus'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + cbPkSdlStatus:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + '='
        .

    IF fiPkSdlNr:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'PkSdlNr'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiPkSdlNr:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .

    IF fiPalleNr:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'cPalleNr'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiPalleNr:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .

    IF fiSesong:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'LagerSesong'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiSesong:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .
    IF fiLokasjon:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'Lokasjon'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiLokasjon:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .

    IF fiVaretype:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'Varetype'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + fiVaretype:SCREEN-VALUE
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'BEGINS'
        .

    IF fiArtGroup:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'pksdl_ArtGroup'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiArtGroup:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .

    IF fiMainGroup:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'pksdl_MainGroup'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiMainGroup:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .

/*    IF cbSendtOutlet:SCREEN-VALUE <> '' THEN                                                */
/*      ASSIGN                                                                                */
/*        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'SendtOutlet'           */
/*        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + cbSendtOutlet:SCREEN-VALUE*/
/*        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'EQ'                */
/*        .                                                                                   */

    IF fiLevKod:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'pksdl_LevKod'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiLevKod:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .

    IF fiLevFargKod:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'pksdl_LevFargKod'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiLevFargKod:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .

    IF fiStorl:SCREEN-VALUE <> '' THEN 
      ASSIGN 
        cFields   = cFields + (IF cFields <> '' THEN ',' ELSE '') + 'pksdl_Storl'  
        cWhere    = cWhere + (IF cWhere <> '' THEN '|' ELSE '') + '*' + fiStorl:SCREEN-VALUE + '*'
        cOperator = cOperator + (IF cOperator <> '' THEN ',' ELSE '') + 'MATCHES'
        .

/*MESSAGE                */
/*'cFields:' cFields SKIP*/
/*'cWhere:' cWhere SKIP  */
/*'cOperator:' cOperator */
/*VIEW-AS ALERT-BOX.     */
    
    oBrwPkSdlHode:setFilter(cFields,cOperator,cWhere).
    oBrwPkSdlHode:openQuery(). 
    DO  WITH FRAME {&FRAME-NAME}:
      ASSIGN 
        fiAntPkSdl:SCREEN-VALUE       = ''
        fiVerdiPkSdl:SCREEN-VALUE     = ''
        fiLCVerdi:SCREEN-VALUE        = ''
        fiWholeSalePkSdl:SCREEN-VALUE = ''
        .
    END.         
    
  END.

/*CASE icParam:                                                       */
/*  WHEN '10' THEN oBrwPkSdlHode:setFilter('pksdl_butlst', '=', '10').*/
/*  WHEN '20' THEN oBrwPkSdlHode:setFilter('pksdl_butlst', '=', '16').*/
/*  WHEN '30' THEN oBrwPkSdlHode:setFilter('pksdl_butlst', '=', '16').*/
/*END CASE.                                                           */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skipInitJukeBox C-Win 
PROCEDURE skipInitJukeBox :
/*------------------------------------------------------------------------------
 Purpose: Underliggende rutiner får via denne funksjonen beskjed om å kippe init 
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pbSkipJBoxInit AS LOG NO-UNDO.
  
  ASSIGN 
    pbSkipJBoxInit = TRUE 
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivFakturaRecord C-Win 
PROCEDURE skrivFakturaRecord :
IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF AVAILABLE PkSdlHode AND PkSdlHode.FakturaNr = ? THEN 
      DO:
        JBoxSession:Instance:ViewMessage("Pakkseddel er ikke koblet mot faktura.").
        RETURN.
      END.
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv ut faktura for valgt(e) poster(er)?") THEN 
        RETURN.
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdlhode_skrivfaktura.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv ut faktura for valgt pakkseddel?") THEN 
        RETURN.
    oBrwPkSdlHode:processSet("pksdlhode_skrivfaktura.p","").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivPkSdlRecord C-Win 
PROCEDURE skrivPkSdlRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
  DO:    
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv ut pakkseddel for valgt(e) poster(er)?") THEN 
        RETURN.
    IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdlhode_skrivpakkseddel.p", "") THEN
        JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).  
  END.
  ELSE DO: 
    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skriv ut pakkseddel for valgt pakkseddel?") THEN 
        RETURN.
    oBrwPkSdlHode:processSet("pksdlhode_skrivpakkseddel.p","").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokPkSdlRecord C-Win 
PROCEDURE sokPkSdlRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  RUN visPakkseddel.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TildelPalleNrRecord C-Win 
PROCEDURE TildelPalleNrRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  
  CASE icParam:
  WHEN '10' OR WHEN '20' OR WHEN '40' THEN 
        DO:
          IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
          DO:    
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal nytt pallenr tildeles pakkseddel?") THEN 
                RETURN.
            IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_tildelPalleNr.p", "") THEN
                JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
            ELSE DO:
/*                oBrwPkSdlHode:refreshSelectedRows().*/
                oBrwPkSdlHode:refresh().
                RUN DisplayRecord.
              END. 
          END.
          ELSE DO: 
            JBoxSession:Instance:ViewMessage("Marker radene som skal tildeles pallenr.").
            RETURN.  
          END.
        END.
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VaremottakRecord C-Win 
PROCEDURE VaremottakRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pbEtikett AS LOG NO-UNDO.
  
  IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
      DO:    
          IF oBrwPkSdlHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN 
          DO:
            IF AVAILABLE PkSdlHode THEN 
              DO:
                IF PkSdlHode.PkSdlNr = '' THEN 
                  DO:
                    JBoxSession:Instance:ViewMessage("Pakkseddelen mangler pakkseddelnr. Kan ikke innleveres.").
                    RETURN.
                  END.
                IF PkSdlHode.PkSdlStatus <> 10 THEN 
                  DO:
                    IF PkSdlHode.PkSdlStatus = 20 THEN 
                      JBoxSession:Instance:ViewMessage("Pakkseddelen er allerede innlevert.").
                    ELSE 
                      JBoxSession:Instance:ViewMessage("Pakkseddelen har feil status. Kan ikke innleveres.").
                    RETURN.
                  END.
              END.
              
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Innlever pakkseddel for valgt post?") THEN 
                RETURN.
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal etiketter skrives ut?") THEN
              pbEtikett = TRUE.
            ELSE 
              pbEtikett = FALSE. 
            IF NOT oBrwPkSdlHode:processRowsNoMessage("pksdl_varemottak.p", cLogg + '|' + STRING(pbEtikett)) THEN
                JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
            oBrwPkSdlHode:refreshRow().
          END.            
          ELSE DO:
            JBoxSession:Instance:ViewMessage('Bare en pakkseddel kan leveres inn pr. gang.').
            RETURN.     
          END.
      END.
  ELSE DO:
      JBoxSession:Instance:ViewMessage('Marker den pakkseddel som skal innleveres.').
      RETURN.     
  /*    IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Innlever alle pakkseddler?") THEN*/
  /*        RETURN.                                                                        */
  /*    oBrwPkSdlHode:processSet("pksdl_varemottak.p",cLogg).                        */
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

