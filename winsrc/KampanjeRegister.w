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
&SCOPED-DEFINE AdvGuiWin  

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF    VAR      bOk                             AS LOG                              NO-UNDO.
DEFINE VARIABLE cMsg                            AS CHARACTER                        NO-UNDO.
DEF    VAR      ix                              AS INT                              NO-UNDO.
DEF    VAR      hBrowse                         AS HANDLE                           NO-UNDO.
DEF    VAR      hQuery                          AS HANDLE                           NO-UNDO.
DEF    VAR      hToolbar                        AS HANDLE                           NO-UNDO.
DEF    VAR      hFieldMap                       AS HANDLE                           NO-UNDO.
DEF    VAR      oContainer                      AS JBoxContainer                    NO-UNDO.
DEFINE VARIABLE cLogg                           AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE oStartDato_JBoxDevExEdit        AS JBoxDevExDateEdit                NO-UNDO.
DEFINE VARIABLE oSluttDato_JBoxDevExEdit        AS JBoxDevExDateEdit                NO-UNDO.
DEFINE VARIABLE iAktiveresTid                   AS INTEGER                          NO-UNDO.
DEFINE VARIABLE iGyldigTilTid                   AS INTEGER                          NO-UNDO.
DEFINE VARIABLE lKamp%                          AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE hAktivertColumn                 AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hKampanjehode_AktivTilbudColumn AS HANDLE                           NO-UNDO.
DEFINE VARIABLE iProfilNr                       AS INTEGER                          NO-UNDO.
DEFINE VARIABLE iButNr                          AS INTEGER                          NO-UNDO.
DEFINE VARIABLE cVareFelt                       AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cVareVerdier                    AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE pcTekst                         AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE plRab%                          AS DECIMAL                          FORMAT "->>9.9" NO-UNDO.
DEFINE VARIABLE iKampanjeId                     AS INTEGER                          NO-UNDO.
DEFINE VARIABLE cBeskrivelse                    AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE dStartDato                      AS DATE                             NO-UNDO.
DEFINE VARIABLE dSluttDato                      AS DATE                             NO-UNDO.
DEFINE VARIABLE lKampanjePris AS DECIMAL NO-UNDO.
DEFINE VARIABLE lKroneRabatt AS DECIMAL NO-UNDO.
DEFINE VARIABLE lMinstepris AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAvslagType AS INTEGER NO-UNDO.
DEFINE VARIABLE cNotat                          AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE iAntLinjer                      AS INTEGER                          NO-UNDO.
DEFINE VARIABLE httImpFilLinje                  AS HANDLE                           NO-UNDO.
DEFINE VARIABLE iType                           AS INTEGER                          NO-UNDO.
DEFINE VARIABLE hKampanjeLinje_NOSColumn        AS HANDLE                           NO-UNDO.
DEFINE VARIABLE opopupKampanjeLinje             AS JBoxPopupMenu                    NO-UNDO.
DEFINE VARIABLE cFilterTekst                    AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE bIgnorerNOS                     AS LOG                              NO-UNDO.
DEFINE VARIABLE pcRetParam                      AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE pbaktive                        AS LOG                              NO-UNDO.
DEFINE VARIABLE lOrgKamp%                       AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE lOrgKampanjePris AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE lOrgKroneRabatt                       AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE cRowIdList AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIdList    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE bNew AS LOG NO-UNDO.
DEFINE VARIABLE bEndreAktivTilbud AS LOG NO-UNDO.  
DEFINE VARIABLE rModellListeHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE rLagerSalgHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE rKampanjeSalg AS HANDLE NO-UNDO.
DEFINE VARIABLE cProfilLst AS CHARACTER NO-UNDO.

DEFINE VARIABLE rclStandardFunksjoner           AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

ASSIGN 
  cLogg = 'KampanjeRegister' + REPLACE(STRING(TODAY),'/','') 
  .
  
rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner( cLogg ) NO-ERROR.

{ cls\StdFunk\dsttImpFil.i }

/*** Start instance property definitions for JBoxBrowse object oBrwKampanjeHode ***/
DEF VAR oBrwKampanjeHode AS JBoxBrowse NO-UNDO.
DEF TEMP-TABLE KampanjeHode
    FIELD KampanjeId AS INTEGER
    FIELD Kampanjehode_AvslagType AS CHARACTER
    FIELD Kampanjehode_AktivTilbud AS INTEGER
    FIELD Aktivert AS LOGICAL
    FIELD IgnorerNOS AS LOGICAL
    FIELD Beskrivelse AS CHARACTER
    FIELD Kampanjehode_AntallLinjer AS CHARACTER
    FIELD fiKamp% AS DECIMAL
    FIELD Kampanjehode_DatoTidAktiveres AS CHARACTER
    FIELD Kampanjehode_DatoTidSlutt AS CHARACTER
    FIELD Notat AS CHARACTER
    FIELD ProfilNr AS INTEGER
    FIELD BrukerID AS CHARACTER
    FIELD RegistrertAv AS CHARACTER
    FIELD StartDato AS DATE
    FIELD SluttDato AS DATE
    FIELD GyldigTilTid AS INTEGER
    FIELD KampanjePris AS DECIMAL
    FIELD AktiveresTid AS INTEGER
    FIELD Kamp% AS DECIMAL
    FIELD EDato AS DATE
    FIELD ETid AS INTEGER
    FIELD AvslagType AS INTEGER
    FIELD Komplett AS LOGICAL
    FIELD LeverandorKampanje AS LOGICAL
    FIELD NormalPris AS LOGICAL
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD setAnnonse AS LOGICAL
    FIELD AktiveresTid_time AS CHARACTER
    FIELD GyldigTilTid_time AS CHARACTER
    FIELD Kampanjehode_Endret AS CHARACTER
    FIELD KroneRabatt AS DECIMAL
    FIELD MistePris AS DECIMAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
  .
DEF BUFFER v_KampanjeHode FOR TEMP-TABLE KampanjeHode.

FUNCTION getBuffersAndFieldsBrwKampanjeHode RETURNS CHARACTER():
  RETURN
    'KampanjeHode'
     + ';KampanjeId'
     + ';Aktivert'
     + ';IgnorerNOS'
     + ';Beskrivelse'
     + ';Notat'
     + ';ProfilNr'
     + ';BrukerID'
     + ';RegistrertAv'
     + ';StartDato'
     + ';SluttDato'
     + ';GyldigTilTid'
     + ';KampanjePris'
     + ';AktiveresTid'
     + ';Kamp%'
     + ';EDato'
     + ';ETid'
     + ';AvslagType'
     + ';Komplett'
     + ';LeverandorKampanje'
     + ';NormalPris'
     + ';RegistrertDato'
     + ';RegistrertTid'
     + ';setAnnonse'
     + ';KroneRabatt'
     + ';MistePris'
     + ';+Kampanjehode_AvslagType|CHARACTER||Kampanjehode_AvslagType(ROWID)|KampanjeType'
     + ';+Kampanjehode_AktivTilbud|INTEGER||Kampanjehode_AktivTilbud(ROWID)|Aktivt'
     + ';+Kampanjehode_AntallLinjer|CHARACTER||Kampanjehode_AntallLinjer(ROWID)|Linjer'
     + ';+fiKamp%|DECIMAL||fiKamp%(ROWID)|Rab%'
     + ';+Kampanjehode_DatoTidAktiveres|CHARACTER||Kampanjehode_DatoTidAktiveres(ROWID)|Aktiveres'
     + ';+Kampanjehode_DatoTidSlutt|CHARACTER||Kampanjehode_DatoTidSlutt(ROWID)|Slutt'
     + ';+AktiveresTid_time|CHARACTER||AktiveresTid_time(ROWID)|Akt.tid'
     + ';+GyldigTilTid_time|CHARACTER||GyldigTilTid_time(ROWID)|Gyld.tid'
     + ';+Kampanjehode_Endret|CHARACTER||Kampanjehode_Endret(ROWID)|Endret'
     .
END FUNCTION.
FUNCTION getQueryJoinBrwKampanjeHode RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKampanjeHode RETURNS CHARACTER():
  RETURN 
     'server/kampanjehode_brwcalc.p' /* Kampanjehode_AvslagType(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* Kampanjehode_AktivTilbud(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* Kampanjehode_AntallLinjer(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* fiKamp%(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* Kampanjehode_DatoTidAktiveres(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* Kampanjehode_DatoTidSlutt(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* AktiveresTid_time(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* GyldigTilTid_time(ROWID) */
   + ',server/kampanjehode_brwcalc.p' /* Kampanjehode_Endret(ROWID) */
     .
END FUNCTION.


DEF VAR oFmKampanjeHode   AS JBoxFieldMap NO-UNDO.
DEF VAR otbKampanjeHode   AS JBoxToolbar  NO-UNDO.
DEF VAR otbKampanjeLinje  AS JBoxToolbar  NO-UNDO.
/*** Start instance property definitions for JBoxBrowse object oBrwKampanjeLinje ***/
DEF VAR oBrwKampanjeLinje AS JBoxBrowse   NO-UNDO.
DEF TEMP-TABLE KampanjeLinje
  FIELD KampanjeLinje            AS INTEGER
  FIELD KampanjeLinje_NOS        AS CHARACTER
  FIELD KampanjeLinje_Beskr      AS CHARACTER
  FIELD KampanjeLinje_LevKod     AS CHARACTER
  FIELD KampanjeLinje_LevFargKod AS CHARACTER
  FIELD KampanjeLinje_SasongKode AS INTEGER
  FIELD Behandlet                AS LOGICAL
  FIELD VareKost                 AS DECIMAL
  FIELD Pris_1                   AS DECIMAL
  FIELD jbextent_1_Pris          AS DECIMAL /* placeholder for calculation */
  FIELD Pris_2                   AS DECIMAL
  FIELD jbextent_2_Pris          AS DECIMAL /* placeholder for calculation */
  FIELD KampanjeLinje_Rab%       AS DECIMAL
  FIELD KampanjeLinje_HovedKat   AS CHARACTER
  FIELD KampanjeLinje_Varemerke  AS CHARACTER
  FIELD KampanjeLinje_Registrert AS CHARACTER
  FIELD KampanjeLinje_Endret     AS CHARACTER
  FIELD ArtikkelNr               AS DECIMAL
  FIELD Vg                       AS INTEGER
  FIELD LopNr                    AS INTEGER
  FIELD KampanjeId               AS INTEGER
  FIELD ProfilNr                 AS INTEGER
  FIELD BrukerID                 AS CHARACTER
  FIELD EDato                    AS DATE
  FIELD ETid                     AS INTEGER
  FIELD Feilkode                 AS CHARACTER
  FIELD Mva%                     AS DECIMAL
  FIELD MvaKr                    AS DECIMAL
  FIELD KampanjeLinje_Sasong     AS CHARACTER
  FIELD RegistrertAv             AS CHARACTER
  FIELD RegistrertDato           AS DATE
  FIELD RegistrertTid            AS INTEGER
  FIELD RowIdent1                AS CHARACTER 
  FIELD RowCount                 AS INTEGER
  FIELD jbCountDistinct          AS INTEGER   FORMAT '>>>,>>>,>>9' INIT 1
  FIELD jbAverage                AS DECIMAL   FORMAT '->>>,>>>,>>9.99'
  INDEX idxRowids RowIdent1
  .
DEF BUFFER v_KampanjeLinje FOR TEMP-TABLE KampanjeLinje.


FUNCTION getBuffersAndFieldsBrwKampanjeLinje RETURNS CHARACTER():
  RETURN
    'KampanjeLinje'
    + ';KampanjeLinje'
    + ';Behandlet'
    + ';VareKost'
    + ';Pris[1]'
    + ';Pris[2]'
    + ';ArtikkelNr'
    + ';Vg'
    + ';LopNr'
    + ';KampanjeId'
    + ';ProfilNr'
    + ';BrukerID'
    + ';EDato'
    + ';ETid'
    + ';Feilkode'
    + ';Mva%'
    + ';MvaKr'
    + ';RegistrertAv'
    + ';RegistrertDato'
    + ';RegistrertTid'
    + ';+KampanjeLinje_NOS|CHARACTER||KampanjeLinje_NOS(ROWID)|Lagerkoder'
    + ';+KampanjeLinje_Beskr|CHARACTER||KampanjeLinje_Beskr(ROWID)|Varetekst'
    + ';+KampanjeLinje_LevKod|CHARACTER||KampanjeLinje_LevKod(ROWID)|Lev.artikkelnr'
    + ';+KampanjeLinje_LevFargKod|CHARACTER||KampanjeLinje_LevFargKod(ROWID)|Lev.fargekode'
    + ';+KampanjeLinje_SasongKode|INTEGER||KampanjeLinje_SasongKode(ROWID)|Sasong'
    + ';+KampanjeLinje_Rab%|DECIMAL||KampanjeLinje_Rab%(ROWID)|Rab%'
    + ';+KampanjeLinje_HovedKat|CHARACTER||KampanjeLinje_HovedKat(ROWID)|Hovedkategori'
    + ';+KampanjeLinje_Varemerke|CHARACTER||KampanjeLinje_Varemerke(ROWID)|Varemerke'
    + ';+KampanjeLinje_Registrert|CHARACTER||KampanjeLinje_Registrert(ROWID)|Registrert'
    + ';+KampanjeLinje_Endret|CHARACTER||KampanjeLinje_Endret(ROWID)|Endret'
    + ';+KampanjeLinje_Sasong|CHARACTER||KampanjeLinje_Sasong(ROWID)|Sesong'
    .
END FUNCTION.
FUNCTION getQueryJoinBrwKampanjeLinje RETURNS CHARACTER():
  RETURN ''.
END FUNCTION.
FUNCTION getCalcFieldProcBrwKampanjeLinje RETURNS CHARACTER():
  RETURN 
    'server/kampanjelinje_brwcalc.p' /* KampanjeLinje_NOS(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_Beskr(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_LevKod(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_LevFargKod(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_SasongKode(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_Rab%(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_HovedKat(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_Varemerke(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_Registrert(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_Endret(ROWID) */
    + ',server/kampanjelinje_brwcalc.p' /* KampanjeLinje_Sasong(ROWID) */
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
&Scoped-define BROWSE-NAME BrwKampanjeHode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KampanjeHode KampanjeLinje

/* Definitions for BROWSE BrwKampanjeHode                               */
&Scoped-define FIELDS-IN-QUERY-BrwKampanjeHode KampanjeHode.KampanjeId ~
KampanjeHode.Kampanjehode_AvslagType KampanjeHode.Kampanjehode_AktivTilbud ~
KampanjeHode.Aktivert KampanjeHode.IgnorerNOS KampanjeHode.Beskrivelse ~
KampanjeHode.Kampanjehode_AntallLinjer KampanjeHode.fiKamp% ~
KampanjeHode.Kampanjehode_DatoTidAktiveres ~
KampanjeHode.Kampanjehode_DatoTidSlutt KampanjeHode.Notat ~
KampanjeHode.ProfilNr KampanjeHode.BrukerID KampanjeHode.RegistrertAv ~
KampanjeHode.StartDato KampanjeHode.SluttDato KampanjeHode.GyldigTilTid ~
KampanjeHode.KampanjePris KampanjeHode.AktiveresTid KampanjeHode.Kamp% ~
KampanjeHode.EDato KampanjeHode.ETid KampanjeHode.AvslagType ~
KampanjeHode.Komplett KampanjeHode.LeverandorKampanje ~
KampanjeHode.NormalPris KampanjeHode.RegistrertDato ~
KampanjeHode.RegistrertTid KampanjeHode.setAnnonse ~
KampanjeHode.AktiveresTid_time KampanjeHode.GyldigTilTid_time ~
KampanjeHode.Kampanjehode_Endret KampanjeHode.KroneRabatt ~
KampanjeHode.MistePris 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKampanjeHode ~
KampanjeHode.KampanjeId 
&Scoped-define QUERY-STRING-BrwKampanjeHode FOR EACH KampanjeHode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKampanjeHode OPEN QUERY BrwKampanjeHode FOR EACH KampanjeHode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKampanjeHode KampanjeHode
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKampanjeHode KampanjeHode


/* Definitions for BROWSE BrwKampanjeLinje                              */
&Scoped-define FIELDS-IN-QUERY-BrwKampanjeLinje KampanjeLinje.KampanjeLinje ~
KampanjeLinje.KampanjeLinje_NOS KampanjeLinje.KampanjeLinje_Beskr ~
KampanjeLinje.KampanjeLinje_LevKod KampanjeLinje.KampanjeLinje_LevFargKod ~
KampanjeLinje.KampanjeLinje_SasongKode KampanjeLinje.Behandlet ~
KampanjeLinje.VareKost KampanjeLinje.Pris_1 KampanjeLinje.Pris_2 ~
KampanjeLinje.KampanjeLinje_Rab% KampanjeLinje.KampanjeLinje_HovedKat ~
KampanjeLinje.KampanjeLinje_Varemerke ~
KampanjeLinje.KampanjeLinje_Registrert KampanjeLinje.KampanjeLinje_Endret ~
KampanjeLinje.ArtikkelNr KampanjeLinje.Vg KampanjeLinje.LopNr ~
KampanjeLinje.KampanjeId KampanjeLinje.ProfilNr KampanjeLinje.BrukerID ~
KampanjeLinje.EDato KampanjeLinje.ETid KampanjeLinje.Feilkode ~
KampanjeLinje.Mva% KampanjeLinje.MvaKr KampanjeLinje.KampanjeLinje_Sasong ~
KampanjeLinje.RegistrertAv KampanjeLinje.RegistrertDato ~
KampanjeLinje.RegistrertTid 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrwKampanjeLinje ~
KampanjeLinje.KampanjeLinje KampanjeLinje.ArtikkelNr 
&Scoped-define QUERY-STRING-BrwKampanjeLinje FOR EACH KampanjeLinje NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrwKampanjeLinje OPEN QUERY BrwKampanjeLinje FOR EACH KampanjeLinje NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrwKampanjeLinje KampanjeLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BrwKampanjeLinje KampanjeLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS first_tbKampanjeHode BrwKampanjeHode ~
tbKampanjeHode btnSplitBarY tbKampanjeLinje first_tbKampanjeLinje ~
BrwKampanjeLinje prev_tbKampanjeHode prev_tbKampanjeLinje ~
next_tbKampanjeHode next_tbKampanjeLinje last_tbKampanjeHode ~
last_tbKampanjeLinje new_tbKampanjeHode copy_tbKampanjeHode ~
delete_tbKampanjeLinje refresh_tbKampanjeLinje edit_tbKampanjeHode ~
filter_tbKampanjeLinje save_tbKampanjeHode excel_tbKampanjeLinje ~
undo_tbKampanjeHode LagerSalg_tbKampanjeLinje delete_tbKampanjeHode ~
refresh_tbKampanjeHode filter_tbKampanjeHode excel_tbKampanjeHode ~
LeggTilPrisprofil_tbKampanjeHode LeggTilVare_tbKampanjeLinje ~
EndreAktTilb_tbKampanjeHode KampanjeSalg_tbKampanjeLinje AvslagType ~
KampanjePris Kronerabatt fiKamp% MistePris AngreAvbryt_tbKampanjeHode Notat ~
Beskrivelse IgnorerNOS HentFraLager_tbKampanjeLinje ~
TilPrisko_tbKampanjeHode HentFraKampanje_tbKampanjeLinje ~
AntallPaTilbud_tbKampanjeHode StartDato SluttDato slEkstraPrisProfiler ~
AktiveresTid_time GyldigTilTid_time Kampanjehode_Endret 
&Scoped-Define DISPLAYED-OBJECTS ProfilNr AvslagType KampanjePris ~
Kronerabatt fiKamp% MistePris Notat Beskrivelse IgnorerNOS StartDato ~
SluttDato slEkstraPrisProfiler AktiveresTid_time GyldigTilTid_time ~
fiTekst-2 Kampanjehode_Endret fiTekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON AngreAvbryt_tbKampanjeHode 
     LABEL "Deaktiver kampanje" 
     SIZE 22 BY 1.1 TOOLTIP "Avslutter en aktiv kampanje".

DEFINE BUTTON AntallPaTilbud_tbKampanjeHode 
     LABEL "Vis antall artikler på tilbud" 
     SIZE 30 BY 1.1 TOOLTIP "Viser antall artikler som er aktive på tilbud".

DEFINE BUTTON btnSplitBarY 
     IMAGE-UP FILE "bmp/tabup.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 1" 
     SIZE 198 BY .43.

DEFINE BUTTON copy_tbKampanjeHode 
     IMAGE-UP FILE "bmp/copy.bmp":U
     LABEL "Kopier" 
     SIZE 4.6 BY 1.1 TOOLTIP "Kopier (ALT-K)".

DEFINE BUTTON delete_tbKampanjeHode 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON delete_tbKampanjeLinje 
     IMAGE-UP FILE "bmp/del16e.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.1 TOOLTIP "Slett (CTRL-D)".

DEFINE BUTTON edit_tbKampanjeHode 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 4.6 BY 1.1 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON EndreAktTilb_tbKampanjeHode 
     LABEL "Endre aktivt tilbud" 
     SIZE 20 BY 1.1 TOOLTIP "Endrer et aktivt tilbud.".

DEFINE BUTTON excel_tbKampanjeHode 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON excel_tbKampanjeLinje 
     IMAGE-UP FILE "gif/afexcel.gif":U
     LABEL "Eksporter til E&xcel" 
     SIZE 4.6 BY 1.1 TOOLTIP "Eksporter til E&xcel (ALT-X)".

DEFINE BUTTON filter_tbKampanjeHode 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON filter_tbKampanjeLinje 
     IMAGE-UP FILE "gif/filter.gif":U
     LABEL "Filter" 
     SIZE 4.6 BY 1.1 TOOLTIP "Filter (CTRL-F)".

DEFINE BUTTON first_tbKampanjeHode 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON first_tbKampanjeLinje 
     IMAGE-UP FILE "bmp/first.bmp":U
     LABEL "First" 
     SIZE 4.6 BY 1.1 TOOLTIP "First (ALT-F)".

DEFINE BUTTON HentFraKampanje_tbKampanjeLinje 
     LABEL "Hent fra kampanjer" 
     SIZE 20.6 BY 1.1 TOOLTIP "Henter inn varelinjer fra andre kampanjer".

DEFINE BUTTON HentFraLager_tbKampanjeLinje 
     LABEL "Hent ikke aktiverte" 
     SIZE 20 BY 1.1 TOOLTIP "Henter ikke aktiverte artikler med lager".

DEFINE BUTTON KampanjeSalg_tbKampanjeLinje 
     LABEL "Hent fra solgt% aktiv kampanje" 
     SIZE 31 BY 1.1 TOOLTIP "Legge til varer fra liste med solgt% på kampanje".

DEFINE BUTTON LagerSalg_tbKampanjeLinje 
     LABEL "Hent fra solgt% lager" 
     SIZE 24.6 BY 1.1 TOOLTIP "Legg til varer fra liste over vare med solgt% lager".

DEFINE BUTTON last_tbKampanjeHode 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON last_tbKampanjeLinje 
     IMAGE-UP FILE "bmp/last.bmp":U
     LABEL "Last" 
     SIZE 4.6 BY 1.1 TOOLTIP "Last (ALT-L)".

DEFINE BUTTON LeggTilPrisprofil_tbKampanjeHode 
     LABEL "Legg til prisprofil" 
     SIZE 20 BY 1.1 TOOLTIP "Legger til prisprofiler som kampanjen skal aktiveres for".

DEFINE BUTTON LeggTilVare_tbKampanjeLinje 
     LABEL "Hent fra modell liste" 
     SIZE 25.4 BY 1.1 TOOLTIP "Legger inn nye varer i tilbudet fra søkeliste modell.".

DEFINE BUTTON new_tbKampanjeHode 
     IMAGE-UP FILE "bmp/new16e.bmp":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.1 TOOLTIP "Ny (CTRL-N)".

DEFINE BUTTON next_tbKampanjeHode 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON next_tbKampanjeLinje 
     IMAGE-UP FILE "bmp/next.bmp":U
     LABEL "Next" 
     SIZE 4.6 BY 1.1 TOOLTIP "Next (ALT-N)".

DEFINE BUTTON prev_tbKampanjeHode 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON prev_tbKampanjeLinje 
     IMAGE-UP FILE "bmp/prev.bmp":U
     LABEL "Prev" 
     SIZE 4.6 BY 1.1 TOOLTIP "Prev (ALT-P)".

DEFINE BUTTON refresh_tbKampanjeHode 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON refresh_tbKampanjeLinje 
     IMAGE-UP FILE "gif/refresh.gif":U
     LABEL "Refresh" 
     SIZE 4.6 BY 1.1 TOOLTIP "Refresh (F5)".

DEFINE BUTTON save_tbKampanjeHode 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON TilPrisko_tbKampanjeHode 
     LABEL "AKTIVER - Send til priskø" 
     SIZE 27 BY 1.1 TOOLTIP "Aktiverer kampanje og legger den inn i priskøen".

DEFINE BUTTON undo_tbKampanjeHode 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE AktiveresTid_time AS CHARACTER INITIAL "00:00" 
     VIEW-AS COMBO-BOX INNER-LINES 50
     LIST-ITEMS "00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","23:59" 
     DROP-DOWN
     SIZE 11 BY 1 TOOLTIP "Tidspunkt for aktivering av kampanje." NO-UNDO.

DEFINE VARIABLE AvslagType AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Avslag type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Rabatt%",1,
                     "Fastpris",2,
                     "Kronerabatt",3
     DROP-DOWN-LIST
     SIZE 16 BY 1 TOOLTIP "Viser type av avslag som kampanjen har." NO-UNDO.

DEFINE VARIABLE GyldigTilTid_time AS CHARACTER INITIAL "23:59" 
     VIEW-AS COMBO-BOX INNER-LINES 50
     LIST-ITEMS "00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00","23:59" 
     DROP-DOWN
     SIZE 11 BY 1 TOOLTIP "Tidspunkt for deaktivering av kampanje." NO-UNDO.

DEFINE VARIABLE Notat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 99 BY 2.24 TOOLTIP "Kort notat som forklarer kampanjens målsetning" NO-UNDO.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 83 BY 1.

DEFINE VARIABLE fiKamp% AS DECIMAL FORMAT "->9" INITIAL 0 
     LABEL "Rabatt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Kampanjerabatt.".

DEFINE VARIABLE fiTekst AS CHARACTER FORMAT "X(256)":U INITIAL "Ekstra profiler:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiTekst-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Notat:" 
      VIEW-AS TEXT 
     SIZE 6 BY .62 NO-UNDO.

DEFINE VARIABLE Kampanjehode_Endret AS CHARACTER FORMAT "X(256)":U 
     LABEL "Endret" 
      VIEW-AS TEXT 
     SIZE 45.6 BY .62 TOOLTIP "Dato, tid og brukerid på den som sist endret/oppdaterte kampanjen." NO-UNDO.

DEFINE VARIABLE KampanjePris AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Kampanjepris" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Fast pris på produktene i kampanjen" NO-UNDO.

DEFINE VARIABLE Kronerabatt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Kronerabatt" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Fast kronerabatt" NO-UNDO.

DEFINE VARIABLE MistePris AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Minstepris" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Minstepris på artikler som kan legges inn ved fastpris og kronerabatt" NO-UNDO.

DEFINE VARIABLE ProfilNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Profilnr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE SluttDato AS DATE FORMAT "99/99/99":U 
     LABEL "Slutt dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Dato for avsluttning av kampanjen" NO-UNDO.

DEFINE VARIABLE StartDato AS DATE FORMAT "99/99/99":U 
     LABEL "Start dato/tid" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Dato for start av kampanje" NO-UNDO.

DEFINE RECTANGLE tbKampanjeHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 198.8 BY 1.29.

DEFINE RECTANGLE tbKampanjeLinje
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 197.8 BY 1.29.

DEFINE VARIABLE slEkstraPrisProfiler AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 28.2 BY 2.14 TOOLTIP "Viser ekstra prsprofiler som kampanjen skal aktiveres for"
     FGCOLOR 0 FONT 6 NO-UNDO.

DEFINE VARIABLE IgnorerNOS AS LOGICAL INITIAL NO 
     LABEL "Ignorer NOS ved aktivering" 
     VIEW-AS TOGGLE-BOX
     SIZE 36.8 BY .81 TOOLTIP "Ved aktivering av kampanje, aktiveres også NOS merkede artikler" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrwKampanjeHode FOR 
      KampanjeHode SCROLLING.

DEFINE QUERY BrwKampanjeLinje FOR 
      KampanjeLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrwKampanjeHode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKampanjeHode C-Win _STRUCTURED
  QUERY BrwKampanjeHode NO-LOCK DISPLAY
      KampanjeHode.KampanjeId COLUMN-LABEL "Id" FORMAT ">>>>>>9":U
      KampanjeHode.Kampanjehode_AvslagType COLUMN-LABEL "KampanjeType" FORMAT "X(12)":U
      KampanjeHode.Kampanjehode_AktivTilbud COLUMN-LABEL "Aktivt" FORMAT "9":U
            WIDTH 2.8
      KampanjeHode.Aktivert COLUMN-LABEL "Aktivert" FORMAT "*/":U
            WIDTH 3.6
      KampanjeHode.IgnorerNOS COLUMN-LABEL "INOS" FORMAT "*/":U
      KampanjeHode.Beskrivelse COLUMN-LABEL "Beskrivelse" FORMAT "X(40)":U
      KampanjeHode.Kampanjehode_AntallLinjer COLUMN-LABEL "Linjer" FORMAT "X(8)":U
      KampanjeHode.fiKamp% COLUMN-LABEL "Rab%" FORMAT "->>,>>9.9":U
            WIDTH 6
      KampanjeHode.Kampanjehode_DatoTidAktiveres COLUMN-LABEL "Aktiveres" FORMAT "X(25)":U
            WIDTH 22
      KampanjeHode.Kampanjehode_DatoTidSlutt COLUMN-LABEL "Slutt" FORMAT "X(25)":U
            WIDTH 22
      KampanjeHode.Notat COLUMN-LABEL "Notat" FORMAT "X(256)":U
      KampanjeHode.ProfilNr COLUMN-LABEL "Prisprofil" FORMAT ">>>>>>9":U
      KampanjeHode.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      KampanjeHode.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      KampanjeHode.StartDato COLUMN-LABEL "Startdato" FORMAT "99/99/99":U
      KampanjeHode.SluttDato COLUMN-LABEL "Sluttdato" FORMAT "99/99/99":U
      KampanjeHode.GyldigTilTid COLUMN-LABEL "Gyldig til tidspunkt" FORMAT "->,>>>,>>9":U
      KampanjeHode.KampanjePris COLUMN-LABEL "Kampanjepris" FORMAT "->>>,>>9.99":U
      KampanjeHode.AktiveresTid COLUMN-LABEL "Tidspunkt" FORMAT "->,>>>,>>9":U
      KampanjeHode.Kamp% COLUMN-LABEL "Rab%" FORMAT "->9.9":U WIDTH 7
      KampanjeHode.EDato COLUMN-LABEL "EDato" FORMAT "99/99/99":U
      KampanjeHode.ETid COLUMN-LABEL "ETid" FORMAT "->,>>>,>>9":U
      KampanjeHode.AvslagType COLUMN-LABEL "Ty" FORMAT "9":U
      KampanjeHode.Komplett COLUMN-LABEL "Komplett" FORMAT "yes/no":U
      KampanjeHode.LeverandorKampanje COLUMN-LABEL "Leverandørkampanje" FORMAT "yes/no":U
      KampanjeHode.NormalPris COLUMN-LABEL "NormalPris" FORMAT "yes/no":U
      KampanjeHode.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      KampanjeHode.RegistrertTid COLUMN-LABEL "Registreringstidspunkt" FORMAT "->,>>>,>>9":U
      KampanjeHode.setAnnonse COLUMN-LABEL "Sett annonseflagg" FORMAT "yes/no":U
      KampanjeHode.AktiveresTid_time COLUMN-LABEL "Akt.tid" FORMAT "X(8)":U
      KampanjeHode.GyldigTilTid_time COLUMN-LABEL "Gyld.tid" FORMAT "X(8)":U
      KampanjeHode.Kampanjehode_Endret COLUMN-LABEL "Endret" FORMAT "X(50)":U
      KampanjeHode.KroneRabatt COLUMN-LABEL "Kronerabatt" FORMAT "->>,>>9.99":U
      KampanjeHode.MistePris COLUMN-LABEL "Minstepris" FORMAT "->,>>>,>>9.99":U
  ENABLE
      KampanjeHode.KampanjeId
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS DROP-TARGET SIZE 84 BY 9.29 FIT-LAST-COLUMN.

DEFINE BROWSE BrwKampanjeLinje
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrwKampanjeLinje C-Win _STRUCTURED
  QUERY BrwKampanjeLinje NO-LOCK DISPLAY
      KampanjeLinje.KampanjeLinje COLUMN-LABEL "Linjenr" FORMAT ">>>>9":U
            WIDTH 6.6
      KampanjeLinje.KampanjeLinje_NOS COLUMN-LABEL "Lagerkoder" FORMAT "X(10)":U
      KampanjeLinje.KampanjeLinje_Beskr COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
      KampanjeLinje.KampanjeLinje_LevKod COLUMN-LABEL "Lev.artikkelnr" FORMAT "X(30)":U
      KampanjeLinje.KampanjeLinje_LevFargKod COLUMN-LABEL "Lev.fargekode" FORMAT "X(30)":U
      KampanjeLinje.KampanjeLinje_SasongKode COLUMN-LABEL "Sasong" FORMAT ">>>>>>9":U
      KampanjeLinje.Behandlet COLUMN-LABEL "Behandlet" FORMAT "*/":U
      KampanjeLinje.VareKost COLUMN-LABEL "Varekost" FORMAT "->,>>>,>>9.99":U
      KampanjeLinje.Pris_1 COLUMN-LABEL "Norm.pris" FORMAT "->,>>>,>>9.99":U
      KampanjeLinje.Pris_2 COLUMN-LABEL "Tilb.pris" FORMAT "->,>>>,>>9.99":U
      KampanjeLinje.KampanjeLinje_Rab% COLUMN-LABEL "Rab%" FORMAT "->,>>9.9":U
      KampanjeLinje.KampanjeLinje_HovedKat COLUMN-LABEL "Hovedkategori" FORMAT "X(20)":U
      KampanjeLinje.KampanjeLinje_Varemerke COLUMN-LABEL "Varemerke" FORMAT "X(20)":U
      KampanjeLinje.KampanjeLinje_Registrert COLUMN-LABEL "Registrert" FORMAT "X(20)":U
      KampanjeLinje.KampanjeLinje_Endret COLUMN-LABEL "Endret" FORMAT "X(20)":U
      KampanjeLinje.ArtikkelNr COLUMN-LABEL "Artikkelnummer" FORMAT "zzzzzzzzzzzz9":U
            WIDTH 14.4
      KampanjeLinje.Vg COLUMN-LABEL "Varegruppe" FORMAT "zzzzz9":U
      KampanjeLinje.LopNr COLUMN-LABEL "Løpenummer" FORMAT "->>>>>9":U
      KampanjeLinje.KampanjeId COLUMN-LABEL "Kampanjeid" FORMAT "->,>>>,>>9":U
      KampanjeLinje.ProfilNr COLUMN-LABEL "Prisprofil" FORMAT ">>>>>>9":U
      KampanjeLinje.BrukerID COLUMN-LABEL "Bruker" FORMAT "X(10)":U
      KampanjeLinje.EDato COLUMN-LABEL "Endret" FORMAT "99/99/9999":U
      KampanjeLinje.ETid COLUMN-LABEL "ETid" FORMAT "->,>>>,>>9":U
      KampanjeLinje.Feilkode COLUMN-LABEL "Feilkode" FORMAT "Feilkode":U
      KampanjeLinje.Mva% COLUMN-LABEL "Mva%" FORMAT "->>9.99":U
      KampanjeLinje.MvaKr COLUMN-LABEL "MvaKr" FORMAT "->,>>>,>>9.99":U
      KampanjeLinje.KampanjeLinje_Sasong COLUMN-LABEL "Sesong" FORMAT "X(30)":U
      KampanjeLinje.RegistrertAv COLUMN-LABEL "Reg.Av" FORMAT "X(10)":U
      KampanjeLinje.RegistrertDato COLUMN-LABEL "RDato" FORMAT "99/99/9999":U
      KampanjeLinje.RegistrertTid COLUMN-LABEL "Registreringstidspunkt" FORMAT "->,>>>,>>9":U
  ENABLE
      KampanjeLinje.KampanjeLinje
      KampanjeLinje.ArtikkelNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 198 BY 10.1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     first_tbKampanjeHode AT ROW 1.29 COL 2.4 WIDGET-ID 10
     BrwKampanjeHode AT ROW 2.76 COL 2 WIDGET-ID 200
     btnSplitBarY AT ROW 12.19 COL 2 WIDGET-ID 6
     first_tbKampanjeLinje AT ROW 12.91 COL 2.4 WIDGET-ID 48
     BrwKampanjeLinje AT ROW 14.24 COL 2 WIDGET-ID 300
     prev_tbKampanjeHode AT ROW 1.29 COL 7.2 WIDGET-ID 12
     prev_tbKampanjeLinje AT ROW 12.91 COL 7.2 WIDGET-ID 50
     next_tbKampanjeHode AT ROW 1.29 COL 11.8 WIDGET-ID 14
     next_tbKampanjeLinje AT ROW 12.91 COL 11.8 WIDGET-ID 52
     last_tbKampanjeHode AT ROW 1.29 COL 16.4 WIDGET-ID 16
     last_tbKampanjeLinje AT ROW 12.91 COL 16.4 WIDGET-ID 54
     new_tbKampanjeHode AT ROW 1.29 COL 21 WIDGET-ID 18
     copy_tbKampanjeHode AT ROW 1.29 COL 25.6 WIDGET-ID 114
     delete_tbKampanjeLinje AT ROW 12.91 COL 21 WIDGET-ID 58
     refresh_tbKampanjeLinje AT ROW 12.91 COL 25.6 WIDGET-ID 60
     edit_tbKampanjeHode AT ROW 1.29 COL 30.2 WIDGET-ID 92
     filter_tbKampanjeLinje AT ROW 12.91 COL 30.2 WIDGET-ID 62
     save_tbKampanjeHode AT ROW 1.29 COL 34.8 WIDGET-ID 72
     excel_tbKampanjeLinje AT ROW 12.91 COL 34.8 WIDGET-ID 64
     undo_tbKampanjeHode AT ROW 1.29 COL 39.4 WIDGET-ID 94
     LagerSalg_tbKampanjeLinje AT ROW 12.91 COL 39.4 WIDGET-ID 108
     delete_tbKampanjeHode AT ROW 1.29 COL 44 WIDGET-ID 20
     refresh_tbKampanjeHode AT ROW 1.29 COL 48.6 WIDGET-ID 22
     filter_tbKampanjeHode AT ROW 1.29 COL 53.2 WIDGET-ID 24
     excel_tbKampanjeHode AT ROW 1.29 COL 57.8 WIDGET-ID 26
     LeggTilPrisprofil_tbKampanjeHode AT ROW 1.29 COL 62.4 WIDGET-ID 122
     LeggTilVare_tbKampanjeLinje AT ROW 12.91 COL 64 WIDGET-ID 102
     EndreAktTilb_tbKampanjeHode AT ROW 1.29 COL 82.4 WIDGET-ID 104
     KampanjeSalg_tbKampanjeLinje AT ROW 12.91 COL 89.4 WIDGET-ID 106
     ProfilNr AT ROW 2.91 COL 98.4 COLON-ALIGNED
     AvslagType AT ROW 3.95 COL 98.4 COLON-ALIGNED WIDGET-ID 124
     KampanjePris AT ROW 5 COL 98.4 COLON-ALIGNED
     Kronerabatt AT ROW 5 COL 98.4 COLON-ALIGNED
     fiKamp% AT ROW 5 COL 98.4 COLON-ALIGNED
     MistePris AT ROW 6.05 COL 98.4 COLON-ALIGNED
     AngreAvbryt_tbKampanjeHode AT ROW 1.29 COL 102.4 WIDGET-ID 80
     Notat AT ROW 8.29 COL 101 NO-LABEL WIDGET-ID 66
     Beskrivelse AT ROW 2.91 COL 115 COLON-ALIGNED NO-LABEL
     IgnorerNOS AT ROW 4.05 COL 117.2 WIDGET-ID 98
     HentFraLager_tbKampanjeLinje AT ROW 12.91 COL 120.4 WIDGET-ID 112
     TilPrisko_tbKampanjeHode AT ROW 1.29 COL 124.4 WIDGET-ID 78
     HentFraKampanje_tbKampanjeLinje AT ROW 12.91 COL 140.4 WIDGET-ID 116
     AntallPaTilbud_tbKampanjeHode AT ROW 1.29 COL 151.4 WIDGET-ID 110
     StartDato AT ROW 4 COL 169.6 COLON-ALIGNED
     SluttDato AT ROW 5 COL 169.6 COLON-ALIGNED
     slEkstraPrisProfiler AT ROW 6.05 COL 171.6 NO-LABEL WIDGET-ID 118
     AktiveresTid_time AT ROW 4 COL 187 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     GyldigTilTid_time AT ROW 5 COL 187 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     fiTekst-2 AT ROW 8.43 COL 92.4 COLON-ALIGNED NO-LABEL
     Kampanjehode_Endret AT ROW 7.48 COL 98.4 COLON-ALIGNED
     fiTekst AT ROW 6.05 COL 155.4 COLON-ALIGNED NO-LABEL
     tbKampanjeHode AT ROW 1.19 COL 2.2 WIDGET-ID 8
     tbKampanjeLinje AT ROW 12.91 COL 4 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 200.8 BY 23.43 WIDGET-ID 100.


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
         HEIGHT             = 23.43
         WIDTH              = 200.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 200.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 200.8
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
   FRAME-NAME L-To-R,COLUMNS                                            */
/* BROWSE-TAB BrwKampanjeHode first_tbKampanjeHode DEFAULT-FRAME */
/* BROWSE-TAB BrwKampanjeLinje first_tbKampanjeLinje DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 23.43
       FRAME DEFAULT-FRAME:WIDTH            = 200.8.

ASSIGN 
       AktiveresTid_time:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "00:00".

ASSIGN 
       KampanjeHode.AktiveresTid:VISIBLE IN BROWSE BrwKampanjeHode = FALSE
       KampanjeHode.Kamp%:VISIBLE IN BROWSE BrwKampanjeHode = FALSE.

ASSIGN 
       KampanjeLinje.BrukerID:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.EDato:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.ETid:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.Feilkode:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.Mva%:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.MvaKr:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.RegistrertAv:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.RegistrertDato:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE
       KampanjeLinje.RegistrertTid:VISIBLE IN BROWSE BrwKampanjeLinje = FALSE.

ASSIGN 
       btnSplitBarY:MOVABLE IN FRAME DEFAULT-FRAME          = TRUE.

/* SETTINGS FOR FILL-IN fiTekst IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTekst:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Ekstra profiler:".

/* SETTINGS FOR FILL-IN fiTekst-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiTekst-2:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "Notat:".

ASSIGN 
       GyldigTilTid_time:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "23:59".

/* SETTINGS FOR FILL-IN ProfilNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       tbKampanjeHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,new;Ny,copy;Kopier,edit;Edit,save;Lagre,undo;Angre,delete;Slett,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,LeggTilPrisprofil;Legg til prisprofil,EndreAktTilb;Endre aktivt tilbud,AngreAvbryt;Deaktiver kampanje,TilPrisko;AKTIVER - Send til priskø,AntallPaTilbud;Vis antall artikler på tilbudmaxborder".

ASSIGN 
       tbKampanjeLinje:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "first;First,prev;Prev,next;Next,last;Last,delete;Slett,refresh;Refresh,filter;Filter,excel;Eksporter til E&xcel,LagerSalg;Hent fra solgt% lager¤enable,LeggTilVare;Hent fra modell liste¤enable,KampanjeSalg;Hent fra solgt% aktiv kampanje¤enable,HentFraLager;Hent ikke aktiverte¤enable,HentFraKampanje;Hent fra kampanjer¤enablemaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKampanjeHode
/* Query rebuild information for BROWSE BrwKampanjeHode
     _TblList          = "SkoTex.KampanjeHode"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KampanjeHode.KampanjeId" "Id" ">>>>>>9" "INTEGER" ? ? ? ? ? ? yes "" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KampanjeHode.Kampanjehode_AvslagType" "KampanjeType" "X(12)" "CHARACTER" ? ? ? ? ? ? no "" no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KampanjeHode.Kampanjehode_AktivTilbud" "Aktivt" "9" "INTEGER" ? ? ? ? ? ? no "" no no "2.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KampanjeHode.Aktivert" "Aktivert" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "3.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KampanjeHode.IgnorerNOS" "INOS" "*~~/" "LOGICAL" ? ? ? ? ? ? no "Ignorer NOS flagg på artikler ved aktivering av kampanjen." no no "5.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KampanjeHode.Beskrivelse" "Beskrivelse" "X(40)" "CHARACTER" ? ? ? ? ? ? no "" no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KampanjeHode.Kampanjehode_AntallLinjer" "Linjer" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KampanjeHode.fiKamp%" "Rab%" "->>,>>9.9" "DECIMAL" ? ? ? ? ? ? no "" no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KampanjeHode.Kampanjehode_DatoTidAktiveres" "Aktiveres" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KampanjeHode.Kampanjehode_DatoTidSlutt" "Slutt" "X(25)" "CHARACTER" ? ? ? ? ? ? no "" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KampanjeHode.Notat" "Notat" "X(256)" "CHARACTER" ? ? ? ? ? ? no "" no no "256" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KampanjeHode.ProfilNr" "Prisprofil" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Prisprofil" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KampanjeHode.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KampanjeHode.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte bilde" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KampanjeHode.StartDato" "Startdato" "99/99/99" "DATE" ? ? ? ? ? ? no "" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KampanjeHode.SluttDato" "Sluttdato" "99/99/99" "DATE" ? ? ? ? ? ? no "" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KampanjeHode.GyldigTilTid" "Gyldig til tidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Gyldig til tidspunkt." no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KampanjeHode.KampanjePris" "Kampanjepris" "->>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "" no no "12.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KampanjeHode.AktiveresTid" "Tidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt da prisenskal aktiveres." no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KampanjeHode.Kamp%" "Rab%" "->9.9" "DECIMAL" ? ? ? ? ? ? no "Kampanje/prisendringsprosent" no no "7" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KampanjeHode.EDato" "EDato" "99/99/99" "DATE" ? ? ? ? ? ? no "" no no "9.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KampanjeHode.ETid" "ETid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "" no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KampanjeHode.AvslagType" "Ty" "9" "INTEGER" ? ? ? ? ? ? no "" no no "2.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"KampanjeHode.Komplett" "Komplett" "yes/no" "LOGICAL" ? ? ? ? ? ? no "" no no "8.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"KampanjeHode.LeverandorKampanje" "Leverandørkampanje" "yes/no" "LOGICAL" ? ? ? ? ? ? no "Leverandørkampanje. Bare innpris endres i tilbudet." no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"KampanjeHode.NormalPris" "NormalPris" "yes/no" "LOGICAL" ? ? ? ? ? ? no "Feltet krysses for hvis det er normalprisendring." no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"KampanjeHode.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da bilde ble registrert i registeret" no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"KampanjeHode.RegistrertTid" "Registreringstidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt for registrering av bildet" no no "20.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"KampanjeHode.setAnnonse" "Sett annonseflagg" "yes/no" "LOGICAL" ? ? ? ? ? ? no "" no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"KampanjeHode.AktiveresTid_time" "Akt.tid" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"KampanjeHode.GyldigTilTid_time" "Gyld.tid" "X(8)" "CHARACTER" ? ? ? ? ? ? no "" no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"KampanjeHode.Kampanjehode_Endret" "Endret" "X(50)" "CHARACTER" ? ? ? ? ? ? no "" no no "50" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"KampanjeHode.KroneRabatt" "Kronerabatt" "->>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Fast kronerabatt." no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"KampanjeHode.MistePris" "Minstepris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Minstepris kan settes på kampanjer som har kronerabatt" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKampanjeHode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrwKampanjeLinje
/* Query rebuild information for BROWSE BrwKampanjeLinje
     _TblList          = "SkoTex.KampanjeLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje" "Linjenr" ">>>>9" "INTEGER" ? ? ? ? ? ? yes "" no no "6.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_NOS" "Lagerkoder" "X(10)" "CHARACTER" ? ? ? ? ? ? no "" no no "10.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_Beskr" "Varetekst" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_LevKod" "Lev.artikkelnr" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_LevFargKod" "Lev.fargekode" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_SasongKode" "Sasong" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"KampanjeLinje.Behandlet" "Behandlet" "*~~/" "LOGICAL" ? ? ? ? ? ? no "" no no "9.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"KampanjeLinje.VareKost" "Varekost" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Varekost" no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"KampanjeLinje.Pris_1" "Norm.pris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris inkl. mva." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"KampanjeLinje.Pris_2" "Tilb.pris" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Pris inkl. mva." no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_Rab%" "Rab%" "->,>>9.9" "DECIMAL" ? ? ? ? ? ? no "" no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_HovedKat" "Hovedkategori" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_Varemerke" "Varemerke" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_Registrert" "Registrert" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_Endret" "Endret" "X(20)" "CHARACTER" ? ? ? ? ? ? no "" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"KampanjeLinje.ArtikkelNr" "Artikkelnummer" "zzzzzzzzzzzz9" "DECIMAL" ? ? ? ? ? ? yes "" no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"KampanjeLinje.Vg" "Varegruppe" "zzzzz9" "INTEGER" ? ? ? ? ? ? no "'varegruppenummer" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"KampanjeLinje.LopNr" "Løpenummer" "->>>>>9" "INTEGER" ? ? ? ? ? ? no "Løpenummer innenfor varegruppen" no no "12.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"KampanjeLinje.KampanjeId" "Kampanjeid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "" no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"KampanjeLinje.ProfilNr" "Prisprofil" ">>>>>>9" "INTEGER" ? ? ? ? ? ? no "Prisprofil" no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"KampanjeLinje.BrukerID" "Bruker" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Bruker som registrerte/endret posten" no no "10" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"KampanjeLinje.EDato" "Endret" "99/99/9999" "DATE" ? ? ? ? ? ? no "Endret dato" no no "11.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"KampanjeLinje.ETid" "ETid" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Endret tidspunkt" no no "10.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"KampanjeLinje.Feilkode" "Feilkode" "Feilkode" "CHARACTER" ? ? ? ? ? ? no "" no no "8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"KampanjeLinje.Mva%" "Mva%" "->>9.99" "DECIMAL" ? ? ? ? ? ? no "Mva%" no no "7.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"KampanjeLinje.MvaKr" "MvaKr" "->,>>>,>>9.99" "DECIMAL" ? ? ? ? ? ? no "Mva kroner" no no "13.2" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"KampanjeLinje.KampanjeLinje_Sasong" "Sesong" "X(30)" "CHARACTER" ? ? ? ? ? ? no "" no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"KampanjeLinje.RegistrertAv" "Reg.Av" "X(10)" "CHARACTER" ? ? ? ? ? ? no "Brukerid på den som registrerte posten" no no "10" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"KampanjeLinje.RegistrertDato" "RDato" "99/99/9999" "DATE" ? ? ? ? ? ? no "Dato da posten ble registrert i registeret" no no "11.6" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"KampanjeLinje.RegistrertTid" "Registreringstidspunkt" "->,>>>,>>9" "INTEGER" ? ? ? ? ? ? no "Tidspunkt for registrering av posten" no no "20.8" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BrwKampanjeLinje */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <Insert window title> */
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
ON WINDOW-CLOSE OF C-Win /* <Insert window title> */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AktiveresTid_time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AktiveresTid_time C-Win
ON VALUE-CHANGED OF AktiveresTid_time IN FRAME DEFAULT-FRAME
DO:
  
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


&Scoped-define SELF-NAME StartDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StartDato C-Win
ON LEAVE OF StartDato IN FRAME DEFAULT-FRAME /* Start dato/tid */
DO:
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrwKampanjeHode
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
{incl/conttrigg.i oBrwKampanjeHode:BROWSE-HANDLE}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AngreAvbrytRecord C-Win 
PROCEDURE AngreAvbrytRecord :
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    IF oBrwKampanjeHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      IF oBrwKampanjeHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN
      AVBRYTKAMPANJE: 
      DO:
        IF AVAILABLE KampanjeHode THEN 
        DO:
          IF KampanjeHode.Aktivert = FALSE THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen er ikke aktivert!").
            RETURN.
          END.

          IF KampanjeHode.SluttDato < TODAY THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen er utgått og kan ikke deaktiveres!").
            RETURN.
          END.
        END.
                
        IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Deaktiver kampanje?") THEN 
          RETURN.
        pcTekst = ",KAMPANJE," + STRING(KampanjeHode.KampanjeId).  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
        oBrwKampanjeHode:refresh().
        oBrwKampanjeLinje:refresh().
        RUN DisplayRecord.
      END. /* AVBRYTKAMPANJE */            
      ELSE 
      DO:
        JBoxSession:Instance:ViewMessage('Bare en kampanje kan deaktiveres ad gangen.').
        RETURN.     
      END.
    END.
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage('Marker den kampanjen som skal deaktiveres.').
      RETURN.     
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AntallPaTilbudRecord C-Win 
PROCEDURE AntallPaTilbudRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF JBoxServerAPI:Instance:CallServerProc("kampanje_antallpatilbud.p","",?) THEN
    DO:
      pcRetParam = JBoxServerApi:Instance:getCallReturnParam().
      RUN KampanjeAktivePatilbud.w (INPUT-OUTPUT pcRetParam).
      IF pcRetParam = 'YES' THEN
      DO:
        
        /* Henter ny info til tilbuds som skal opprettes. Beskrivelse, dato, rabatt og tid. */
        ASSIGN
          cBeskrivelse  = 'Ny kampanje ' + STRING(NOW,"99/99/9999 HH:MM:SS")   
          plRab%        = 0
          dStartDato    = ?
          dSluttDato    = ?
          iAktiveresTid = 0
          iGyldigtiltid = (60 * 60 * 24) - 60
          cNotat        = ''
          bIgnorerNOS   = FALSE
          lKampanjePris = 0
          lKronerabatt  = 0
          lMinstePris   = 0
          iAvslagtype   = 1
          bNew          = TRUE
          .
        RUN kampanjeEndreAktiv.w (INPUT-OUTPUT cBeskrivelse,
          INPUT-OUTPUT plRab%,
          INPUT-OUTPUT dStartDato,
          INPUT-OUTPUT iAktiveresTid,
          INPUT-OUTPUT dSluttDato,
          INPUT-OUTPUT iGyldigTilTid,
          INPUT-OUTPUT cNotat,
          INPUT-OUTPUT bIgnorerNOS,
          INPUT-OUTPUT lKampanjePris,
          INPUT-OUTPUT lKronerabatt,
          INPUT-OUTPUT lMinstePris,
          INPUT-OUTPUT iAvslagType
          ).
        IF cBeskrivelse = 'AVBRYT' THEN
        DO:
          pcRetParam = ''.
          RETURN NO-APPLY.
        END.            
        ELSE DO:
            /* Oppretter ny kampanjerecord. */
            IF NOT JBoxServerAPI:Instance:CallServerProc("kampanje_opprettny.p",
                     STRING(plRab%) + '|' +
                     STRING(dStartDato) + '|' +
                     STRING(iAktiveresTid) + '|' +
                     STRING(dSluttDato) + '|' +
                     STRING(iGyldigTilTid) + '|' +
                     STRING(cNotat) + '|' +
                     STRING(bIgnorerNOS) + '|' +
                     STRING(lKampanjePris) + '|' +
                     STRING(lKronerabatt) + '|' +
                     STRING(lMinstePris) + '|' +
                     STRING(iAvslagType) + '|' + 
                     cBeskrivelse + '|' + 
                     STRING(iProfilNr),?) THEN
              DO:
                JBoxSession:Instance:ViewMessage("Feil opprettelse av kampanje: " + JBoxServerAPI:Instance:getCallMessage()).
                RETURN.
              END.
            /* Legger opp den nye kampanjen først i browser. */
            ELSE DO:
              iKampanjeId = INT(ENTRY(1,JBoxServerApi:Instance:getCallReturnParam(),'|')).
            END.
            /* Legger til varelinjene på kampanjen. */
            IF JBoxServerAPI:Instance:CallServerProc("kampanje_leggtilikkeaktive.p",STRING(iKampanjeId),?) THEN
            DO:              
              IF iAvslagType = 1 THEN
              DO:
                pcTekst = ",RAB%," + STRING(iKampanjeId) + ',' + STRING(ABS(plRab%) * -1) + ',' + JBoxSession:Instance:UserId.  
                IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
                  JBoxSession:Instance:ViewMessage("Feil pga (RAB%) " + JBoxServerAPI:Instance:getCallMessage()).              
              END.
              ELSE IF iAvslagType = 2 THEN 
              DO:
                pcTekst = ",KPRIS," + STRING(iKampanjeId) + ',' + STRING(lKampanjePris) + ',' + JBoxSession:Instance:UserId.  
                IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
                  JBoxSession:Instance:ViewMessage("Feil pga (KPRIS) " + JBoxServerAPI:Instance:getCallMessage()).              
              END.
              ELSE IF iAvslagType = 3 THEN 
              DO:
                pcTekst = ",KRRAB," + STRING(iKampanjeId) + ',' + STRING(lKroneRabatt) + ',' + JBoxSession:Instance:UserId.  
                IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
                  JBoxSession:Instance:ViewMessage("Feil pga (KRRAB) " + JBoxServerAPI:Instance:getCallMessage()).              
              END.
              JBoxSession:Instance:ViewMessage("Kampanje " + STRING(iKampanjeId) + " opprettet, og ikke aktiverte varer med lager er lagt til kampanjen.").
              oBrwKampanjeHode:openQuery().
/*              oBrwKampanjeLinje:openQuery().*/
            END.  
            ELSE   
              JBoxSession:Instance:ViewMessage("Feil opprettelse av kampanje: " + JBoxServerAPI:Instance:getCallMessage()).
          END.
        RETURN NO-APPLY.          
      END.
      ELSE 
        pcRetParam = ''.
    END.  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyRecord C-Win 
PROCEDURE CopyRecord :
DEFINE VARIABLE plKamp%      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE piKampanjeId AS INTEGER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE Kampanjehode THEN 
    DO:
      ASSIGN 
        plKamp%      = KampanjeHode.Kamp% * -1
        piKampanjeId = KampanjeHode.KampanjeId
        .        
      IF JBoxServerAPI:Instance:CallServerProc("Kampanjehode_kopier.p",
        STRING(KampanjeHode.KampanjeId) + '|' + JBoxSession:Instance:UserId + '|' + STRING(plKamp%),?) 
        THEN oBrwKampanjeHode:openQuery().
      ELSE JBoxSession:Instance:ViewMessage("Kopiering feilet!").        
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeleteRecord C-Win 
PROCEDURE DeleteRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeHode:isCurrent THEN 
    DO:
      IF AVAILABLE KampanjeHode THEN 
      DO:
        IF KampanjeHode.Kampanjehode_AktivTilbud = 1 THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Kampanjen inneholder varer som er aktive på kampanje. Kan ikke slettes.").
          RETURN.
        END.
      END.
    END.  
    ELSE IF otbKampanjeLinje:isCurrent THEN 
    DO:
      IF AVAILABLE KampanjeHode THEN 
      DO:
        IF KampanjeHode.Kampanjehode_AktivTilbud = 1 THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Kampanjen inneholder varer som er aktive på kampanje. Kan ikke ta bort varelinjer.").
          RETURN.
        END.
        IF KampanjeHode.Aktivert THEN 
        DO:
          JBoxSession:Instance:ViewMessage("Kampanjen er aktivert. Kan ikke ta bort varelinjer.").
          RETURN.
        END.
      END.
    END. 
    ELSE 
      RETURN. 
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeHode:isCurrent THEN 
    DO:
    END.  
    IF otbKampanjeLinje:isCurrent THEN 
    DO:
    END.  
  END.
  oBrwKampanjeHode:REFRESH().
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
IF oBrwKampanjeHode:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE KampanjeHode THEN  
    KAMPANJEHODEBLOKK:
    DO:
      slEkstraPrisProfiler:LIST-ITEMS = REPLACE(DYNAMIC-FUNCTION("getFieldList",
                                                         "KampanjeProfil;ProfilNr",
                                                         "WHERE KampanjeId = '" + STRING(KampanjeHode.KampanjeId) + "'"
                                                         ),'|',',').
      IF slEkstraPrisProfiler:LIST-ITEMS <> ? THEN 
        slEkstraPrisProfiler:BGCOLOR = 14.
      ELSE 
        slEkstraPrisProfiler:BGCOLOR = ?.
      IF KampanjeHode.Aktivert = TRUE THEN
      DO:
        otbKampanjeHode:disabledTools = "Edit,EndreKamp%,TilPrisko".
      END.
      ELSE 
      DO:
        otbKampanjeHode:disabledTools = "AngreAvbryt,Gjenbruk,EndreAktTilb".
      END.
      
      IF Kampanjehode.Kampanjehode_AktivTilbud = 1 AND KampanjeHode.Aktivert = TRUE THEN
        oBrwKampanjehode:BROWSE-HANDLE:TOOLTIP = 'Kampanjens aktikler er aktive på tilbud.'.
      ELSE IF Kampanjehode.Kampanjehode_AktivTilbud <> 1 AND KampanjeHode.Aktivert = TRUE THEN
          oBrwKampanjehode:BROWSE-HANDLE:TOOLTIP = 'Kampanjens kampanjeperiode er utgått.'.
        ELSE
          oBrwKampanjehode:BROWSE-HANDLE:TOOLTIP = ''.
          
    END. /* KAMPANJEHODEBLOKK */
  END. /* End frame blokk */
  
  RUN SUPER.
  
  RUN EndreAvslagtype.

/*  ASSIGN                                                        */
/*    ProfilNr:SENSITIVE IN FRAME {&FRAME-NAME}            = FALSE*/
/*    Kampanjehode_Endret:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE*/
/*    .                                                           */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DropFileNotifyBrowse C-Win 
PROCEDURE DropFileNotifyBrowse :
DEF VAR cFileNames AS CHAR NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:

    DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE bNyFil   AS LOG       NO-UNDO.
    
    IF SELF:NUM-DROPPED-FILES = 1 THEN 
    DO:
      ASSIGN 
        cFilNavn = SELF:GET-DROPPED-FILE(1).
      IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
        MESSAGE "Tillatte filtyper: '.xls,.xlsx'"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ELSE 
      DO:
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
        /*          ELSE IF cMsg <> '' THEN                  */
        /*            JBoxSession:Instance:ViewMessage(cMsg).*/

        /* Henter ny info til tilbuds som skal opprettes. Beskrivelse, dato, rabatt og tid. */
        ASSIGN
          bNyFil        = TRUE /* hantering av temptablar i LesExcel */
          cBeskrivelse  = IF itype = 1 THEN 
                              'Drop&Notify fra Salg% Aktiv kampanje'
                            ELSE IF itype = 2 THEN 
                              'Drop&Notify fra Solgt% lager'  
                            ELSE IF itype = 3 THEN 
                              'Drop&Notify fra PRSKampanje'  
                            ELSE IF itype =  4 THEN 
                              'Drop&Notify fra Lagerliste'
                            ELSE 
                              '** Ukjent filtype **'  
          plRab%        = 0
          dStartDato    = ?
          dSluttDato    = ?
          iAktiveresTid = 0
          iGyldigtiltid = (60 * 60 * 24) - 60
          cNotat        = ''
          bIgnorerNOS   = FALSE
          lKampanjePris = 0
          lKronerabatt  = 0
          lMinstePris   = 0
          iAvslagType   = 1
          .
        RUN kampanjeEndreAktiv.w (INPUT-OUTPUT cBeskrivelse,
          INPUT-OUTPUT plRab%,
          INPUT-OUTPUT dStartDato,
          INPUT-OUTPUT iAktiveresTid,
          INPUT-OUTPUT dSluttDato,
          INPUT-OUTPUT iGyldigTilTid,
          INPUT-OUTPUT cNotat,
          INPUT-OUTPUT bIgnorerNOS,
          INPUT-OUTPUT lKampanjePris,
          INPUT-OUTPUT lKronerabatt,
          INPUT-OUTPUT lMinstePris,
          INPUT-OUTPUT iAvslagType
          ).

        IF cBeskrivelse = '' THEN
        DO:
          JBoxSession:Instance:ViewMessage("Endring avbrutt!").
          RETURN.
        END.

        RUN NewRecord.
          
        /* Oppdatere skjermverdier. */
        ASSIGN
          Notat                          = 'Fil importert via Dropp&Notify.' + STRING(NOW,"99/99/9999 HH:MM:SS") + chr(10) +
                                       '  Filnavn     : ' + cFilNavn  + CHR(10) + 
                                       '  Ant. linjer : ' + STRING(iAntLinjer) + CHR(10) + 
                                       '  Importert av: ' + jBoxSession:Instance:UserId
          Notat:SCREEN-VALUE             = Notat
          fiKamp%                        = plRab%
          fiKamp%:SCREEN-VALUE           = STRING(plRab%)
          Beskrivelse                    = cBeskrivelse
          Beskrivelse:SCREEN-VALUE       = cBeskrivelse
          StartDato                      = dStartDato
          StartDato:SCREEN-VALUE         = STRING(dStartDato)
          SluttDato                      = dSluttDato
          SluttDato:SCREEN-VALUE         = STRING(dSluttDato)
          AktiveresTid_time              = STRING(iAktiveresTid,"HH:MM")
          AktiveresTid_time:SCREEN-VALUE = AktiveresTid_time
          GyldigTilTid_time              = STRING(igyldigTilTid,"HH:MM")
          GyldigTilTid_time:SCREEN-VALUE = GyldigTilTid_time
          ProfilNr:SCREEN-VALUE          = STRING(iProfilNr)
          ProfilNr:SENSITIVE             = FALSE
          Kampanjehode_Endret:SENSITIVE  = FALSE
          IgnorerNOS:SCREEN-VALUE = STRING(bIgnorerNOS) 
          KampanjePris:SCREEN-VALUE = STRING(lKampanjePris)
          Kronerabatt:SCREEN-VALUE = STRING(lKronerabatt)
          MistePris:SCREEN-VALUE = STRING(lMinstePris)
          AvslagType:SCREEN-VALUE = STRING(iAvslagType) 
          .

        APPLY "CHOOSE" TO save_tbKampanjeHode.
          
        /* Opprette kampanjelinjene på server. */ 
        IF CAN-FIND(FIRST ttImpFil) THEN 
        DO:
          httImpFilLinje = BUFFER ttImpFilLinje:HANDLE.
            
          /*            SESSION:SET-WAIT-STATE("GENERAL").*/ 
          JBoxServerAPI:Instance:CallServerProc("kampanje_import.p",
            STRING(KampanjeHode.KampanjeId) + '|' + STRING(iType) + '|' + STRING(KampanjeHode.ProfilNr) + '|' + cLogg,
            httImpFilLinje
            ).
          /*            SESSION:SET-WAIT-STATE("").*/
          oBrwKampanjeLinje:openQuery().  
        END.
          
      END.
    END.
    ELSE 
    DO:
      MESSAGE "Bare en fil er tillatt importer ad gangen!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ProfilNr:SENSITIVE = FALSE
      Kampanjehode_Endret:SENSITIVE = FALSE 
      Avslagtype:SENSITIVE = FALSE  
      . 
    IF bEndreAktivTilbud THEN 
      ASSIGN 
        Beskrivelse:SENSITIVE = FALSE
        StartDato:SENSITIVE = FALSE
        AktiveresTid_time:SENSITIVE = FALSE
        IgnorerNOS:SENSITIVE = FALSE
        Notat:SENSITIVE = FALSE
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
  DISPLAY ProfilNr AvslagType KampanjePris Kronerabatt fiKamp% MistePris Notat 
          Beskrivelse IgnorerNOS StartDato SluttDato slEkstraPrisProfiler 
          AktiveresTid_time GyldigTilTid_time fiTekst-2 Kampanjehode_Endret 
          fiTekst 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE first_tbKampanjeHode BrwKampanjeHode tbKampanjeHode btnSplitBarY 
         tbKampanjeLinje first_tbKampanjeLinje BrwKampanjeLinje 
         prev_tbKampanjeHode prev_tbKampanjeLinje next_tbKampanjeHode 
         next_tbKampanjeLinje last_tbKampanjeHode last_tbKampanjeLinje 
         new_tbKampanjeHode copy_tbKampanjeHode delete_tbKampanjeLinje 
         refresh_tbKampanjeLinje edit_tbKampanjeHode filter_tbKampanjeLinje 
         save_tbKampanjeHode excel_tbKampanjeLinje undo_tbKampanjeHode 
         LagerSalg_tbKampanjeLinje delete_tbKampanjeHode refresh_tbKampanjeHode 
         filter_tbKampanjeHode excel_tbKampanjeHode 
         LeggTilPrisprofil_tbKampanjeHode LeggTilVare_tbKampanjeLinje 
         EndreAktTilb_tbKampanjeHode KampanjeSalg_tbKampanjeLinje AvslagType 
         KampanjePris Kronerabatt fiKamp% MistePris AngreAvbryt_tbKampanjeHode 
         Notat Beskrivelse IgnorerNOS HentFraLager_tbKampanjeLinje 
         TilPrisko_tbKampanjeHode HentFraKampanje_tbKampanjeLinje 
         AntallPaTilbud_tbKampanjeHode StartDato SluttDato slEkstraPrisProfiler 
         AktiveresTid_time GyldigTilTid_time Kampanjehode_Endret 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreAktTilbRecord C-Win 
PROCEDURE EndreAktTilbRecord :
DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      bEndreAktivTilbud = FALSE 
      .
    IF oBrwKampanjeHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      IF oBrwKampanjeHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN 
      DO:
        IF AVAILABLE KampanjeHode THEN
        AVAILKAMPANJEHODE: 
        DO:
          IF KampanjeHode.Aktivert = FALSE THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen er ikke aktivert tidligere! Kan ikke endres.").
            RETURN.
          END.

          IF KampanjeHode.SluttDato < TODAY THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen er avsluttet! Kan ikke endres.").
            RETURN.
          END.
          IF KampanjeHode.SluttDato = TODAY THEN 
          DO:
            IF KampanjeHode.GyldigTilTid < TIME THEN 
            DO:
              JBoxSession:Instance:ViewMessage("Kampanjen er avsluttet! Kan ikke endres.").
              RETURN.
            END.
          END.

          IF KampanjeHode.StartDato > TODAY THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen er aktivert, men har ikke startet. Bruk deaktiver funksjonen, rediger kampanjen og aktiver den på nytt.").
            RETURN.
          END.
          IF KampanjeHode.StartDato = TODAY THEN 
          DO:
            IF KampanjeHode.AktiveresTid > TIME THEN 
            DO:
              JBoxSession:Instance:ViewMessage("Kampanjen er aktivert, men har ikke startet. Bruk deaktiver funksjonen, rediger kampanjen og aktiver den på nytt.").
              RETURN.
            END.
          END.

          /* Her er det nå bare pågående kampanjer som behandles.                              */
          /* På disse skal - avhengig av type - rabatt, kampanjepris eller kronerabatt endres. */
          /* Dette endres direkte i skjermbildet, og linjene oppdateres deretter direkte mot   */
          /* den aktive kampanjeprisen.                                                        */
          ASSIGN 
            bEndreAktivTilbud = TRUE
            .
          APPLY "CHOOSE" TO edit_tbKampanjeHode.
                    
        END. /* AVAILKAMPANJEHODE */
      END.            
      ELSE 
      DO:
        JBoxSession:Instance:ViewMessage('Bare en kampanje kan endres ad gangen.').
        RETURN.     
      END.
    END.
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage('Marker den kampanjen som skal endres.').
      RETURN.     
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EndreAvslagtype C-Win 
PROCEDURE EndreAvslagtype :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
      DO WITH FRAME {&FRAME-NAME}:
/*        IF AVAILABLE KampanjeHode THEN*/
        DO:
          IF INT(AvslagType:SCREEN-VALUE) = 1 THEN 
          DO:
            ASSIGN 
              fiKamp%:HIDDEN      = FALSE
              KampanjePris:HIDDEN = TRUE
              Kronerabatt:HIDDEN  = TRUE 
              MistePris:HIDDEN   = TRUE
              .
          END.
          ELSE IF INT(AvslagType:SCREEN-VALUE) = 2 THEN
          DO:
            ASSIGN 
              fiKamp%:HIDDEN      = TRUE
              KampanjePris:HIDDEN = FALSE
              Kronerabatt:HIDDEN  = TRUE 
              MistePris:HIDDEN   = TRUE
              .
          END.
          ELSE IF INT(AvslagType:SCREEN-VALUE) = 3 THEN
            ASSIGN 
              fiKamp%:HIDDEN      = TRUE
              KampanjePris:HIDDEN = TRUE
              Kronerabatt:HIDDEN  = FALSE 
              MistePris:HIDDEN   = FALSE
              .
          END.
      END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentFraKampanjeRecord C-Win 
PROCEDURE HentFraKampanjeRecord :
DO WITH FRAME {&FRAME-NAME}:
  
  IF NOT AVAILABLE KampanjeHode THEN 
    RETURN.
  IF KampanjeHode.Aktivert THEN 
  DO:
    JBoxSession:Instance:ViewMessage('Kampanjen er aktivert og kan ikke endres.').    
    RETURN.
  END.  
  ASSIGN 
    cRowIdList = ''
    cIdList    = ''
    .

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "Kampanjehode;KampanjeId;Beskrivelse;Aktivert||*/",
                      "WHERE TRUE",
                      INPUT-OUTPUT cRowIdList,
                      "KampanjeId",
                      INPUT-OUTPUT cIdList,
                      "",
                      "",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  
  IF cIdList <> '' THEN 
  DO iLoop = 1 TO NUM-ENTRIES(cIdList,'|'):
    DO:
      IF INTEGER(ENTRY(iLoop,cIdList,'|')) = KampanjeHode.KampanjeId THEN 
        NEXT.
         
      JBoxServerAPI:Instance:CallServerProc("Kampanjehode_kopier.p",
        STRING(KampanjeHode.KampanjeId) + '|' + JBoxSession:Instance:UserId + '|0|' + ENTRY(iLoop,cIdList,'|'),?).
    END.
  END.
  IF KampanjeHode.Avslagtype = 1 THEN
  DO:
    pcTekst = ",RAB%," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.Kamp%) + ',' + JBoxSession:Instance:UserId.  
    IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
  END.
  ELSE IF KampanjeHode.AvslagType = 2 THEN 
  DO:
    pcTekst = ",KPRIS," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KampanjePris) + ',' + JBoxSession:Instance:UserId.  
    IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
  END.
  ELSE IF KampanjeHode.AvslagType = 3 THEN 
  DO:
    pcTekst = ",KRRAB," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KroneRabatt) + ',' + JBoxSession:Instance:UserId.  
    IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
      JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
  END.
  oBrwKampanjeHode:refresh().
/*  oBrwKampanjeLinje:refresh().*/
/*  JBoxSession:Instance:ViewMessage(JBoxServerAPI:Instance:getCallMessage()).*/
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentFraLagerRecord C-Win 
PROCEDURE HentFraLagerRecord :
DEFINE VARIABLE pbOk    AS LOG       NO-UNDO.
  DEFINE VARIABLE pcVgLst AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcVmLst AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT AVAILABLE KampanjeHode THEN 
      RETURN.
    IF KampanjeHode.SluttDato < TODAY THEN 
    DO:
      JBoxSession:Instance:ViewMessage("Kampanje er avsluttet. Artikler kan ikke legges til").
      RETURN.
    END.   

    IF KampanjeHode.Aktivert THEN 
    DO:
      IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal artikler som har lager og ikke er lagt inn på kampanjen fra tidligere hentes inn i kampanjen?" + CHR(10) + CHR(10) + 
        "Kampanjen er allerede aktivert!!" + CHR(10) + "ADVARSEL - Varene vil bli aktivert på kampanje umiddelbart ADVARSEL!!!") THEN 
        RETURN.
    END.
    /*    ELSE IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Skal artikler som har lager og ikke er lagt inn på kampanjen fra tidligere hentes inn i kampanjen?") THEN*/
    /*        RETURN.                                                                                                                                                     */
        
    /* Legger inn kriterier for avgrensing av varer som skal legges inn. */
    RUN KampanjeDialogIkkeAktiverte.w (OUTPUT pbOk, OUTPUT pcVgLst, OUTPUT pcVmLst, OUTPUT pbAktive).
    IF pbOk <> TRUE THEN 
      RETURN.    
        
    pcVgLst = REPLACE(pcVgLst,'|',',').
    pcVmLst = REPLACE(pcVmLst,'|',',').     
    
    /*    MESSAGE                    */
    /*      'pcVgLst '  pcVgLst  SKIP*/
    /*      'pcVmLst '  pcVmLst  SKIP*/
    /*      'pbAktive'  pbAktive SKIP*/
    /*    VIEW-AS ALERT-BOX.         */
    IF NOT JBoxServerAPI:Instance:CallServerProc("kampanje_leggtilikkeaktive.p",STRING(KampanjeHode.KampanjeId) + '|' + pcVgLst + '|' + pcVmLst + '|' + STRING(pbAktive),?) THEN
      JBoxSession:Instance:ViewMessage("Feil ved supplering av artikler på kampanjen. Feil: " + JBoxServerAPI:Instance:getCallMessage()).
    ELSE DO:     
      IF KampanjeHode.Avslagtype = 1 THEN
      DO:
        pcTekst = ",RAB%," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.Kamp%) + ',' + JBoxSession:Instance:UserId.  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
      END.
      ELSE IF KampanjeHode.AvslagType = 2 THEN 
      DO:
        pcTekst = ",KPRIS," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KampanjePris) + ',' + JBoxSession:Instance:UserId.  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
      END.
      ELSE IF KampanjeHode.AvslagType = 3 THEN 
      DO:
        pcTekst = ",KRRAB," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KroneRabatt) + ',' + JBoxSession:Instance:UserId.  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
      END.
      
      /* Er kampanjen allerede aktivert, og linjer lagt til, skal de nye linjene også aktiveres. */
      IF KampanjeHode.Aktivert THEN 
        oBrwKampanjeHode:processRowsNoMessage("kampanjehode_aktiver.p", "").
               
      oBrwKampanjeHode:refreshRow().
      oBrwKampanjeLinje:openQuery().
    END.  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE importerFil C-Win 
PROCEDURE importerFil :
/*------------------------------------------------------------------------------
   Purpose:
   Notes: RUN importerFil (INPUT cFilNavn, OUTPUT iAntLinjer). /* Les inn filen i en temp tabell/Datasett. */
     
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcFilNavn AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pianLinjer AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER piType AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
  DEFINE OUTPUT PARAMETER pcMsg AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE pcLinje   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcTempFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cJSonFil  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType1    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType2    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType3    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cType4    AS CHARACTER NO-UNDO.

  SESSION:SET-WAIT-STATE("GENERAL").
  
  ASSIGN 
    /* Salg% aktiv kampanje */ 
    cType1 = "Varetekst;Lev.artikkelnr;Lev.farkgekode;Sesong;TilbudFraDato;TilbudTilDato;Pris"
    /* Solgt% lager         */ 
    cType2 = 'Varetekst;Lev.artikkelnr;Lev.fargekode;Sesong;Pris;Solgt%;Solgt'
    /* PRSKampanje import   */ 
    cType3 = 'RecType;KampanjeId;Beskrivelse;StartDato;StartTid;SluttDato;SluttTid;(K)amp(N)ormal;ProfilNr;Rab%'
    /* Lagerliste           */ 
    cType4 = 'Butikk;Varemerke;'
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
    /*    IF CAN-FIND(FIRST ttImpFil) THEN                        */
    /*      DATASET dsttImpFil:WRITE-JSON("file", cJSonFil, TRUE).*/
      
    rclStandardFunksjoner:SkrivTilLogg(cLogg, 'Kampanje - Importert fil ' + pcTempFil).
    rclStandardFunksjoner:SkrivTilLogg(cLogg, 'Kampanje - JSonFil   fil ' + cJSonFil).
    
    FIND FIRST ttImpFilLinje WHERE 
      ttImpFilLinje.LinjeNr = 1 NO-ERROR.
    IF AVAILABLE ttImpFilLinje THEN 
    DO:      
      
      /*MESSAGE*/
      /*'Type1' ttImpfilLinje.Record BEGINS cType1 SKIP   */
      /*'Type2' ttImpfilLinje.Record BEGINS cType2 SKIP   */
      /*'Type3' ttImpfilLinje.Record BEGINS cType3 SKIP   */
      /*'Type4' ttImpfilLinje.Record BEGINS cType4 SKIP(1)*/
      /*'Type1' cType1 SKIP(1)                            */
      /*'Type2' cType2 SKIP(1)                            */
      /*'Type3' cType3 SKIP(1)                            */
      /*'Type4' cType4 SKIP(1)                     */
      /*'ttImpfilLinje.Record' ttImpfilLinje.Record*/
      /*VIEW-AS ALERT-BOX.                         */
      /*      OUTPUT TO value('konv\heading.txt').*/
      /*      PUT UNFORMATTED                     */
      /*        ttImpfilLinje.Record              */
      /*        SKIP.                             */
      /*      OUTPUT CLOSE.                       */

      IF ttImpfilLinje.Record BEGINS cType1 THEN 
        piType = 1.
      ELSE IF ttImpfilLinje.Record BEGINS cType2 THEN 
          piType = 2.
        ELSE IF ttImpfilLinje.Record BEGINS cType3 THEN 
            piType = 3.
          ELSE IF ttImpfilLinje.Record BEGINS cType4 THEN 
              piType = 4.

      rclStandardFunksjoner:SkrivTilLogg(cLogg, '  Record: ' + ttImpfilLinje.Record).
      CASE pitype:
        WHEN 1 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType1: ' + cType1).
        WHEN 2 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType2: ' + cType2).
        WHEN 3 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType3: ' + cType3).
        WHEN 4 THEN rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType4: ' + cType3).
        OTHERWISE 
        DO:
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  Ingen match på type.').
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType1: ' + cType1).
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType2: ' + cType2).
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType3: ' + cType3).
          rclStandardFunksjoner:SkrivTilLogg(cLogg, '  cType4: ' + cType3).
        END.
      END CASE.

    END.
    
    pianLinjer = 0.
    FOR EACH ttImpFilLinje:
      pianLinjer = pianLinjer + 1.
    END.
    
    IF CAN-DO('1,2,3,4',STRING(piType)) THEN 
      ASSIGN 
        pbOk  = TRUE 
        pcMsg = 'Fil ' + pcFilNavn + ' importert i nyopprettet kampanje.'
        .
    ELSE 
      ASSIGN 
        pbOk  = FALSE 
        pcMsg = '** Feil FORMAT på fil. Kan ikke importeres.'
        .
  END. /* IMPORTER */
  ELSE 
  DO:
    ASSIGN 
      pbOk  = FALSE 
      pcMsg = '** Finner ikke filen ' + pcFilNavn + ' '.
    .
  END.
  
  SESSION:SET-WAIT-STATE("").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
DO WITH FRAME {&FRAME-NAME}:


    oStartDato_JBoxDevExEdit = NEW JBoxDevExDateEdit(THIS-PROCEDURE,StartDato:HANDLE).
    oStartDato_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
    oStartDato_JBoxDevExEdit:CreateDisplayLink(oFmKampanjeHode:BUFFER-HANDLE,'StartDato').

    oSluttDato_JBoxDevExEdit = NEW JBoxDevExDateEdit(THIS-PROCEDURE,SluttDato:HANDLE).
    oSluttDato_JBoxDevExEdit:RegisterWithJukeBox(YES). /* YES: Visible */
    oSluttDato_JBoxDevExEdit:CreateDisplayLink(oFmKampanjeHode:BUFFER-HANDLE,'SluttDato').
  
    IF JBoxServerAPI:Instance:Find("SysPara", "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 3") THEN
      iButNr = INT(ENTRY(1,JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1"))).
    IF JBoxServerAPI:Instance:Find("Butiker", "WHERE Butik = '" + STRING(iButNr) + "'") THEN
      iProfilNr = INT(ENTRY(1,JBoxServerAPI:Instance:FieldValue("Butiker.ProfilNr"))).
  
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
  SUBSCRIBE "msgFraLagerListeButArtStr" ANYWHERE.
  SUBSCRIBE "OpenQueryKampanjeLinje" ANYWHERE.
  SUBSCRIBE "msgNyttKampanjeId" ANYWHERE.
  
&IF "{&WINDOW-SYSTEM}" NE "TTY" &THEN  
  RUN enable_UI.

  oContainer = NEW JBoxContainer().
  oContainer:addStatusBar().
&ENDIF

  DO WITH FRAME {&FRAME-NAME}:
    oBrwKampanjeHode = NEW JBoxBrowse(brwKampanjeHode:HANDLE).

    oFmKampanjeHode = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
    oBrwKampanjeHode:viewRecordCount = NO.
  
    oFmKampanjeHode:updateFields = 'ProfilNr,AvslagType,Beskrivelse,Notat,fiKamp%,Kronerabatt,KampanjePris,MistePris,IgnorerNOS,StartDato,SluttDato,AktiveresTid_Time,GyldigTilTid_Time,Kampanjehode_Endret'.
/*    oFmKampanjeHode:displayFields = ''.*/
    oFmKampanjeHode:primaryKeyFields = 'KampanjeId'.

    oFmKampanjeHode:BROWSE-OBJECT = oBrwKampanjeHode.
    otbKampanjeHode = NEW JBoxToolbar(tbKampanjeHode:HANDLE).
    
    oBrwKampanjeHode:TOOLBAR-OBJECT = otbKampanjeHode.
    oFmKampanjeHode:TOOLBAR-OBJECT = otbKampanjeHode.    
    oContainer:setAddMoveX("Profilnr,Beskrivelse,AvslagType,Notat,StartDato,SluttDato,fiKamp%,Kronerabatt,KampanjePris,MistePris,IgnorerNOS,AktiveresTid_time,GyldigTilTid_time,Kampanjehode_Endret,tgNOS,slEkstraPrisProfiler,fiTekst,fiTekst-2").
  
    oBrwKampanjeLinje = NEW JBoxBrowse(brwKampanjeLinje:HANDLE).
    oBrwKampanjeLinje:setParentBrowseObject(oBrwKampanjeHode,"KampanjeId").
    otbKampanjeLinje = NEW JBoxToolbar(tbKampanjeLinje:HANDLE).
    oBrwKampanjeLinje:TOOLBAR-OBJECT = otbKampanjeLinje.
    oBrwKampanjeLinje:rowsToBatch = 1000000.
    oBrwKampanjeLinje:viewRecordCount = YES.  
  
    RUN InitializeComponents.

    hAktivertColumn = oBrwKampanjehode:getColumnHandle("Aktivert").
    hKampanjehode_AktivTilbudColumn = oBrwKampanjeHode:getColumnHandle("Kampanjehode_AktivTilbud").
   
    oContainer:setSplitBarY(btnSplitBarY:HANDLE).
    oContainer:setSplitBarYlimits(btnSplitBarY:HANDLE,200,300). /* 200 pixels from the top, 300 pixels from the bottom */
    oContainer:setFollowSplitBarY(STRING(BrwKampanjeHode:HANDLE) + ',' + 
      STRING(BrwKampanjeLinje:HANDLE) + ',' +
      STRING(tbKampanjeLinje:HANDLE) + ',' +
      STRING(first_tbKampanjeLinje:HANDLE) + ',' +
      STRING(prev_tbKampanjeLinje:HANDLE) + ',' +
      STRING(next_tbKampanjeLinje:HANDLE) + ',' +
      STRING(last_tbKampanjeLinje:HANDLE) + ',' +
      STRING(filter_tbKampanjeLinje:HANDLE) + ',' +
      STRING(refresh_tbKampanjeLinje:HANDLE) + ',' +
      STRING(excel_tbKampanjeLinje:HANDLE) + ',' +
      STRING(delete_tbKampanjeLinje:HANDLE) + ',' +
      STRING(LeggTilVare_tbKampanjeLinje:HANDLE) + ',' +
      STRING(KampanjeSalg_tbKampanjeLinje:HANDLE) + ',' +
      STRING(LagerSalg_tbKampanjeLinje:HANDLE) + ',' +
      STRING(HentFraLager_tbKampanjeLinje:HANDLE) + ',' +
      STRING(HentFraKampanje_tbKampanjeLinje:HANDLE) + ',' +
      /*    STRING(Beskrivelse:HANDLE) + ',' +*/
      /*    STRING(Kamp%:HANDLE) + ',' +      */
      STRING(Notat:HANDLE)    
      ).
    oContainer:setNoResizeY("BrwKampanjeHode,Beskrivelse,Kamp%,Notat,tgNOS,Kampanjehode_Endret,slEkstraPrisProfiler,fiTekst,fitekst-2").
    oContainer:setNoResizeX("Notat,slEkstraPrisProfiler").
    oBrwKampanjeHode:postUpdateProc = "kampanjehode_post_update.p".
    oBrwKampanjeHode:baseQuery = "WHERE ProfilNr = '" + STRING(iProfilNr) + "'".
    oBrwKampanjehode:setQuerySort('KampanjeId;DESC').
    oBrwKampanjeLinje:setQuerySort('KampanjeId,KampanjeLinje;DESC').
  
    oBrwKampanjeHode:customDeleteValProc = "kampanjehode_post_delete.p". /* Fjerner vlaidering på om det ligger Ovbuffer poster under PkSdlHode. */

    hKampanjeLinje_NOSColumn = oBrwKampanjelinje:getColumnHandle("Kampanjelinje_NOS").
  
    opopupKampanjeLinje = NEW JBoxPopupMenu().
    opopupKampanjeLinje:AddToolGroup('OppslagModell;Vis i modell liste').  
    oBrwKampanjeLinje:POPUP-MENU-OBJECT = opopupKampanjeLinje.
    slEkstraPrisProfiler:SENSITIVE = FALSE.
  END.
  oBrwKampanjeHode:OpenQuery().

  RUN DisplayRecord.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KampanjeSalgRecord C-Win 
PROCEDURE KampanjeSalgRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeLinje:isCurrent THEN 
    DO:
      IF AVAILABLE KampanjeHode THEN
      RAD: 
      DO:
        IF KampanjeHode.aktivert = TRUE THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Kampanjen er akivert og kan ikke endres. ").
          RETURN.
        END.
        IF VALID-HANDLE(rKampanjeSalg) THEN 
        DO:
          RUN MoveToTop IN rKampanjeSalg.
        END.
        ELSE DO:
          rKampanjeSalg = jboxmainmenu:Instance:StartChildWindow('KampanjeSalg.w', 
                                      'Solg% aktiv kampanje',
                                      NO).
          IF VALID-HANDLE(rKampanjeSalg) THEN 
          DO:
            IF VALID-HANDLE(rKampanjeSalg) THEN
              RUN setKampanjeId IN rKampanjeSalg (KampanjeHode.KampanjeId).
          END.
        END.

/*        CURRENT-WINDOW:SENSITIVE = FALSE.                                     */
/*        ASSIGN                                                                */
/*          cVareFelt    = ''                                                   */
/*          cVareVerdier = ''                                                   */
/*          .                                                                   */
/*        RUN KampanjeSalg.w (INPUT "16|20|" + STRING(KampanjeHode.KampanjeId)).*/
/*        CURRENT-WINDOW:SENSITIVE = TRUE.                                      */
/*        IF cVareFelt = '' OR cVarefelt = ? THEN                               */
/*          RETURN.                                                             */
/*        ELSE                                                                  */
/*          oBrwKampanjeLinje:openQuery().                                      */
        RETURN.
      END. /* RAD */
    END.  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagerSalgRecord C-Win 
PROCEDURE LagerSalgRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeLinje:isCurrent THEN 
    DO:
      IF AVAILABLE KampanjeHode THEN
      RAD: 
      DO:
        IF KampanjeHode.aktivert = TRUE THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Kampanjen er akivert og kan ikke endres. ").
          RETURN.
        END.
        
        IF VALID-HANDLE(rLagerSalgHandle) THEN 
        DO:
          RUN MoveToTop IN rLagerSalgHandle.
        END.
        ELSE DO:
          cProfilLst = ProfilNr:SCREEN-VALUE + (IF slEkstraPrisProfiler:LIST-ITEMS <> ? THEN ',' + slEkstraPrisProfiler:LIST-ITEMS ELSE '').
          rLagerSalgHandle = jboxmainmenu:Instance:StartChildWindow('SalgLager.w', 
                                      'Solg% lager',
                                      '16|20|' + REPLACE(cProfilLst,',','¤') + '|' + STRING(KampanjeHode.KampanjeId),
                                      ?,
                                      NO).
          IF VALID-HANDLE(rLagerSalgHandle) THEN 
            RUN MoveToTop IN rLagerSalgHandle.
        END.
        RETURN.
      END. /* RAD */
    END.  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggTilPrisprofilRecord C-Win 
PROCEDURE LeggTilPrisprofilRecord :
DO WITH FRAME {&FRAME-NAME}:
  IF NOT AVAILABLE KampanjeHode THEN 
    RETURN.
  IF KampanjeHode.Aktivert THEN 
  DO:
    JBoxSession:Instance:ViewMessage('Kampanjen er aktivert og kan ikke endres.').    
    RETURN.
  END.  

  ASSIGN 
    cRowIdList = ''
    cIdList    = ''
    .

  cIdList = DYNAMIC-FUNCTION("getFieldList",
                             "KampanjeProfil;ProfilNr",
                             "WHERE KampanjeId = '" + STRING(KampanjeHode.KampanjeId) + "'"
                             ).
  IF cIdList <> '' THEN 
  DO:
    IF JBoxServerAPI:Instance:CallServerProc("kampanjehode_rowidliste.p",cIdList,?) THEN
      cRowIdList = JBoxServerApi:Instance:getCallReturnParam().
    ELSE 
      cIdList = ''.
  END.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxSelector.w (THIS-PROCEDURE,0,
                      "PrisProfil;ProfilNr;Beskrivelse;KortNavn",
                      "WHERE ProfilNr <> '" + STRING(KampanjeHode.ProfilNr) + "'",
                      INPUT-OUTPUT cRowIdList,
                      "ProfilNr",
                      INPUT-OUTPUT cIdList,
                      "",
                      "",
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF cIdList <> '' THEN
  DO iLoop = 1 TO NUM-ENTRIES(cIdList,'|'):
    DO:
      JBoxServerAPI:Instance:CallServerProc("kampanjehode_tagprofil.p",STRING(KampanjeHode.KampanjeId) + '|' + REPLACE(cIdList,'|',','),?).
      slEkstraPrisProfiler:LIST-ITEMS = REPLACE(DYNAMIC-FUNCTION("getFieldList",
                                                         "KampanjeProfil;ProfilNr",
                                                         "WHERE KampanjeId = '" + STRING(KampanjeHode.KampanjeId) + "'"
                                                         ),'|',',').
    END.
  END.
  ELSE DO:
    JBoxServerAPI:Instance:CallServerProc("kampanjehode_tagprofil.p",STRING(KampanjeHode.KampanjeId) + '|',?).
    slEkstraPrisProfiler:LIST-ITEMS = ?.
  END.
  
  IF slEkstraPrisProfiler:LIST-ITEMS <> ? THEN 
    slEkstraPrisProfiler:BGCOLOR = 14.
  ELSE 
    slEkstraPrisProfiler:BGCOLOR = ?.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeggTilVareRecord C-Win 
PROCEDURE LeggTilVareRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeLinje:isCurrent THEN 
    DO:
      IF AVAILABLE KampanjeHode THEN
      RAD: 
      DO:
        IF KampanjeHode.aktivert = TRUE THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Kampanjen er akivert og kan ikke endres. ").
          RETURN.
        END.
        
        IF VALID-HANDLE(rModellListeHandle) THEN 
        DO:
          RUN MoveToTop IN rModellListeHandle.
        END.
        ELSE DO:
          cProfilLst = ProfilNr:SCREEN-VALUE + (IF slEkstraPrisProfiler:LIST-ITEMS <> ? THEN ',' + slEkstraPrisProfiler:LIST-ITEMS ELSE '').
          rModellListeHandle = jboxmainmenu:Instance:StartChildWindow('LagerListeModButStr.w', 
                                      'Modelliste',
                                       NO).
          IF VALID-HANDLE(rModellListeHandle) THEN 
          DO:
            IF VALID-HANDLE(rModellListeHandle) THEN
              RUN setKampanjeId IN rModellListeHandle (INPUT KampanjeHode.KampanjeId).
          END.
        END.
        RETURN.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msgNyttKampanjeId C-Win 
PROCEDURE msgNyttKampanjeId :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER piKampanjeId AS INTEGER NO-UNDO.
  
  ASSIGN 
    iKampanjeId = piKampanjeId
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeHode:isCurrent THEN 
    DO:    
      /* Henter ny info til tilbuds som skal opprettes. Beskrivelse, dato, rabatt og tid. */
      ASSIGN
        cBeskrivelse  = 'Ny kampanje ' + STRING(NOW,"99/99/9999 HH:MM:SS")   
        plRab%        = 0
        dStartDato    = ?
        dSluttDato    = ?
        iAktiveresTid = 0
        iGyldigtiltid = (60 * 60 * 24) - 60
        cNotat        = ''
        bIgnorerNOS   = FALSE
        lKampanjePris = 0
        lKronerabatt  = 0
        lMinstePris   = 0
        iAvslagtype   = 1
        bNew          = TRUE
        .
      /*
      RUN kampanjeEndreAktiv.w (INPUT-OUTPUT cBeskrivelse,
        INPUT-OUTPUT plRab%,
        INPUT-OUTPUT dStartDato,
        INPUT-OUTPUT iAktiveresTid,
        INPUT-OUTPUT dSluttDato,
        INPUT-OUTPUT iGyldigTilTid,
        INPUT-OUTPUT cNotat,
        INPUT-OUTPUT bIgnorerNOS,
        INPUT-OUTPUT lKampanjePris,
        INPUT-OUTPUT lKronerabatt,
        INPUT-OUTPUT lMinstePris,
        INPUT-OUTPUT iAvslagType
        ).
      IF cBeskrivelse = 'AVBRYT' THEN
      DO:
/*        JBoxSession:Instance:ViewMessage("Endring avbrutt!").*/
        RETURN.
      END.
      */
    END.  
    
    IF otbKampanjeLinje:isCurrent THEN 
    DO:
      IF AVAILABLE KampanjeHode THEN
      RAD: 
      DO:
        IF KampanjeHode.aktivert = TRUE THEN
        DO: 
          JBoxSession:Instance:ViewMessage("Kampanjen er akivert og kan ikke endres. ").
          RETURN.
        END.
        CURRENT-WINDOW:SENSITIVE = FALSE.
        ASSIGN 
          cVareFelt    = ''
          cVareVerdier = ''
          .
        RUN LagerListeButArtStr.w ('16|45||' + STRING(KampanjeHode.KampanjeId)).
        CURRENT-WINDOW:SENSITIVE = TRUE.
        IF cVareFelt = '' OR cVarefelt = ? THEN
          RETURN. 
        ELSE 
          oBrwKampanjeLinje:openQuery().
        RUN SaveRecord.
        RETURN.
      END. /* RAD */
    END.  
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF otbKampanjeHode:isCurrent THEN 
    DO:
      /* Oppdatere skjermverdier. */
      ASSIGN
        fiKamp% = plRab%
        lKamp% = fiKamp% * -1
        /*
        StartDato = dStartDato
        SluttDato = dSluttDato
        AktiveresTid_time = STRING(iAktiveresTid,"HH:MM")
        GyldigTilTid_time = STRING(igyldigTilTid,"HH:MM")
        AktiveresTid = iAktiveresTid
        GyldigTilTid = igyldigTilTid
        AvslagType = iAvslagType
        IgnorerNOS = bIgnorerNOS
        ProfilNr = iProfilNr
        KampanjePris = lKampanjePris
        KroneRabatt = lKroneRabatt
        Minstepris = lMinstePris
        Beskrivelse = cBeskrivelse
        Notat =  'Ny kampanje registrert av '+ JBoxSession:Instance:UserId + ' ' + STRING(NOW,"99/99/9999 HH:MM:SS")
        */
        .
      /*
      DISPLAY 
        Avslagtype
        GyldigTilTid_time
        AktiveresTid_time
        SluttDato        
        StartDato
        fiKamp%
        IgnorerNOS
        ProfilNr
        KampanjePris
        KroneRabatt
        Minstepris
        Beskrivelse
        Notat
      WITH FRAME {&FRAME-NAME}.
      */
      ASSIGN 
        Avslagtype:SCREEN-VALUE = STRING(iAvslagType) 
        AktiveresTid_time:SCREEN-VALUE = STRING(iAktiveresTid,"HH:MM")
        GyldigTilTid_time:SCREEN-VALUE = STRING(igyldigTilTid,"HH:MM")
        SluttDato:SCREEN-VALUE = STRING(dStartDato)        
        StartDato:SCREEN-VALUE = STRING(dSluttDato)
        fiKamp%:SCREEN-VALUE = STRING(fiKamp%)
        IgnorerNOS:SCREEN-VALUE = IF bIgnorerNOS THEN 'YES' ELSE 'NO'
        ProfilNr:SCREEN-VALUE = STRING(iProfilNr)
        KampanjePris:SCREEN-VALUE = STRING(lKampanjePris)
        KroneRabatt:SCREEN-VALUE = STRING(lKroneRabatt)
        MistePris:SCREEN-VALUE = STRING(lMinstepris)
        Beskrivelse:SCREEN-VALUE = cBeskrivelse
        Notat:SCREEN-VALUE = 'Ny kampanje registrert av '+ JBoxSession:Instance:UserId + ' ' + STRING(NOW,"99/99/9999 HH:MM:SS")
      .
      RUN EndreAvslagtype.
      ASSIGN 
        ProfilNr:SENSITIVE = FALSE
        .
    END.  
    IF otbKampanjeLinje:isCurrent THEN 
    NYRAD:
    DO:
    END. /* NYRAD */ 

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryKampanjeLinje C-Win 
PROCEDURE OpenQueryKampanjeLinje :
/*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  oBrwKampanjeLinje:openQuery().
  
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
    IF AVAILABLE KampanjeLinje THEN 
    DO:
      
      /* FILTER:LevKod=XX,LevFargKod=XX */
      ASSIGN 
        cFilterTekst = 'FILTER:LevKod=&LevKod,LevFargKod=&LevFargKod'
        cFilterTekst = REPLACE(cFilterTekst,'&LevKod',KampanjeLinje.KampanjeLinje_LevKod)
        cFilterTekst = REPLACE(cFilterTekst,'&LevFargKod',KampanjeLinje.KampanjeLinje_LevFargKod)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE Kampanjehode THEN
    DO:
      IF KampanjeHode.aktivert = TRUE THEN
        hAktivertColumn:BGCOLOR = ?.
      ELSE
        hAktivertColumn:BGCOLOR = 10.
      IF KampanjeHode.KampanjeHode_AktivTilbud = 1
        THEN hKampanjehode_AktivTilbudColumn:BGCOLOR = 12.
      ELSE 
        hKampanjehode_AktivTilbudColumn:BGCOLOR = ?.
    END.
    IF AVAILABLE KampanjeLinje THEN
    DO:
      IF KampanjeLinje.KampanjeLinje_NOS MATCHES '*NOS*' THEN
        ASSIGN hKampanjeLinje_NOSColumn:BGCOLOR = 12 NO-ERROR.
      ELSE IF KampanjeLinje.KampanjeLinje_NOS <> '' THEN
          ASSIGN hKampanjeLinje_NOSColumn:BGCOLOR = 10 NO-ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF AVAILABLE kampanjeHode THEN 
      ASSIGN 
        lOrgKamp% = KampanjeHode.Kamp%
        lOrgKampanjePris = KampanjeHode.KampanjePris
        lOrgKroneRabatt = KampanjeHode.KroneRabatt
        .
    ASSIGN 
      iAktiveresTid = INT(ENTRY(1,AktiveresTid_time:SCREEN-VALUE,':')) * 3600 +
                    INT(ENTRY(2,AktiveresTid_time:SCREEN-VALUE,':')) * 60 
      iGyldigTilTid = INT(ENTRY(1,GyldigTilTid_time:SCREEN-VALUE,':')) * 3600 +
                    INT(ENTRY(2,GyldigTilTid_time:SCREEN-VALUE,':')) * 60
      lKamp%        = INT(fiKamp%:SCREEN-VALUE IN FRAME {&FRAME-NAME}) * -1
      .
  END.
  
  RUN SUPER.
  
  DO WITH FRAME {&FRAME-NAME}:
    
    IF AVAILABLE Kampanjehode THEN 
    DO:
      IF bNew THEN 
        JBoxServerApi:Instance:Update("Kampanjehode",
          KampanjeHode.RowIdent1,
          'BrukerID,AvslagType,ProfilNr',
          JBoxSession:Instance:UserId + '|' + AvslagType:SCREEN-VALUE + '|' + ProfilNr:SCREEN-VALUE,
          FALSE,
          "kampanjehode_post_update.p",
          TRUE).
          
      JBoxServerApi:Instance:Update("Kampanjehode",
        KampanjeHode.RowIdent1,
        'BrukerID,AktiveresTid,GyldigTilTid',
        JBoxSession:Instance:UserId + '|' + STRING(iAktiveresTid) + '|' + STRING(iGyldigTilTid),
        FALSE,
        "kampanjehode_post_update.p",
        TRUE).
        
      IF KampanjeHode.Avslagtype = 1 AND ABS(lOrgKamp%) <> ABS(lKamp%) THEN
      DO:
        pcTekst = ",RAB%," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(lKamp%) + ',' + JBoxSession:Instance:UserId + ',' + STRING(bEndreAktivTilbud).  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
      END.
      ELSE IF KampanjeHode.AvslagType = 2 AND lOrgKampanjePris <> KampanjeHode.KampanjePris THEN 
      DO:
        pcTekst = ",KPRIS," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KampanjePris) + ',' + JBoxSession:Instance:UserId + ',' + STRING(bEndreAktivTilbud).  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
      END.
      ELSE IF KampanjeHode.AvslagType = 3 AND lOrgKroneRabatt <> KampanjeHode.KroneRabatt THEN 
      DO:
        pcTekst = ",KRRAB," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KroneRabatt) + ',' + JBoxSession:Instance:UserId + ',' + STRING(bEndreAktivTilbud).  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
      END.
      
      /* Ved endring av aktivt tilbud, aktiveres endringen her. */
      IF bEndreAktivTilbud THEN 
      DO:
          httImpFilLinje = BUFFER KampanjeLinje:HANDLE.
          IF NOT JBoxServerAPI:Instance:CallServerProc("kampanje_kontroll.p",
            STRING(KampanjeHode.KampanjeId) + '|' + cLogg,
            httImpFilLinje
            ) THEN 
            DO: 
              bEndreAktivTilbud = FALSE.
              oBrwKampanjeHode:refresh().        
              JBoxSession:Instance:ViewMessage("Aktivering kan ikke gjøres fordi " + JBoxServerAPI:Instance:getCallMessage()).
              RETURN.
            END.
        
        pcTekst = ",AKTIVERENDRING," + STRING(KampanjeHode.KampanjeId) + ',' + STRING(KampanjeHode.KroneRabatt) + ',' + JBoxSession:Instance:UserId + ',' + STRING(bEndreAktivTilbud).  
        IF NOT oBrwKampanjeHode:processRowsNoMessage("avbryt_Kampanje.p", pcTekst) THEN
          JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).              
        oBrwKampanjeHode:refresh().        
        
        JBoxSession:Instance:ViewMessage("Endring er oppdatert på kampanjelinjene, og de aktive tilbudsprisene er oppdatert tilsvarende.").
      END.
      ELSE 
        oBrwKampanjeHode:refresh().        
    END.
  END.
  bNew = FALSE.
  bEndreAktivTilbud = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TilPriskoRecord C-Win 
PROCEDURE TilPriskoRecord :
DO WITH FRAME {&FRAME-NAME}:
    IF oBrwKampanjeHode:BROWSE-HANDLE:NUM-SELECTED-ROWS >= 1 THEN
    DO:    
      IF oBrwKampanjeHode:BROWSE-HANDLE:NUM-SELECTED-ROWS = 1 THEN 
      DO:
        IF AVAILABLE KampanjeHode THEN 
        DO:
          IF KampanjeHode.StartDato = ? OR KampanjeHode.SluttDato = ? THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Start og slutt dato for kampanjen må være registrert.").
            RETURN.
          END.
          IF KampanjeHode.Beskrivelse = '' THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen mangler beskrivelse.").
            RETURN.
          END.
          IF KampanjeHode.Aktivert THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjen er allerede aktivert.").
            RETURN.
          END.
          IF KampanjeHode.StartDato < TODAY THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjens startdato er tidligere enn dagens dato. Korriger dato!").
            RETURN.
          END.
          IF KampanjeHode.StartDato > KampanjeHode.SluttDato THEN 
          DO:
            JBoxSession:Instance:ViewMessage("Kampanjens startdato er større enn dens sluttdato. Korriger dato!").
            RETURN.
          END.
          IF KampanjeHode.StartDato = KampanjeHode.SluttDato THEN 
          DO:
            IF KampanjeHode.AktiveresTid_time >= KampanjeHode.GyldigTilTid_time THEN 
            DO:
              JBoxSession:Instance:ViewMessage("Kampanjens start tidspunkt er tidligere eller lik dens avsluttnings tidspunkt. Korriger tidsiintervall!").
              RETURN.
            END.
          END.
          IF KampanjeHode.AvslagType = 2 AND KampanjeHode.KampanjePris <= 0 THEN
          DO:
            JBoxSession:Instance:ViewMessage("Fastpris kampanje og kampanjepris er ikke angitt!").
            RETURN.
          END.
          IF KampanjeHode.AvslagType = 3 AND KampanjeHode.Kronerabatt <= 0 THEN
          DO:
            JBoxSession:Instance:ViewMessage("Kampanje med kronerabatt og kronerabatt er ikke angitt!").
            RETURN.
          END.
          IF KampanjeHode.AvslagType = 3 AND KampanjeHode.MistePris <= 0 THEN
          DO:
            JBoxSession:Instance:ViewMessage("Kampanje med kronerabatt og minstepris er ikke angitt! (Minstepris: " + STRING(KampanjeHode.MistePris) + ")" ).
            RETURN.
          END.
          
          httImpFilLinje = BUFFER KampanjeLinje:HANDLE.
          IF NOT JBoxServerAPI:Instance:CallServerProc("kampanje_kontroll.p",
            STRING(KampanjeHode.KampanjeId) + '|' + cLogg,
            httImpFilLinje
            ) THEN 
            DO: 
              JBoxSession:Instance:ViewMessage("Aktivering kan ikke gjøres fordi " + JBoxServerAPI:Instance:getCallMessage()).
              RETURN.
            END.
          
          IF CAN-DO('Ja,true,yes',STRING(KampanjeHode.IgnorerNOS)) THEN 
          DO:
            IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Du har valgt å ignorere NOS merking av artikklene ved aktivering av kampanjen. Er dette korrekt?") THEN 
              RETURN.
          END.   
          IF NOT JBoxSession:Instance:ViewQuestionOkCancel("Aktiver kampanje?") THEN 
            RETURN.
          IF NOT oBrwKampanjeHode:processRowsNoMessage("kampanjehode_aktiver.p", "") THEN
            JBoxSession:Instance:ViewMessage("Feil pga " + JBoxServerAPI:Instance:getCallMessage()).
          oBrwKampanjeHode:refresh().
          oBrwKampanjeLinje:refresh().
        END. /* Kampanjehode avail */
      END.            
      ELSE 
      DO:
        JBoxSession:Instance:ViewMessage('Bare en kampanje kan aktiveres ad gangen.').
        RETURN.     
      END.
    END.
    ELSE 
    DO:
      JBoxSession:Instance:ViewMessage('Marker den kampanjen som skal aktiveres.').
      RETURN.     
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
DEF INPUT PARAM icFieldName   AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN "AvslagType" THEN
      DO:
        RUN EndreAvslagtype.
      END.
    WHEN "fiKamp%" THEN 
     DO:
       ASSIGN 
         ProfilNr:SENSITIVE = FALSE
         AvslagType:SENSITIVE = FALSE
         .
     END.
  END CASE.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

