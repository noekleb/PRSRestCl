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
DEFINE INPUT PARAMETER lKOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
DEFINE OUTPUT PARAMETER bOk AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE ix             AS INTEGER       NO-UNDO.
DEFINE VARIABLE hBrowse        AS HANDLE        NO-UNDO.
DEFINE VARIABLE hQuery         AS HANDLE        NO-UNDO.
DEFINE VARIABLE hToolbar       AS HANDLE        NO-UNDO.
DEFINE VARIABLE hFieldMap      AS HANDLE        NO-UNDO.
DEFINE VARIABLE oContainer     AS JBoxContainer NO-UNDO.


/* Code for Definitions: */

/*** Start instance property definitions for JBoxQuery object oQryKOrdreHode ***/
DEFINE VARIABLE oQryKOrdreHode AS JBoxQuery     NO-UNDO.

DEFINE TEMP-TABLE KOrdreHode
  FIELD Adresse1             AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD Adresse2             AS CHARACTER FORMAT "X(40)" LABEL "Adresse"
  FIELD AnsvVerksted         AS CHARACTER FORMAT "X(8)" LABEL "Ansv. versted"
  FIELD AntApnet             AS INTEGER   FORMAT ">9" LABEL "Ant. åpnet"
  FIELD AntDager             AS INTEGER   FORMAT ">>9" LABEL "Antall betalingsfrie dager" COLUMN-LABEL "AntDg"
  FIELD AntKolli             AS INTEGER   FORMAT ">9" LABEL "Antall kolli"
  FIELD AntPPEti             AS INTEGER   FORMAT ">9" LABEL "Antall etiketter"
  FIELD AvdelingNr           AS INTEGER   FORMAT ">>>9" LABEL "Avdelingsnr" COLUMN-LABEL "AvdNr"
  FIELD AvgFriSalg           AS DECIMAL   DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Avg. fritt salg" COLUMN-LABEL "AvgFriSalg"
  FIELD AvgPlSalg            AS DECIMAL   DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Avg. pliktig salg" COLUMN-LABEL "AvgPlSalg"
  FIELD Avrund               AS DECIMAL   DECIMALS 2 LABEL "Avrundet"
  FIELD AvrundingKr          AS INTEGER   LABEL "Avrunding"
  FIELD AvrundingType        AS INTEGER   FORMAT ">9" LABEL "Avrundingstype"
  FIELD BetaltDato           AS DATE      LABEL "Betalt"
  FIELD BetBet               AS INTEGER   FORMAT ">>9" LABEL "Betalings.bet"
  FIELD BetTekst             AS CHARACTER FORMAT "X(30)" LABEL "Betalingsbetingelse"
  FIELD BrukerID             AS CHARACTER FORMAT "X(10)" LABEL "Bruker"
  FIELD Bruttovekt           AS DECIMAL   DECIMALS 2 FORMAT "->>,>>9.999" LABEL "Bruttovekt"
  FIELD Butik                AS INTEGER   FORMAT ">>>>>9" LABEL "Butikknummer" COLUMN-LABEL "ButNr"
  FIELD ButikkNr             AS INTEGER   FORMAT ">>>>>9" LABEL "Butikk"
  FIELD cOpt1                AS CHARACTER
  FIELD Dato                 AS DATE      LABEL "Fakturadato"
  FIELD DatoTidEndret        AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ? LABEL "Endret"
  FIELD DatoTidOpprettet     AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ?
  FIELD DeresRef             AS CHARACTER FORMAT "X(30)" LABEL "Deres referanse"
  FIELD dOpt1                AS DECIMAL   DECIMALS 2
  FIELD EDato                AS DATE      FORMAT "99/99/9999" LABEL "Endret"
  FIELD EkstOrdreNr          AS CHARACTER FORMAT "X(15)" LABEL "Ekst.ordrenr"
  FIELD Embalage             AS CHARACTER FORMAT "X(30)" LABEL "Embalage/godsslag"
  FIELD ePostAdresse         AS CHARACTER FORMAT "X(40)" LABEL "E-Post adresse" COLUMN-LABEL "E-Post"
  FIELD ETid                 AS INTEGER   LABEL "Endret tid" COLUMN-LABEL "ETid"
  FIELD FaktAdresse1         AS CHARACTER FORMAT "X(30)" LABEL "Fakt.adresse"
  FIELD FaktAdresse2         AS CHARACTER FORMAT "X(30)" LABEL "Fakt.Adresse"
  FIELD FaktLand             AS CHARACTER FORMAT "X(30)" LABEL "Land"
  FIELD FaktPostNr           AS CHARACTER FORMAT "X(15)" LABEL "Postnr"
  FIELD FaktPoststed         AS CHARACTER FORMAT "X(8)" LABEL "Fakt. Poststed"
  FIELD FaktTekstNr          AS INTEGER   FORMAT ">>9" LABEL "FakturatekstNr"
  FIELD Faktura_Id           AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "FakturaId"
  FIELD FakturertAv          AS CHARACTER FORMAT "X(20)" LABEL "Fakturert av"
  FIELD FakturertDato        AS DATE      LABEL "Fakturert dato"
  FIELD FakturertTid         AS INTEGER   LABEL "Fakturert tid"
  FIELD FirmaAdresse1        AS CHARACTER FORMAT "X(30)" LABEL "Firmaadresse"
  FIELD FirmaAdresse2        AS CHARACTER FORMAT "X(30)" LABEL "Firmaadresse"
  FIELD FirmaBankKonto       AS CHARACTER FORMAT "X(20)" LABEL "Bankkonto"
  FIELD FirmaEPost           AS CHARACTER FORMAT "X(40)" LABEL "E-Post"
  FIELD FirmaLand            AS CHARACTER FORMAT "X(30)" LABEL "Land"
  FIELD FirmaNavn            AS CHARACTER FORMAT "X(30)" LABEL "Firma"
  FIELD FirmaOrganisasjonsNr AS CHARACTER FORMAT "X(12)" LABEL "OrganisasjonsNr"
  FIELD FirmaPostgiro        AS CHARACTER FORMAT "X(20)" LABEL "Postgiro"
  FIELD FirmaPostNr          AS CHARACTER FORMAT "X(10)" LABEL "PostNr"
  FIELD FirmaPoststed        AS CHARACTER FORMAT "X(30)" LABEL "Poststed"
  FIELD FirmaTelefaks        AS CHARACTER FORMAT "X(15)" LABEL "Telefaks"
  FIELD FirmaTelefon         AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD FirmaURLAdresse      AS CHARACTER FORMAT "X(40)" LABEL "URL adresse"
  FIELD ForfallsDato         AS DATE      LABEL "Forfall"
  FIELD ForsNr               AS INTEGER   FORMAT ">>>>>9" LABEL "Kasserer"
  FIELD Fraktbrevtekst       AS CHARACTER FORMAT "X(30)" LABEL "Fraktbrevtekst"
  FIELD Godsmerking          AS CHARACTER FORMAT "X(30)" LABEL "Godsmerking"
  FIELD InternMerknad        AS CHARACTER FORMAT "X(40)" LABEL "Intern merknad"
  FIELD iOpt1                AS INTEGER
  FIELD KasseNr              AS INTEGER   FORMAT ">>9" LABEL "Kassenummer" COLUMN-LABEL "KasseNr"
  FIELD KontNavn             AS CHARACTER FORMAT "X(40)" LABEL "Kontaktperson"
  FIELD KontoNr              AS INTEGER   FORMAT ">>9999" LABEL "Kontonummer" COLUMN-LABEL "Konto"
  FIELD KontTelefon          AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD KOrdre_Id            AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre Id" COLUMN-LABEL "KOId"
  FIELD KProsjektNr          AS INTEGER   FORMAT ">>>9" LABEL "Kundeprosjekt"
  FIELD KundeMerknad         AS CHARACTER FORMAT "X(40)" LABEL "Kunde merknad"
  FIELD KundeNr              AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Kundenummer" COLUMN-LABEL "KundeNr"
  FIELD Kundeservice         AS LOGICAL   LABEL "Kundeservice"
  FIELD LevAdresse1          AS CHARACTER FORMAT "X(40)" LABEL "Leveringsadresse"
  FIELD LevAdresse2          AS CHARACTER FORMAT "X(40)" LABEL "Leveringsadresse"
  FIELD LeveresDatoTid       AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ? COLUMN-LABEL "Leverings dato/tid"
  FIELD Leveringsdato        AS DATE      LABEL "Leveringsdato"
  FIELD LevFNr               AS INTEGER   FORMAT ">9" LABEL "Leveringsform"
  FIELD LevLand              AS CHARACTER FORMAT "X(30)" LABEL "Lev. Land"
  FIELD LevPostNr            AS CHARACTER FORMAT "X(10)" LABEL "Lev. PostNr"
  FIELD LevPostSted          AS CHARACTER FORMAT "X(30)" LABEL "Poststed"
  FIELD LevStatus            AS CHARACTER FORMAT "x(2)" LABEL "Lev.status"
  FIELD MobilTlf             AS CHARACTER FORMAT "X(15)" LABEL "Mobiltelefon"
  FIELD Mva                  AS DECIMAL   DECIMALS 2 FORMAT "->>,>>>,>>9.99" LABEL "Mva"
  FIELD MvaKr                AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Mva"
  FIELD Navn                 AS CHARACTER FORMAT "X(40)" LABEL "Navn"
  FIELD Opphav               AS INTEGER   FORMAT ">9" LABEL "Opphav"
  FIELD PostNr               AS CHARACTER FORMAT "X(10)" LABEL "PostNr"
  FIELD PostSted             AS CHARACTER FORMAT "X(30)" LABEL "Poststed"
  FIELD ProdStatus           AS CHARACTER FORMAT "X" LABEL "Prod.status"
  FIELD ProduksjonsDato      AS DATE      LABEL "Produsert"
  FIELD Referanse            AS CHARACTER FORMAT "X(30)" LABEL "Referanse"
  FIELD RefKOrdre_Id         AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "KOrdre Id" COLUMN-LABEL "KOId"
  FIELD RegistrertAv         AS CHARACTER FORMAT "X(10)" LABEL "Registrert av" COLUMN-LABEL "Reg.Av"
  FIELD RegistrertDato       AS DATE      FORMAT "99/99/9999" LABEL "Registrert dato" COLUMN-LABEL "RDato"
  FIELD RegistrertTid        AS INTEGER   LABEL "Registreringstidspunkt"
  FIELD ReturNr              AS CHARACTER FORMAT "x(30)" LABEL "Retur nr."
  FIELD SelgerNr             AS DECIMAL   DECIMALS 2 FORMAT ">>>>>>>>>>>>9" LABEL "Selgernummer" COLUMN-LABEL "SelgerNr"
  FIELD SendingsNr           AS CHARACTER FORMAT "X(30)" LABEL "Sendingsnummer"
  FIELD ShipmentSendt        AS DATETIME  FORMAT "99/99/9999 HH:MM:SS.SSS" INITIAL ?
  FIELD SvarFrist            AS DATE      FORMAT "99/99/9999" LABEL "Svarfrist"
  FIELD Telefaks             AS CHARACTER FORMAT "X(15)" LABEL "Telefaks"
  FIELD Telefon              AS CHARACTER FORMAT "X(15)" LABEL "Telefon"
  FIELD TotalRabatt%         AS DECIMAL   DECIMALS 2 FORMAT "->9.99" LABEL "Totalrabatt%"
  FIELD TotalRabattKr        AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>9.99" LABEL "Totalrabatt"
  FIELD Totalt               AS DECIMAL   DECIMALS 2 FORMAT "->>>,>>>,>>9.99" LABEL "Totalt" COLUMN-LABEL "Totalt"
  FIELD TotaltVolum          AS DECIMAL   DECIMALS 2 FORMAT "->>,>>9.999" LABEL "Totalt volum"
  FIELD Utsendelsesdato      AS DATE      LABEL "Utsendelsesdato"
  FIELD VaarRef              AS CHARACTER FORMAT "X(30)" LABEL "Vår referanse"
  FIELD ValKod               AS CHARACTER FORMAT "x(3)" LABEL "ValutaKode"
  FIELD VerkstedMerknad      AS CHARACTER FORMAT "X(80)" LABEL "Merknad (Verksted)"
  FIELD Verkstedordre        AS LOGICAL   LABEL "Verkstedordre"
  FIELD WebKanSendeEMail     AS LOGICAL
  FIELD WebKanSetteOrdre     AS LOGICAL
  FIELD WebKOrdreHode        AS LOGICAL
  FIELD RowIdent1            AS CHARACTER 
  FIELD RowCount             AS INTEGER
  INDEX idxRowids RowIdent1
  .
/*DEFINE BUFFER v_KOrdreHode FOR KOrdreHode.*/

DEFINE VARIABLE httKOrdreHodeBuffer AS HANDLE NO-UNDO.
httKOrdreHodeBuffer = BUFFER KOrdreHode:HANDLE.

FUNCTION getTableHandleQryKOrdreHode RETURNS HANDLE().
  RETURN BUFFER KOrdreHode:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryKOrdreHode RETURNS CHARACTER().
  RETURN 
    'KOrdreHode'.
END FUNCTION.
FUNCTION getQueryJoinQryKOrdreHode RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxQuery object oQryKOrdreHode ***/

DEFINE VARIABLE oFmKOrdreHode AS JBoxFieldMap  NO-UNDO.
DEFINE VARIABLE otbKOrdreHode AS JBoxToolbar   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define QUERY-NAME QUERY-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KOrdreHode

/* Definitions for QUERY QUERY-2                                        */
&Scoped-define SELF-NAME QUERY-2
&Scoped-define QUERY-STRING-QUERY-2 FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-2 OPEN QUERY {&SELF-NAME} FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-2 KOrdreHode
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-2 KOrdreHode


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edit_tbKOrdreHode tbKOrdreHode RECT-1 ~
save_tbKOrdreHode undo_tbKOrdreHode KundeNr Navn ePostAdresse Telefon ~
MobilTlf FirmaNavn KontNavn Adresse1 Adresse2 PostNr FaktAdresse1 ~
FaktAdresse2 FaktPostNr FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand ~
btnPost btnFaktPost btnLevPost cOpt1 BtnDone RegistrertDato EDato 
&Scoped-Define DISPLAYED-OBJECTS KOrdre_Id KundeNr Navn ePostAdresse ~
Telefon MobilTlf FirmaNavn KontNavn Adresse1 Adresse2 PostNr FaktAdresse1 ~
FaktAdresse2 FaktPostNr FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand ~
PostSted FaktPostSted LevPostSted cOpt1 RegistrertDato EDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14 TOOLTIP "Avslutter og lukker vinduet."
     BGCOLOR 8 .

DEFINE BUTTON btnFaktPost 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk i liste over faktura adressens postnummer".

DEFINE BUTTON btnLevPost 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk i liste over faktura adressens postnummer".

DEFINE BUTTON btnPost 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk i liste over postnummer".

DEFINE BUTTON edit_tbKOrdreHode 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 4.6 BY 1.1 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON save_tbKOrdreHode 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbKOrdreHode 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE cOpt1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 58.4 BY 20.62 NO-UNDO.

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "KOrdreHodens adresse".

DEFINE VARIABLE Adresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse2" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "KOrdreHodens adresse".

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/9999":U 
      VIEW-AS TEXT 
     SIZE 18.2 BY .62 TOOLTIP "Når KOrdreHodens data sist ble endret" NO-UNDO.

DEFINE VARIABLE ePostAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "ePost" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "KOrdreHodes ePost adresse".

DEFINE VARIABLE FaktAdresse1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adresse1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Fakturaadresse." NO-UNDO.

DEFINE VARIABLE FaktAdresse2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adresse2" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE FaktLand AS CHARACTER FORMAT "X(256)":U 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Fakturaadressens land." NO-UNDO.

DEFINE VARIABLE FaktPostNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Postnummer faktura adresse" NO-UNDO.

DEFINE VARIABLE FaktPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 TOOLTIP "Faktura adressens poststed." NO-UNDO.

DEFINE VARIABLE FirmaNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eget firmanavn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE KontNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundeordre Id" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 18.2 BY 1 TOOLTIP "Kundeordrens unike id i systemet." NO-UNDO.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 TOOLTIP "Kundenummer" NO-UNDO.

DEFINE VARIABLE LevAdresse1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adresse1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Leveringsadresse" NO-UNDO.

DEFINE VARIABLE LevAdresse2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adresse2" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Leveringsadresse" NO-UNDO.

DEFINE VARIABLE LevLand AS CHARACTER FORMAT "X(256)":U 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Leveringsadressens land." NO-UNDO.

DEFINE VARIABLE LevPostNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Postnummer leverings adresse" NO-UNDO.

DEFINE VARIABLE LevPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 TOOLTIP "Leverings adressens poststed." NO-UNDO.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(40)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "KOrdreHodens mobil telefonnummer".

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Navn eller firmanavn".

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Postnummer." NO-UNDO.

DEFINE VARIABLE PostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 TOOLTIP "Poststed." NO-UNDO.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Opprettet/endret" 
      VIEW-AS TEXT 
     SIZE 18.2 BY .62 TOOLTIP "Når KOrdreHoden ble opprettet" NO-UNDO.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(40)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "KOrdreHodens fast telefon".

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 119.8 BY 23.57.

DEFINE RECTANGLE tbKOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 107.4 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QUERY-2 FOR 
      KOrdreHode SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     edit_tbKOrdreHode AT ROW 1.29 COL 1.8 WIDGET-ID 4 NO-TAB-STOP 
     save_tbKOrdreHode AT ROW 1.29 COL 6.6 WIDGET-ID 6 NO-TAB-STOP 
     undo_tbKOrdreHode AT ROW 1.29 COL 11.2 WIDGET-ID 12 NO-TAB-STOP 
     KOrdre_Id AT ROW 3.14 COL 17.8 COLON-ALIGNED
     KundeNr AT ROW 4.14 COL 17.8 COLON-ALIGNED
     Navn AT ROW 5.14 COL 17.8 COLON-ALIGNED
     ePostAdresse AT ROW 6.14 COL 17.8 COLON-ALIGNED
     Telefon AT ROW 7.14 COL 17.8 COLON-ALIGNED
     MobilTlf AT ROW 8.14 COL 17.8 COLON-ALIGNED
     FirmaNavn AT ROW 9.14 COL 17.8 COLON-ALIGNED
     KontNavn AT ROW 10.14 COL 17.8 COLON-ALIGNED
     Adresse1 AT ROW 12.91 COL 18 COLON-ALIGNED
     Adresse2 AT ROW 13.91 COL 18 COLON-ALIGNED
     PostNr AT ROW 14.91 COL 18 COLON-ALIGNED
     FaktAdresse1 AT ROW 16.91 COL 18 COLON-ALIGNED
     FaktAdresse2 AT ROW 17.91 COL 18 COLON-ALIGNED
     FaktPostNr AT ROW 18.91 COL 18 COLON-ALIGNED
     FaktLand AT ROW 19.91 COL 18 COLON-ALIGNED
     LevAdresse1 AT ROW 21.71 COL 18 COLON-ALIGNED
     LevAdresse2 AT ROW 22.71 COL 18 COLON-ALIGNED
     LevPostNr AT ROW 23.71 COL 18 COLON-ALIGNED
     LevLand AT ROW 24.71 COL 18 COLON-ALIGNED
     btnPost AT ROW 14.91 COL 34 WIDGET-ID 24 NO-TAB-STOP 
     btnFaktPost AT ROW 18.91 COL 34 WIDGET-ID 34 NO-TAB-STOP 
     btnLevPost AT ROW 23.71 COL 34 WIDGET-ID 50 NO-TAB-STOP 
     PostSted AT ROW 14.91 COL 36 COLON-ALIGNED NO-LABEL
     FaktPostSted AT ROW 18.91 COL 36 COLON-ALIGNED NO-LABEL
     LevPostSted AT ROW 23.71 COL 36 COLON-ALIGNED NO-LABEL
     cOpt1 AT ROW 5.14 COL 62.6 NO-LABEL WIDGET-ID 64
     BtnDone AT ROW 26.57 COL 106.8 WIDGET-ID 58
     RegistrertDato AT ROW 26.67 COL 17.8 COLON-ALIGNED
     EDato AT ROW 26.67 COL 36.2 COLON-ALIGNED NO-LABEL
     "Ordrens adresse" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 12.19 COL 20 WIDGET-ID 26
          FONT 6
     "Gaveinnpakning" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 4.38 COL 62.6 WIDGET-ID 68
          FONT 6
     "Leveringsadresse" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 21.05 COL 20 WIDGET-ID 56
          FONT 6
     "Faktura adresse" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 16.19 COL 20 WIDGET-ID 38
          FONT 6
     tbKOrdreHode AT ROW 1.19 COL 1.6 WIDGET-ID 2
     RECT-1 AT ROW 2.67 COL 2.2 WIDGET-ID 62
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.2 BY 26.95
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


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
         TITLE              = "Vedlikehold av kundeinformasjon på kundeordre"
         HEIGHT             = 26.95
         WIDTH              = 121.2
         MAX-HEIGHT         = 36.05
         MAX-WIDTH          = 149.8
         VIRTUAL-HEIGHT     = 36.05
         VIRTUAL-WIDTH      = 149.8
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
ASSIGN 
       cOpt1:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

/* SETTINGS FOR FILL-IN FaktPostSted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FaktPostSted:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN KOrdre_Id IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       KOrdre_Id:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN LevPostSted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       LevPostSted:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN PostSted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       PostSted:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       tbKOrdreHode:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "edit;Edit,save;Lagre,undo;Angremaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-2
/* Query rebuild information for QUERY QUERY-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH KOrdreHode NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 4.33 , 4 )
*/  /* QUERY QUERY-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Vedlikehold av kundeinformasjon på kundeordre */
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
ON WINDOW-CLOSE OF C-Win /* Vedlikehold av kundeinformasjon på kundeordre */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      ASSIGN 
        bOk = TRUE 
        .
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFaktPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFaktPost C-Win
ON CHOOSE OF btnFaktPost IN FRAME DEFAULT-FRAME /* ... */
DO:
    /* To manipulate the lookup query object add the callback procedure hook "myLookupObject" 
       (probably use "WHERE false" as the initial query in this case) */
  
    JBoxServerAPI:Instance:Lookup("Post"
      + ";PostNr"
      + ";Beskrivelse"
      ,"WHERE TRUE"
      ,"PostNr,Beskrivelse"   /* <- return values for these fields */
      ).

    IF JBoxServerAPI:Instance:LookupOk THEN 
    DO:
      ASSIGN       
        FaktPostNr                = JBoxServerAPI:Instance:LookupValue("PostNr")
        FaktPostSted              = JBoxServerAPI:Instance:LookupValue("Beskrivelse")
        FaktPostNr:SCREEN-VALUE   = FaktPostNr
        FaktPostSted:SCREEN-VALUE = FaktPostSted
        .
      APPLY "ENTRY" TO FaktPostNr.
      RETURN NO-APPLY.
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevPost C-Win
ON CHOOSE OF btnLevPost IN FRAME DEFAULT-FRAME /* ... */
DO:
    /* To manipulate the lookup query object add the callback procedure hook "myLookupObject" 
       (probably use "WHERE false" as the initial query in this case) */
  
    JBoxServerAPI:Instance:Lookup("Post"
      + ";PostNr"
      + ";Beskrivelse"
      ,"WHERE TRUE"
      ,"PostNr,Beskrivelse"   /* <- return values for these fields */
      ).

    IF JBoxServerAPI:Instance:LookupOk THEN 
    DO:
      ASSIGN   
        LevPostNr                = JBoxServerAPI:Instance:LookupValue("PostNr")     
        LevPostSted              = JBoxServerAPI:Instance:LookupValue("Beskrivelse")
        LevPostNr:SCREEN-VALUE   = LevPostNr
        LevPostSted:SCREEN-VALUE = LevPostSted
        .
      APPLY "ENTRY" TO LevPostNr.
      RETURN NO-APPLY.
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPost C-Win
ON CHOOSE OF btnPost IN FRAME DEFAULT-FRAME /* ... */
DO:
    /* To manipulate the lookup query object add the callback procedure hook "myLookupObject" 
       (probably use "WHERE false" as the initial query in this case) */
  
    JBoxServerAPI:Instance:Lookup("Post"
      + ";PostNr"
      + ";Beskrivelse"
      ,"WHERE TRUE"
      ,"PostNr,Beskrivelse"   /* <- return values for these fields */
      ).

    IF JBoxServerAPI:Instance:LookupOk THEN 
    DO:
      ASSIGN  
        PostNr                = JBoxServerAPI:Instance:LookupValue("PostNr")    
        PostSted              = JBoxServerAPI:Instance:LookupValue("Beskrivelse")
        PostNr:SCREEN-VALUE   = PostNr
        PostSted:SCREEN-VALUE = PostSted
        .
      APPLY "ENTRY" TO PostNr.
      RETURN NO-APPLY.
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FaktPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FaktPostNr C-Win
ON TAB OF FaktPostNr IN FRAME DEFAULT-FRAME /* Postnummer */
DO:
    IF FaktPostNr:SCREEN-VALUE <> '' THEN 
    DO:
      IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + FaktPostNr:SCREEN-VALUE + "'") THEN
        ASSIGN 
          FaktPostSted              = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
          FaktPostSted:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
          .
      ELSE 
      DO:
        JBoxSession:Instance:ViewMessage("Ugyldig postnummer " + FaktPostNr:SCREEN-VALUE + " angitt på fakturaadressen.").      
        ASSIGN
          FaktPostNr:SCREEN-VALUE     = '' 
          FaktPostSted              = ''
          FaktPostSted:SCREEN-VALUE = ''
          .
        APPLY "ENTRY" TO FaktPostNr. 
        RETURN NO-APPLY.
      END.
    END.
    ELSE 
    DO:
      ASSIGN
        FaktPostNr:SCREEN-VALUE     = '' 
        FaktPostSted              = ''
        FaktPostSted:SCREEN-VALUE = ''
        .
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevPostNr C-Win
ON TAB OF LevPostNr IN FRAME DEFAULT-FRAME /* Postnummer */
DO:
    IF LevPostNr:SCREEN-VALUE <> '' THEN 
    DO:
      IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + LevPostNr:SCREEN-VALUE + "'") THEN
        ASSIGN 
          LevPostSted              = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
          LevPostSted:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
          .
      ELSE 
      DO:
        JBoxSession:Instance:ViewMessage("Ugyldig postnummer " + LevPostNr:SCREEN-VALUE + " angitt på leveringsaadressen.").      
        ASSIGN
          LevPostNr:SCREEN-VALUE     = '' 
          LevPostSted              = ''
          LevPostSted:SCREEN-VALUE = ''
          .
        APPLY "ENTRY" TO LevPostNr. 
        RETURN NO-APPLY.
      END.
    END.
    ELSE 
    DO:
      ASSIGN 
        LevPostSted              = ''
        LevPostSted:SCREEN-VALUE = ''
        .
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON TAB OF PostNr IN FRAME DEFAULT-FRAME /* Postnummer */
DO:
    IF PostNr:SCREEN-VALUE <> '' THEN 
    DO:
      IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'") THEN
        ASSIGN 
          PostSted              = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
          PostSted:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
          .
      ELSE 
      DO:
        JBoxSession:Instance:ViewMessage("Ugyldig postnummer " + PostNr:SCREEN-VALUE + " angitt på KOrdreHodens adresse.").      
        ASSIGN
          PostNr:SCREEN-VALUE     = '' 
          PostSted              = ''
          PostSted:SCREEN-VALUE = ''
          .
        APPLY "ENTRY" TO PostNr. 
        RETURN NO-APPLY.
      END.
    END.
    ELSE 
    DO:
      ASSIGN 
        PostSted              = ''
        PostSted:SCREEN-VALUE = ''
        .
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME QUERY-2
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
/*{incl/conttrigg.i oBrw<>:BROWSE-HANDLE} */
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
IF oQryKOrdreHode:isCurrent THEN 
  DO WITH FRAME {&FRAME-NAME}:
  END.
  
  RUN SUPER.
  
  IF oQryKOrdreHode:isCurrent THEN  
  DO WITH FRAME {&FRAME-NAME}:
/*    ASSIGN                                                   */
/*      btnPost:SENSITIVE     = NOT edit_tbKOrdreHode:SENSITIVE*/
/*      btnFaktPost:SENSITIVE = NOT edit_tbKOrdreHode:SENSITIVE*/
/*      btnLevPost:SENSITIVE  = NOT edit_tbKOrdreHode:SENSITIVE*/
/*      .                                                      */
    ASSIGN 
      btnPost:SENSITIVE     = FALSE  
      btnFaktPost:SENSITIVE = FALSE 
      btnLevPost:SENSITIVE  = FALSE 
      .
  END.
  
  IF AVAILABLE KOrdreHode THEN 
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      PostSted     = ''
      FaktPoststed = ''
      LevPostSted  = ''
      .
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + KOrdreHode.PostNr + "'") THEN
      PostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse").
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + KOrdreHode.FaktPostNr + "'") THEN
      FaktPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse").
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + KOrdreHode.LevPostNr + "'") THEN
      LevPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse").
    DISPLAY
      PostSted
      FaktPoststed
      LevPostSted
      .
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
      btnPost:SENSITIVE     = TRUE  
      btnFaktPost:SENSITIVE = TRUE  
      btnLevPost:SENSITIVE  = TRUE  
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
  DISPLAY KOrdre_Id KundeNr Navn ePostAdresse Telefon MobilTlf FirmaNavn 
          KontNavn Adresse1 Adresse2 PostNr FaktAdresse1 FaktAdresse2 FaktPostNr 
          FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand PostSted 
          FaktPostSted LevPostSted cOpt1 RegistrertDato EDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE edit_tbKOrdreHode tbKOrdreHode RECT-1 save_tbKOrdreHode 
         undo_tbKOrdreHode KundeNr Navn ePostAdresse Telefon MobilTlf FirmaNavn 
         KontNavn Adresse1 Adresse2 PostNr FaktAdresse1 FaktAdresse2 FaktPostNr 
         FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand btnPost btnFaktPost 
         btnLevPost cOpt1 BtnDone RegistrertDato EDato 
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
    
    oQryKOrdreHode = NEW JBoxQuery('KOrdreHode').
    /*  oQryKOrdreHode:baseQuery = "WHERE KOrdreHodeNr = '114117'".*/
  
    oFmKOrdreHode = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
    oFmKOrdreHode:DisplayFields = 'KOrdre_Id,KundeNr,RegistrertDato,EDato'.
    oFmKOrdreHode:updateFields = 'Navn,FirmaNavn,KontNavn' + 
      ',Adresse1,Adresse2,PostNr,PostSted' + 
      ',FaktAdresse1,FaktAdresse2,FaktPostNr,FaktPostSted,FaktLand' + 
      ',LevAdresse1,LevAdresse2,LevPostNr,LevPostSted,LevLand' +
      ',ePostAdresse,Telefon,MobilTlf,cOpt1' 
      .
    oFmKOrdreHode:customUpdateValProc = "KOrdreHode_update.p".
    
    oFmKOrdreHode:QUERY-OBJECT = oQryKOrdreHode.
    otbKOrdreHode = NEW JBoxToolbar(tbKOrdreHode:HANDLE).

    oQryKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.
    oFmKOrdreHode:TOOLBAR-OBJECT = otbKOrdreHode.

    oQryKOrdreHode:baseQuery = "WHERE KOrdre_Id = '" + STRING(lKOrdre_Id) + "'".
    oQryKOrdreHode:openQuery().
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnOfWidget C-Win 
PROCEDURE ReturnOfWidget :
IF oFmKOrdreHode:currentFieldModified THEN 
  DO WITH FRAME {&FRAME-NAME}:
    CASE oFmKOrdreHode:currentField:
      WHEN "PostNr" THEN 
        DO:
          APPLY 'TAB' TO SELF.
        END.
      WHEN "LevPostNr" THEN 
        DO:
          APPLY 'TAB' TO SELF.
        END.
      WHEN "FaktPostNr" THEN 
        DO:
          APPLY 'TAB' TO SELF.
        END.
    END CASE.
  END.
  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      btnPost:SENSITIVE     = FALSE
      btnFaktPost:SENSITIVE = FALSE
      btnLevPost:SENSITIVE  = FALSE
      edit_tbKOrdreHode:SENSITIVE = TRUE
      save_tbKOrdreHode:SENSITIVE = FALSE
      undo_tbKOrdreHode:SENSITIVE = FALSE
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord C-Win 
PROCEDURE UndoRecord :
DO WITH FRAME {&FRAME-NAME}:
  END.
  RUN SUPER.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      btnPost:SENSITIVE     = FALSE
      btnFaktPost:SENSITIVE = FALSE
      btnLevPost:SENSITIVE  = FALSE
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

