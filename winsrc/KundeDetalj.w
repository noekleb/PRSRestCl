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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR oContainer  AS JBoxContainer NO-UNDO.


/* Code for Definitions: */

/*** Start instance property definitions for JBoxQuery object oQryKunde ***/
DEF VAR oQryKunde AS JBoxQuery NO-UNDO.

DEF TEMP-TABLE Kunde
    FIELD Adresse1 AS CHARACTER
    FIELD Adresse2 AS CHARACTER
    FIELD Aktiv AS LOGICAL
    FIELD Alder AS INTEGER
    FIELD Avdeling AS CHARACTER
    FIELD BankKode AS CHARACTER
    FIELD BankKonto AS CHARACTER
    FIELD BankNavn AS CHARACTER
    FIELD BetBet AS INTEGER
    FIELD BetType AS INTEGER
    FIELD BrukerID AS CHARACTER
    FIELD ButikkNr AS INTEGER
    FIELD BydelsNr AS CHARACTER
    FIELD ByNavn AS CHARACTER
    FIELD DeresRef AS CHARACTER
    FIELD EDato AS DATE
    FIELD EksterntKundeNr AS CHARACTER
    FIELD eMailFirma AS CHARACTER
    FIELD ePostAdresse AS CHARACTER
    FIELD Etablert AS DATE
    FIELD ETid AS INTEGER
    FIELD FaktAdresse1 AS CHARACTER
    FIELD FaktAdresse2 AS CHARACTER
    FIELD FaktLand AS CHARACTER
    FIELD FaktPostNr AS CHARACTER
    FIELD FaktTekstNr AS INTEGER
    FIELD FakturaDeltajniva AS INTEGER
    FIELD Fakturagebyr AS LOGICAL
    FIELD Faktureringsperiode AS INTEGER
    FIELD FodtDato AS DATE
    FIELD ForsteKjop AS DATE
    FIELD GruppeId AS INTEGER
    FIELD Hilsen AS CHARACTER
    FIELD Hovedkunde AS LOGICAL
    FIELD Kilde AS CHARACTER
    FIELD Kjon AS INTEGER
    FIELD KobletTilKunde AS DECIMAL
    FIELD Kommentar AS CHARACTER
    FIELD KontE-Post AS CHARACTER
    FIELD KontMobilTlf AS CHARACTER
    FIELD KontNavn AS CHARACTER
    FIELD KontTelefaks AS CHARACTER
    FIELD KontTelefon AS CHARACTER
    FIELD KreditSperret AS LOGICAL
    FIELD KundeNr AS DECIMAL
    FIELD KundeSaldo AS DECIMAL
    FIELD Land AS CHARACTER
    FIELD LevAdresse1 AS CHARACTER
    FIELD LevAdresse2 AS CHARACTER
    FIELD LevLand AS CHARACTER
    FIELD LevPostNr AS CHARACTER
    FIELD MaksKredit AS DECIMAL
    FIELD MobilTlf AS CHARACTER
    FIELD Momskod AS INTEGER
    FIELD MottaeMailUtsendelser AS LOGICAL
    FIELD MvaFri AS LOGICAL
    FIELD Navn AS CHARACTER
    FIELD Opphort AS DATE
    FIELD OrgNr AS CHARACTER
    FIELD Postgiro AS CHARACTER
    FIELD PostNr AS CHARACTER
    FIELD Privat AS LOGICAL
    FIELD PrivatTlf AS CHARACTER
    FIELD Purregebyr AS LOGICAL
    FIELD Region AS CHARACTER
    FIELD RegistrertAv AS CHARACTER
    FIELD RegistrertDato AS DATE
    FIELD RegistrertTid AS INTEGER
    FIELD SamleFaktura AS LOGICAL
    FIELD SisteKjop AS DATE
    FIELD Stilling AS CHARACTER
    FIELD Telefaks AS CHARACTER
    FIELD Telefon AS CHARACTER
    FIELD TelefonFirma AS CHARACTER
    FIELD TilgKilde AS CHARACTER
    FIELD Tittel AS CHARACTER
    FIELD TotalRabatt% AS DECIMAL
    FIELD TypeId AS INTEGER
    FIELD UrlFirma AS CHARACTER
    FIELD ValKod AS CHARACTER
    FIELD WebKanSendeEMail AS LOGICAL
    FIELD WebKanSetteOrdre AS LOGICAL
    FIELD WebKunde AS LOGICAL
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    INDEX idxRowids  RowIdent1
    .
/*DEFINE BUFFER v_Kunde FOR Kunde.*/

FUNCTION getTableHandleQryKunde RETURNS HANDLE().
  RETURN BUFFER Kunde:HANDLE:TABLE-HANDLE.
END FUNCTION.
FUNCTION getBuffersAndFieldsQryKunde RETURNS CHARACTER().
  RETURN 
      'Kunde'.
END FUNCTION.
FUNCTION getQueryJoinQryKunde RETURNS CHARACTER().
  RETURN ''.
END FUNCTION.
/*** End instance property settings for JBoxQuery object oQryKunde ***/

DEF VAR oFmKunde AS JBoxFieldMap NO-UNDO.


DEF VAR otbKunde AS JBoxToolbar NO-UNDO.

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
&Scoped-define INTERNAL-TABLES Kunde

/* Definitions for QUERY QUERY-2                                        */
&Scoped-define SELF-NAME QUERY-2
&Scoped-define QUERY-STRING-QUERY-2 FOR EACH Kunde NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-QUERY-2 OPEN QUERY {&SELF-NAME} FOR EACH Kunde NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-QUERY-2 Kunde
&Scoped-define FIRST-TABLE-IN-QUERY-QUERY-2 Kunde


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS edit_tbKunde tbKunde save_tbKunde ~
undo_tbKunde KundeNr Navn Adresse1 Adresse2 PostNr Land FaktAdresse1 ~
FaktAdresse2 FaktPostNr FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand ~
btnPost btnFaktPost btnLevPost ePostAdresse Telefon MobilTlf Kundesaldo ~
ForsteKjop SisteKjop RegistrertDato EDato 
&Scoped-Define DISPLAYED-OBJECTS KundeNr Navn Adresse1 Adresse2 PostNr Land ~
FaktAdresse1 FaktAdresse2 FaktPostNr FaktLand LevAdresse1 LevAdresse2 ~
LevPostNr LevLand fiPostSted fiFaktPostSted fiLevPostSted ePostAdresse ~
Telefon MobilTlf Kundesaldo ForsteKjop SisteKjop RegistrertDato EDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFaktPost 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk i liste over faktura adressens postnummer".

DEFINE BUTTON btnLevPost 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk i liste over faktura adressens postnummer".

DEFINE BUTTON btnPost 
     LABEL "..." 
     SIZE 4 BY 1 TOOLTIP "Søk i liste over postnummer".

DEFINE BUTTON edit_tbKunde 
     IMAGE-UP FILE "bmp/edit16e.bmp":U
     LABEL "Edit" 
     SIZE 4.6 BY 1.1 TOOLTIP "Edit (CTRL-E)".

DEFINE BUTTON save_tbKunde 
     IMAGE-UP FILE "bmp/save.bmp":U
     LABEL "Lagre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Lagre (CTRL-S)".

DEFINE BUTTON undo_tbKunde 
     IMAGE-UP FILE "bmp/undo16e.bmp":U
     LABEL "Angre" 
     SIZE 4.6 BY 1.1 TOOLTIP "Angre (CTRL-Z)".

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse1" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Kundens adresse".

DEFINE VARIABLE Adresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse2" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Kundens adresse".

DEFINE VARIABLE EDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Endret" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 TOOLTIP "Når kundens data sist ble endret" NO-UNDO.

DEFINE VARIABLE ePostAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "ePost" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Kundes ePost adresse".

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

DEFINE VARIABLE fiFaktPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 TOOLTIP "Faktura adressens poststed." NO-UNDO.

DEFINE VARIABLE fiLevPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 TOOLTIP "Leverings adressens poststed." NO-UNDO.

DEFINE VARIABLE fiPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 24 BY 1 TOOLTIP "Poststed." NO-UNDO.

DEFINE VARIABLE ForsteKjop AS DATE FORMAT "99/99/9999":U 
     LABEL "Første kjøp" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 TOOLTIP "Når kunden gjorde sitt første kjøp." NO-UNDO.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Kundens unike id i kunderegisteret." NO-UNDO.

DEFINE VARIABLE Kundesaldo AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 TOOLTIP "Kundens utestående saldo" NO-UNDO.

DEFINE VARIABLE Land AS CHARACTER FORMAT "X(256)":U 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Land" NO-UNDO.

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

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(40)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Kundens mobil telefonnummer".

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Navn eller firmanavn".

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postnummer" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Postnummer." NO-UNDO.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Opprettet" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 TOOLTIP "Når kunden ble opprettet" NO-UNDO.

DEFINE VARIABLE SisteKjop AS DATE FORMAT "99/99/9999":U 
     LABEL "Siste kjøp" 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY 1 TOOLTIP "Når kunden gjorde sitt siste kjøp." NO-UNDO.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(40)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Kundens fast telefon".

DEFINE RECTANGLE tbKunde
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL  GROUP-BOX  ROUNDED 
     SIZE 107.4 BY 1.29.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY QUERY-2 FOR 
      Kunde SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     edit_tbKunde AT ROW 1.29 COL 1.8 WIDGET-ID 4 NO-TAB-STOP 
     save_tbKunde AT ROW 1.29 COL 6.6 WIDGET-ID 6 NO-TAB-STOP 
     undo_tbKunde AT ROW 1.29 COL 11.2 WIDGET-ID 12 NO-TAB-STOP 
     KundeNr AT ROW 2.67 COL 17 COLON-ALIGNED
     Navn AT ROW 3.67 COL 17 COLON-ALIGNED
     Adresse1 AT ROW 5.71 COL 17 COLON-ALIGNED
     Adresse2 AT ROW 6.71 COL 17 COLON-ALIGNED
     PostNr AT ROW 7.71 COL 17 COLON-ALIGNED
     Land AT ROW 8.71 COL 17 COLON-ALIGNED
     FaktAdresse1 AT ROW 10.71 COL 17 COLON-ALIGNED
     FaktAdresse2 AT ROW 11.71 COL 17 COLON-ALIGNED
     FaktPostNr AT ROW 12.71 COL 17 COLON-ALIGNED
     FaktLand AT ROW 13.71 COL 17 COLON-ALIGNED
     LevAdresse1 AT ROW 15.71 COL 17 COLON-ALIGNED
     LevAdresse2 AT ROW 16.71 COL 17 COLON-ALIGNED
     LevPostNr AT ROW 17.71 COL 17 COLON-ALIGNED
     LevLand AT ROW 18.71 COL 17 COLON-ALIGNED
     btnPost AT ROW 7.71 COL 33 WIDGET-ID 24 NO-TAB-STOP 
     btnFaktPost AT ROW 12.71 COL 33 WIDGET-ID 34 NO-TAB-STOP 
     btnLevPost AT ROW 17.71 COL 33 WIDGET-ID 50 NO-TAB-STOP 
     fiPostSted AT ROW 7.71 COL 35 COLON-ALIGNED NO-LABEL
     fiFaktPostSted AT ROW 12.71 COL 35 COLON-ALIGNED NO-LABEL
     fiLevPostSted AT ROW 17.71 COL 35 COLON-ALIGNED NO-LABEL
     ePostAdresse AT ROW 5.71 COL 75.8 COLON-ALIGNED
     Telefon AT ROW 6.71 COL 75.8 COLON-ALIGNED
     MobilTlf AT ROW 7.71 COL 75.8 COLON-ALIGNED
     Kundesaldo AT ROW 10.71 COL 75.8 COLON-ALIGNED
     ForsteKjop AT ROW 11.71 COL 75.8 COLON-ALIGNED
     SisteKjop AT ROW 12.71 COL 75.8 COLON-ALIGNED
     RegistrertDato AT ROW 15.71 COL 75.8 COLON-ALIGNED
     EDato AT ROW 16.71 COL 75.8 COLON-ALIGNED
     "Kundens adresse" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 5 COL 19 WIDGET-ID 26
          FONT 6
     "Faktura adresse" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 10 COL 19 WIDGET-ID 38
          FONT 6
     "Leveringsadresse" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 15.05 COL 19 WIDGET-ID 56
          FONT 6
     tbKunde AT ROW 1.19 COL 1.6 WIDGET-ID 2
    WITH 1 DOWN NO-BOX OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.4 BY 19.52 WIDGET-ID 100.


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
         HEIGHT             = 19.52
         WIDTH              = 121.4
         MAX-HEIGHT         = 23.76
         MAX-WIDTH          = 175
         VIRTUAL-HEIGHT     = 23.76
         VIRTUAL-WIDTH      = 175
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
/* SETTINGS FOR FILL-IN fiFaktPostSted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiFaktPostSted:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiLevPostSted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiLevPostSted:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN fiPostSted IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiPostSted:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       tbKunde:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "edit;Edit,save;Lagre,undo;Angremaxborder".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY QUERY-2
/* Query rebuild information for QUERY QUERY-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Kunde NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Design-Parent    is FRAME DEFAULT-FRAME @ ( 2.24 , 37 )
*/  /* QUERY QUERY-2 */
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
      fiFaktPostSted = JBoxServerAPI:Instance:LookupValue("Beskrivelse")
      fiFaktPostSted:SCREEN-VALUE = fiPostSted
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
      fiLevPostSted = JBoxServerAPI:Instance:LookupValue("Beskrivelse")
      fiLevPostSted:SCREEN-VALUE = fiPostSted
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
      fiPostSted = JBoxServerAPI:Instance:LookupValue("Beskrivelse")
      fiPostSted:SCREEN-VALUE = fiPostSted
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
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'") THEN
      ASSIGN 
        fiFaktPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
        fiFaktPostSted:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
        .
    ELSE DO:
      JBoxSession:Instance:ViewMessage("Ugyldig postnummer " + FaktPostNr:SCREEN-VALUE + " angitt på fakturaadressen.").      
      ASSIGN
        FaktPostNr:SCREEN-VALUE = '' 
        fiFaktPostSted = ''
        fiFaktPostSted:SCREEN-VALUE = ''
        .
      APPLY "ENTRY" TO FaktPostNr. 
      RETURN NO-APPLY.
    END.
  END.
  ELSE DO:
    ASSIGN
      FaktPostNr:SCREEN-VALUE = '' 
      fiFaktPostSted = ''
      fiFaktPostSted:SCREEN-VALUE = ''
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
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'") THEN
      ASSIGN 
        fiLevPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
        fiLevPostSted:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
        .
    ELSE DO:
      JBoxSession:Instance:ViewMessage("Ugyldig postnummer " + FaktPostNr:SCREEN-VALUE + " angitt på leveringsaadressen.").      
      ASSIGN
        LevPostNr:SCREEN-VALUE = '' 
        fiLevPostSted = ''
        fiLevPostSted:SCREEN-VALUE = ''
        .
      APPLY "ENTRY" TO LevPostNr. 
      RETURN NO-APPLY.
    END.
  END.
  ELSE DO:
    ASSIGN 
      fiLevPostSted = ''
      fiLevPostSted:SCREEN-VALUE = ''
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
        fiPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
        fiPostSted:SCREEN-VALUE = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse")
        .
    ELSE DO:
      JBoxSession:Instance:ViewMessage("Ugyldig postnummer " + PostNr:SCREEN-VALUE + " angitt på kundens adresse.").      
      ASSIGN
        PostNr:SCREEN-VALUE = '' 
        fiPostSted = ''
        fiPostSted:SCREEN-VALUE = ''
        .
      APPLY "ENTRY" TO PostNr. 
      RETURN NO-APPLY.
    END.
  END.
  ELSE DO:
    ASSIGN 
      fiPostSted = ''
      fiPostSted:SCREEN-VALUE = ''
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
ON CLOSE OF THIS-PROCEDURE DO:
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
IF oQryKunde:isCurrent THEN DO WITH FRAME {&FRAME-NAME}:
  END.
  
  RUN SUPER.
  
  IF oQryKunde:isCurrent THEN  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      btnPost:SENSITIVE = NOT edit_tbKunde:SENSITIVE  
      btnFaktPost:SENSITIVE = NOT edit_tbKunde:SENSITIVE  
      btnLevPost:SENSITIVE = NOT edit_tbKunde:SENSITIVE  
      .
  END.
  
  IF AVAILABLE Kunde THEN 
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      fiPostSted     = ''
      fiFaktPoststed = ''
      fiLevPostSted  = ''
      .
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + Kunde.PostNr + "'") THEN
      fiPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse").
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + Kunde.FaktPostNr + "'") THEN
      fiFaktPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse").
    IF JBoxServerAPI:Instance:Find("Post", "WHERE PostNr = '" + Kunde.LevPostNr + "'") THEN
      fiLevPostSted = JBoxServerAPI:Instance:FieldValue("Post.Beskrivelse").
    DISPLAY
      fiPostSted
      fiFaktPoststed
      fiLevPostSted
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
      btnPost:SENSITIVE = TRUE  
      btnFaktPost:SENSITIVE = TRUE  
      btnLevPost:SENSITIVE = TRUE  
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
  DISPLAY KundeNr Navn Adresse1 Adresse2 PostNr Land FaktAdresse1 FaktAdresse2 
          FaktPostNr FaktLand LevAdresse1 LevAdresse2 LevPostNr LevLand 
          fiPostSted fiFaktPostSted fiLevPostSted ePostAdresse Telefon MobilTlf 
          Kundesaldo ForsteKjop SisteKjop RegistrertDato EDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE edit_tbKunde tbKunde save_tbKunde undo_tbKunde KundeNr Navn Adresse1 
         Adresse2 PostNr Land FaktAdresse1 FaktAdresse2 FaktPostNr FaktLand 
         LevAdresse1 LevAdresse2 LevPostNr LevLand btnPost btnFaktPost 
         btnLevPost ePostAdresse Telefon MobilTlf Kundesaldo ForsteKjop 
         SisteKjop RegistrertDato EDato 
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

  oQryKunde = NEW JBoxQuery('Kunde').
/*  oQryKunde:baseQuery = "WHERE KundeNr = '114117'".*/
  
  oFmKunde = NEW JBoxFieldMap(FRAME {&FRAME-NAME}:HANDLE).
  oFmKunde:DisplayFields = 'KundeNr,KundeSaldo,ForsteKjop,SisteKjop,RegistrertDato,EDato'.
  oFmKunde:updateFields = 'Navn' + 
                          ',Adresse1,Adresse2,PostNr,Land' + 
                          ',FaktAdresse1,FaktAdresse2,FaktPostNr,FaktLand' + 
                          ',LevAdresse1,LevAdresse2,LevPostNr,LevLand' +
                          ',ePostAdresse,Telefon,MobilTlf' 
                          .
/*  oFmKunde:customUpdateValProc = "Ignore".*/
  oFmKunde:customUpdateValProc = "kunde_update.p".
  
  oFmKunde:QUERY-OBJECT = oQryKunde.
/*  oQryKunde:openQuery().*/
  otbKunde = NEW JBoxToolbar(tbKunde:HANDLE).

  oQryKunde:TOOLBAR-OBJECT = otbKunde.
  oFmKunde:TOOLBAR-OBJECT = otbKunde.
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
IF oFmKunde:currentFieldModified THEN DO WITH FRAME {&FRAME-NAME}:
  CASE oFmKunde:currentField:
    WHEN "PostNr" THEN DO:
      APPLY 'TAB' TO SELF.
    END.
    WHEN "LevPostNr" THEN DO:
      APPLY 'TAB' TO SELF.
    END.
    WHEN "FaktPostNr" THEN DO:
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
      btnPost:SENSITIVE = FALSE  
      btnFaktPost:SENSITIVE = FALSE  
      btnLevPost:SENSITIVE = FALSE  
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
      btnPost:SENSITIVE = FALSE  
      btnFaktPost:SENSITIVE = FALSE  
      btnLevPost:SENSITIVE = FALSE  
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

