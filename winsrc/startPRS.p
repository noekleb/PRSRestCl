
/*------------------------------------------------------------------------
    File        : startMypage.p
    Purpose     : 

    Syntax      :
O394aiz1958

    Description : 

    Author(s)   : 
    Created     : Tue Jun 21 14:32:27 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE hServer AS HANDLE NO-UNDO.
DEFINE VARIABLE cConfigFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE bSvar AS LOG NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cErorTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWebBruker AS CHARACTER NO-UNDO.
DEFINE VARIABLE cWebPassord AS CHARACTER NO-UNDO.
 
{ttConfig.i}

/* Oppretter config katalog. */
OS-COMMAND SILENT VALUE('md config').

ASSIGN 
  cConfigFile = 'config\PRSRestCl.JSon'
  bTest = FALSE
  bSvar = FALSE 
  .

/* Laster config. hvis den finnes. */
IF SEARCH(cConfigFile) <> ? THEN 
  TEMP-TABLE ttConfig:READ-JSON ("File",cConfigFile,"empty"). 
FIND FIRST ttConfig NO-ERROR.

/* Default parameteroppsett for utvikling og opprettelse av Configfile. */
IF NOT CAN-FIND(FIRST ttConfig) THEN 
DO:
  CREATE ttConfig.
  ASSIGN                             
    ttConfig.cHost       = OS-GETENV("COMPUTERNAME")
    ttConfig.cPort       = '3190'
    ttConfig.cAppServer  = 'asPRS' 
    ttConfig.iButNr      = 15
    ttConfig.iKasseNr    = 99
    ttConfig.webbrukerid = ''
    ttConfig.webpassord  = ''
    ttConfig.DebugAktiv  = TRUE
    ttConfig.DebugLevel  = 1
    ttConfig.DebugLogFile = 'WCloggmanagerDDMMYYYY.txt'
    .
    
  MESSAGE "Finner ikke filen 'config\PRSRestCl.JSon'. Skal den opprettes med verdiene vist under?" SKIP 
  'Host.....: ' ttConfig.cHost SKIP 
  'Port.....: ' ttConfig.cPort SKIP 
  'AppServer: ' ttConfig.cAppServer SKIP 
  'Butikknr.: ' ttConfig.iButNr SKIP
  'KasseNr..: ' ttConfig.iKasseNr SKIP
  'WebBrukerid: ' ttConfig.webbrukerid SKIP 
  'WebPassord: ' ttConfig.webpassord SKIP 
  'DebugAktiv: ' ttConfig.DebugAktiv SKIP 
  'DebugLevel: ' ttConfig.DebugLevel SKIP 
  'DebugLogFile: ' ttConfig.DebugLogFile 
  VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE bSvar.
  IF bSvar THEN 
  DO:
    TEMP-TABLE ttConfig:WRITE-JSON('file', cConfigFile, TRUE).
    TEMP-TABLE ttConfig:READ-JSON ("File",cConfigFile,"empty"). 
  END.
  ELSE 
    RETURN.
END.

IF bTest THEN 
  MESSAGE "Oppkoblingsparametre mot AppServer:" SKIP 
  'Host.....: ' ttConfig.cHost SKIP 
  'Port.....: ' ttConfig.cPort SKIP 
  'AppServer: ' ttConfig.cAppServer SKIP 
  'Butikknr.: ' ttConfig.iButNr SKIP
  'KasseNr..: ' ttConfig.iKasseNr SKIP
  'WebBrukerid: ' ttConfig.webbrukerid SKIP 
  'WebPassord: ' ttConfig.webpassord SKIP 
  'DebugAktiv: ' ttConfig.DebugAktiv SKIP 
  'DebugLevel: ' ttConfig.DebugLevel SKIP 
  'DebugLogFile: ' ttConfig.DebugLogFile 
  'Connect..: ' "-H " + ttConfig.cHost + " -S " + ttConfig.cPort + " -AppService " + ttConfig.cAppServer + " -DirectConnect"
  VIEW-AS ALERT-BOX.

/* Mot asPRS appserveren. */
IF AVAILABLE ttConfig THEN 
DO:
  IF ttConfig.DebugAktiv = TRUE THEN 
  DO:
    LOG-MANAGER:LOGFILE-NAME  = REPLACE(ttConfig.DebugLogFile,'DDMMYYYY',REPLACE(STRING(TODAY,"99/99/9999"),'/','')).
    LOG-MANAGER:LOGGING-LEVEL = ttConfig.DebugLevel.
  END.
  CREATE SERVER hServer.
  /*hServer:CONNECT("-H LAPTOP-CENEE05L -S 3190 -AppService asPRS -DirectConnect").*/
  hServer:CONNECT("-H " + ttConfig.cHost + " -S " + ttConfig.cPort + " -AppService " + ttConfig.cAppServer + " -DirectConnect") NO-ERROR.
END.
IF NOT hServer:CONNECTED() THEN 
DO:
  MESSAGE 'Får ikke kontakt med AppServer ' + ttConfig.cAppServer + '. Applikasjonen avsluttes.'
  VIEW-AS ALERT-BOX.
  QUIT.
END.

IF bTest THEN 
  MESSAGE 'Oppkoblet mot AppServer: ' hServer:CONNECTED() SKIP 
  'ttConfig' AVAILABLE ttConfig 
  VIEW-AS ALERT-BOX.
/* Kjør denne når du kjører uten AppServer. */
/*hServer = SESSION.*/

RUN winsrc\InitPRS.p (OS-GETENV("username"),hServer, TABLE ttConfig) NO-ERROR.
IF ERROR-STATUS:ERROR THEN
DO:
  DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:  
      cErorTekst = cErorTekst + (IF cErorTekst <> '' THEN CHR(10) ELSE '') + ERROR-STATUS:GET-MESSAGE(ix).     
  END.  
  MESSAGE cErorTekst
  VIEW-AS ALERT-BOX.
END. 

IF bTest THEN 
  MESSAGE 'Etter InitPRS.p'
  VIEW-AS ALERT-BOX.

WAIT-FOR System.Windows.Forms.Application:Run().
