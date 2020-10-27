
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
 
DEFINE TEMP-TABLE ttConfig
  FIELD cHost AS CHARACTER 
  FIELD cPort AS CHARACTER 
  FIELD cAppServer AS CHARACTER
  FIELD iButNr AS INTEGER
  FIELD iKasseNr AS INTEGER
  FIELD iSelgerId AS INTEGER 
  FIELD cNavnIKasse AS CHARACTER FORMAT "x(20)"
  FIELD cSelgerNavn AS CHARACTER FORMAT "x(30)"  
  FIELD webBrukerid AS CHARACTER 
  FIELD webpassord AS CHARACTER 
  .

/* Oppretter config katalog. */
OS-COMMAND SILENT VALUE('md config').

ASSIGN 
  cConfigFile = 'config\PRSPosKlient.JSon'
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
    ttConfig.cHost      = OS-GETENV("COMPUTERNAME")
    ttConfig.cPort      = '3190'
    ttConfig.cAppServer = 'asPRS'
    ttConfig.iButNr     = 2
    ttConfig.iKasseNr   = 1 
    ttConfig.webbrukerid = ''
    ttConfig.webpassord  = ''
    .
    
  MESSAGE "Finner ikke filen 'config\PRSPosKlient.JSon'. Skal den opprettesmed verdiene vist under?" SKIP 
  'Host.....: ' ttConfig.cHost SKIP 
  'Port.....: ' ttConfig.cPort SKIP 
  'AppServer: ' ttConfig.cAppServer SKIP
  'Butikk...: ' ttConfig.iButNr SKIP
  'KasseNr..: ' ttConfig.iKasseNr SKIP 
  'WebBrukerid: ' ttConfig.webbrukerid SKIP 
  'WebPassord : ' ttConfig.webpassord
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
  'Butikknr.: ' ttConfig.iButNr SKIP
  'KasseNr..: ' ttConfig.iKasseNr SKIP
  'Host.....: ' ttConfig.cHost SKIP 
  'Port.....: ' ttConfig.cPort SKIP 
  'AppServer: ' ttConfig.cAppServer SKIP 
  'WebBrukerid: ' ttConfig.webbrukerid SKIP 
  'WebPassord : ' ttConfig.webpassord SKIP 
  'Connect..: ' "-H " + ttConfig.cHost + " -S " + ttConfig.cPort + " -AppService " + ttConfig.cAppServer + " -DirectConnect"
  VIEW-AS ALERT-BOX.

/* Mot asPRS appserveren. */
IF AVAILABLE ttConfig THEN 
DO:
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
  MESSAGE 'Oppkoblet mot AppServer: ' hServer:CONNECTED()
  VIEW-AS ALERT-BOX.
/* Kjør denne når du kjører uten AppServer. */
/*hServer = SESSION.*/

RUN winsrc\InitPRSPosKlient.p (OS-GETENV("username"),hServer, TABLE ttConfig) NO-ERROR.

IF bTest THEN 
  MESSAGE 'Etter init.' ERROR-STATUS:ERROR 
  VIEW-AS ALERT-BOX.
IF ERROR-STATUS:ERROR THEN
DO:
  DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:  
      cErorTekst = cErorTekst + (IF cErorTekst <> '' THEN CHR(10) ELSE '') + ERROR-STATUS:GET-MESSAGE(ix).     
  END.  
  MESSAGE 'Feil: ' cErorTekst
  VIEW-AS ALERT-BOX.
END. 

IF bTest THEN 
  MESSAGE 'Etter InitPRS.p'
  VIEW-AS ALERT-BOX.

WAIT-FOR System.Windows.Forms.Application:Run().
