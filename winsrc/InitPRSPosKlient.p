
/*------------------------------------------------------------------------
    File        : InitMyPageAdmin.p
    Purpose     : Start JukeBox libraries and init menu

    Syntax      :

    Description : 	

    Author(s)   : Brynjar
    Created     : Thu May 26 11:06:48 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT PARAMETER icUserId AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ihServer AS HANDLE NO-UNDO.

DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSelgerNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.

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

DEFINE INPUT PARAMETER TABLE FOR ttConfig. 

DEFINE VARIABLE oMenuXML      AS JBoxXmlDoc   NO-UNDO.
DEFINE VARIABLE oJBoxMainForm AS JBoxMainForm NO-UNDO. 
DEFINE VARIABLE hDialogFrame  AS HANDLE       NO-UNDO.
DEFINE VARIABLE hTimer        AS HANDLE       NO-UNDO.
DEFINE VARIABLE bOkUser       AS LOG          NO-UNDO.

ON 'close':U OF THIS-PROCEDURE 
  RETURN.

FIND FIRST ttConfig NO-ERROR.

JBoxSession:Instance:AppServerHandle = ihServer.
DYNAMIC-FUNCTION("setSessionId","validsession").
JBoxSession:Instance:LanguageCode = "NO".
JBoxSession:Instance:BaseLanguageCode = "NO".
JBoxSession:Instance:AppTitle = "PRSKlientPOS".
JBoxSession:Instance:UseAdvGui = YES.
JBoxSession:Instance:setRowShadeColor(240,240,240).

/* TN 28/4-20 Menyen må restartes her for å få fikset det med maks og min knappene. */
/*JBoxMainMenu:Instance:RestartMenu(). NB: Er ikke nødvendig her ifølge Brynjar...*/

/* Setter informasjon som er tilgjegnelige i hele sesjonen. */
DYNAMIC-FUNCTION("setAttribute",SESSION,"ButNr",STRING(ttConfig.iButNr)).
DYNAMIC-FUNCTION("setAttribute",SESSION,"KasseNr",STRING(ttConfig.iKasseNr)).
DYNAMIC-FUNCTION("setAttribute",SESSION,"webbrukerid",STRING(ttConfig.webbrukerid)).
DYNAMIC-FUNCTION("setAttribute",SESSION,"webpassord",STRING(ttConfig.webpassord)).
/* Eksempel på hvordan det hentes ut igjen.                */
/*MESSAGE DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr").*/
/*VIEW-AS ALERT-BOX.                                       */

IF JBoxServerAPI:Instance:Find("Syspara", "WHERE SysHId = 22 and SysGr = 5 and ParaNr = 2") THEN
  ASSIGN cOutletLst = JBoxServerAPI:Instance:FieldValue("SysPara.Parameter1").
ELSE 
  cOutletLst = ''.  
      
IF AVAILABLE ttConfig AND NUM-ENTRIES(SESSION:PARAM) > 1 THEN      
BRUKEROPPSETT: 
DO:
  bOkUser = TRUE.
  /* Plukker ut butikk og selgerId. */
  DO iLoop = 1 TO NUM-ENTRIES(SESSION:PARAM):
    cTekst = ENTRY(iLoop,SESSION:PARAM).
    CASE ENTRY(1,cTekst,'='):
      WHEN 'BUTIKK' THEN
        ttConfig.iButNr = INT(ENTRY(2,cTekst,'=')).
      WHEN 'SELGER' THEN
        ttConfig.iSelgerId = INT(ENTRY(2,cTekst,'=')).
    END CASE.
    /* Setter informasjon som er tilgjegnelige i hele sesjonen. */
    DYNAMIC-FUNCTION("setAttribute",SESSION,"ButNr",STRING(ttConfig.iButNr)).
    DYNAMIC-FUNCTION("setAttribute",SESSION,"SelgerId",STRING(ttConfig.iSelgeRId)).
  END.  
  IF JBoxServerAPI:Instance:Find("ButikkSelger", "WHERE ButikkNr = '" + STRING(ttConfig.iButNr) + "' AND SelgerId = '" + STRING(ttConfig.iSelgerId) + "'") THEN
  DO:
    lSelgerNr = DEC(JBoxServerAPI:Instance:FieldValue("ButikkSelger.SelgerNr")).
    IF JBoxServerAPI:Instance:Find("Selger", "WHERE SelgerNr = '" + STRING(lSelgerNr) + "'") THEN
    DO:
      ttConfig.cNavnIKasse = JBoxServerAPI:Instance:FieldValue("Selger.NavnIKasse").
      IF ttConfig.cNavnIKasse = '' THEN 
        ttConfig.cNavnIKasse = JBoxServerAPI:Instance:FieldValue("Selger.ForNavn").
      ttConfig.cSelgerNavn = JBoxServerAPI:Instance:FieldValue("Selger.ForNavn").
      ttConfig.cSelgerNavn = ttConfig.cSelgerNavn + " " + JBoxServerAPI:Instance:FieldValue("Selger.Navn").
      DYNAMIC-FUNCTION("setASUserId",ttConfig.cNavnIKasse,ttConfig.cSelgerNavn).

      /* Setter informasjon som er tilgjegnelige i hele sesjonen. */
      DYNAMIC-FUNCTION("setAttribute",SESSION,"NavnIKasse",STRING(ttConfig.cNavnIKasse)).
      DYNAMIC-FUNCTION("setAttribute",SESSION,"SelgerNavn",STRING(ttConfig.cSelgerNavn)).
    END.
    ELSE 
      DYNAMIC-FUNCTION("setASUserId",'Ukjent','Trippelt Ukjent Selger').
  END.
  ELSE 
    DYNAMIC-FUNCTION("setASUserId",'Ukjent','Dobbelt Ukjent Selger').
END. /* BRUKEROPPSETT*/
ELSE DO: 
  RUN JBoxLogin.w (OUTPUT bOkUser).
  IF NOT bOkUser THEN QUIT.
  ttConfig.cNavnIKasse = JBoxSession:Instance:UserId.
  ttConfig.cSelgerNavn = JBoxSession:Instance:UserName.
END.

/*MESSAGE 'Test-1'           */
/*  ttConfig.iButNr SKIP     */
/*  ttConfig.iKasseNr SKIP   */
/*  ttConfig.iSelgerId SKIP  */
/*  ttConfig.cNavnIKasse SKIP*/
/*  ttConfig.cSelgerNavn     */
/*VIEW-AS ALERT-BOX.         */

DYNAMIC-FUNCTION("getSessionData").

oMenuXML = NEW JBoxXmlDoc().
oMenuXML:AddBlockNode("menu").
oMenuXML:AddNode("cStateImage","png\j0432620.png"). /* logo */

oMenuXML:AddBlockNode("tv-nav-bar").
oMenuXML:AddBlockNode("sub-menu").
oMenuXML:AddNode("cMenuLabel","Personal").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Rutine og funksjonsbeskrivelser").
oMenuXML:AddNode("cLaunch","linkRutinebeskrivelsePOS.w").
oMenuXML:AddNode("cImage","icon\png\help2.png").
oMenuXML:AddNode("cParameter","NetHelpPOS\index.html").

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
oMenuXML:AddNode("cMenuLabel","Dagsoppgjør").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Dagsoppgjør").
oMenuXML:AddNode("cLaunch","dagsoppgjorPOS.w").
/*    oMenuXML:AddNode("cImage","png\j0432663.png").*/
oMenuXML:AddNode("cImage","icon\png\document_check.png").
oMenuXML:AddNode("cParameter","30").

IF NOT CAN-DO(cOutletLst,STRING(ttConfig.iButNr)) THEN 
DO:
  oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").  
  oMenuXML:AddNode("cMenuLabel","Reservasjon og NettOrdre").
  DO:
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Reservasjonsordre").
    oMenuXML:AddNode("cLaunch","Reservasjonsordre.w'").
    oMenuXML:AddNode("cImage","icon\png\delivery_note.png").
    oMenuXML:AddNode("cParameter","2").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","GANT Ordrestatus").
    oMenuXML:AddNode("cLaunch","BrowserGantOrdrestatus.w"). 
    oMenuXML:AddNode("cImage","icon\png\web.png").
  END.
END.

IF JBoxSession:Instance:UserId = 'tomn' THEN 
DO:
  oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").  
  oMenuXML:AddNode("cMenuLabel","Lager og fakturakontroll").
  DO:
      oMenuXML:AddBlockNode("sub-menu","menu-item").
      oMenuXML:AddNode("cMenuLabel","Fakturakontroll mottatte overføringer").
      oMenuXML:AddNode("cLaunch","fakturakontrollOV.w'").
      oMenuXML:AddNode("cImage","icon\png\invoice.png").
      oMenuXML:AddNode("cParameter","1").
    
      oMenuXML:AddBlockNode("sub-menu","menu-item").
      oMenuXML:AddNode("cMenuLabel","Fakturakontroll sendte overføringer").
      oMenuXML:AddNode("cLaunch","fakturakontrollOV.w'").
      oMenuXML:AddNode("cImage","icon\png\invoice.png").
      oMenuXML:AddNode("cParameter","2").
  END.
END.

/*oMenuXML:DumpXmlNodes().                   */
/*oMenuXML:saveToFile("c:\temp\meny.xml",no).*/

JBoxMainMenu:Instance:DeleteMenu().
/*run toexcelviafile.p (oMenuXML:hbttNodes,0).*/
JBoxMainMenu:Instance:LoadMenuFromXMLNodeTable(oMenuXML:hbttNodes).
/*JBoxMainMenu:Instance:LoadMenuFromXMLfile("c:\temp\meny.xml").*/

/*JBoxMainMenu:Instance:InitializeObject().*/

/* TN 28/4-20 Meny settes opp uten min og maks knappene. */
/*JBoxMainMenu:Instance:RestartMenu(). TN 28/4-20 Restart av menu må gjøres tidligere. Se lenger oppe. */
JBoxMainMenu:Instance:InitializeObject(1).

/*JBoxMainMenu:Instance:QuitOnClose = NO.  NB: Feiler på kassen. */
JBoxMainMenu:Instance:oMainForm:MaximizeBox = NO.
JBoxMainMenu:Instance:oMainForm:MinimizeBox = NO.

/* NT 25/4-20 NB: Setter vindu størrelsen. Gjøres fordi kassene har oppløsning 1024 x 768 */
/*JBoxMainMenu:Instance:setWindowSize(800, 600).*/
JBoxMainMenu:Instance:setWindowSize(1024, 768).

/*JBoxMainMenu:Instance:StatusText = IF icUserId NE "" THEN icUserId ELSE JBoxSession:Instance:UserId + " / " + JBoxSession:Instance:UserName.*/

JBoxMainMenu:Instance:StatusText = JBoxSession:Instance:UserId + " / " + JBoxSession:Instance:UserName.
JBoxMainMenu:Instance:setWindowCaption("GANT " +
                                       "   Innlogget bruker: [" + ttConfig.cNavnIKasse + "] " +
                                       ttConfig.cSelgerNavn
                                       ).
/*JBoxMainMenu:Instance:ViewStartPanel("http://bafomypage.northeurope.cloudapp.azure.com:81/ansa/").*/
/*JBoxMainMenu:Instance:ViewStartPanel("https://medlemsforsikringer.soderbergpartners.no/ansa").*/

/*IF oFirmaInfo:getParameterString('URLHjemmeside') NE ? THEN                            */
/*  JBoxMainMenu:Instance:ViewStartPanel(oFirmaInfo:getParameterString('URLHjemmeside')).*/
/*ELSE     
                                                                              */
/*JBoxMainMenu:Instance:ViewStartPanel('http://www.polygon.se').*/
/*  JBoxMainMenu:Instance:ViewStartPanel('http://www.gant.no').*/
/*JBoxMainMenu:Instance:ViewStartPanel('https://www.stsportswear.info').*/

/*IF SEARCH('tnc.txt') <> ? THEN                                                                  */
/*  JBoxMainMenu:Instance:ViewStartPanel('file:///C:/NSoft/Polygon/PRSRestCl/NetHelp/index.html').*/
/*ELSE                                                                                            */
/*  JBoxMainMenu:Instance:ViewStartPanel('file:///C:/Polygon/PRSRestCl/NetHelp/index.html').      */

/* Aktiverer et menyvalg etter at menyen er startet opp. */
/*JBoxMainMenu:Instance:StartTabWindow("linkRutinebeskrivelsePOS.w").*/
JBoxMainMenu:Instance:StartTabWindow("dagsoppgjorPOS.w").
                                                
/*


RUN JBoxOOTimer.p PERSIST SET hTimer ("TimerTick",2000).
RUN dlgWelcome1.w.

PROCEDURE SetDlgFrame:
  DEF INPUT PARAM ihDlgFrame AS HANDLE NO-UNDO.
  hDialogFrame = ihDlgFrame.
END.

PROCEDURE "TimerTick":
  RUN SuspendJBoxTimer IN hTimer (YES).
  DELETE PROCEDURE hTimer.
  IF VALID-HANDLE(hDialogFrame) THEN
    APPLY "window-close" TO hDialogFrame. 
END.

*/
        
/*                    
DEF VAR hMainMenu AS HANDLE NO-UNDO.
hMainMenu = JBoxMainMenu:Instance:MainMenuHandle.
RUN getMainFormObject IN hMainMenu (OUTPUT oJBoxMainForm).
oJBoxMainForm:ShowDialog() NO-ERROR.
*/
