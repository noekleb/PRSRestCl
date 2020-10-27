
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

{ttConfig.i}

DEFINE INPUT PARAMETER TABLE FOR ttConfig. 

DEFINE VARIABLE oMenuXML      AS JBoxXmlDoc   NO-UNDO.
DEFINE VARIABLE oJBoxMainForm AS JBoxMainForm NO-UNDO. 
DEFINE VARIABLE hDialogFrame  AS HANDLE       NO-UNDO.
DEFINE VARIABLE hTimer        AS HANDLE       NO-UNDO.
DEFINE VARIABLE bOkUser       AS LOG          NO-UNDO.
DEFINE VARIABLE bTest         AS LOG          NO-UNDO.

ASSIGN 
  bTest = FALSE
  .

ON 'close':U OF THIS-PROCEDURE 
  RETURN.

FIND FIRST ttConfig NO-ERROR.

JBoxSession:Instance:AppServerHandle = ihServer.
DYNAMIC-FUNCTION("setSessionId","validsession").
JBoxSession:Instance:LanguageCode = "NO".
JBoxSession:Instance:BaseLanguageCode = "NO".
JBoxSession:Instance:AppTitle = "PRS".
JBoxSession:Instance:UseAdvGui = YES.
JBoxSession:Instance:setRowShadeColor(240,240,240).

/* Setter informasjon som er tilgjegnelige i hele sesjonen. */
DYNAMIC-FUNCTION("setAttribute",SESSION,"ButNr",STRING(ttConfig.iButNr)).
DYNAMIC-FUNCTION("setAttribute",SESSION,"KasseNr",STRING(ttConfig.iKasseNr)).
DYNAMIC-FUNCTION("setAttribute",SESSION,"webbrukerid",STRING(ttConfig.webbrukerid)).
DYNAMIC-FUNCTION("setAttribute",SESSION,"webpassord",STRING(ttConfig.webpassord)).
/* Eksempel på hvordan det hentes ut igjen.                */
/*MESSAGE DYNAMIC-FUNCTION("getAttribute",SESSION,"butnr").*/
/*VIEW-AS ALERT-BOX.                                       */

RUN JBoxLogin.w (OUTPUT bOkUser). 
IF NOT bOkUser THEN QUIT.

DYNAMIC-FUNCTION("getSessionData").

oMenuXML = NEW JBoxXmlDoc().
oMenuXML:AddBlockNode("menu").
oMenuXML:AddNode("cStateImage","png\j0432620.png"). /* logo */

oMenuXML:AddBlockNode("tv-nav-bar").
oMenuXML:AddBlockNode("sub-menu").
oMenuXML:AddNode("cMenuLabel","Personal").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Registrere inn/ut loging").
oMenuXML:AddNode("cLaunch","InnUtloggingLager.w").
oMenuXML:AddNode("cImage","icon\png\date_time_preferences.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Dagsoppgjør").
oMenuXML:AddNode("cLaunch","dagsoppgjorPOS.w").
oMenuXML:AddNode("cImage","icon\png\document_check.png").

IF JBoxSession:Instance:UserId = 'tomn' THEN
DO:
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Personalsalg").
  oMenuXML:AddNode("cLaunch","XPersonalsalg.w"). 
  oMenuXML:AddNode("cImage","icon\png\purchase_order_cart.png").
END.

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Rutine og funksjonsbeskrivelser").
oMenuXML:AddNode("cLaunch","linkRutinebeskrivelse.w").
oMenuXML:AddNode("cImage","icon\png\help2.png").
oMenuXML:AddNode("cParameter","NetHelp\index.html").
    
oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
oMenuXML:AddNode("cMenuLabel","Behandling av nettbutikkordre").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Bekreftet, men ikke behandlet").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0432663.png").*/
oMenuXML:AddNode("cImage","icon\png\document_check.png").
oMenuXML:AddNode("cParameter","30").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Pakkseddel skrevet").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0432572.png").*/
oMenuXML:AddNode("cImage","icon\png\delivery_note.png").
oMenuXML:AddNode("cParameter","35").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Postpakke etikett skrevet").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
oMenuXML:AddNode("cImage","icon\png\tag.png").
oMenuXML:AddNode("cParameter","40").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Elektronisk plukking").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
oMenuXML:AddNode("cImage","icon\png\shopping_basket.png").
oMenuXML:AddNode("cParameter","42").

IF JBoxSession:Instance:UserId = 'tomn' THEN
DO:
oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Lever speditør").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0433941.png").*/
oMenuXML:AddNode("cImage","icon\png\delivery_man_parcel.png").
oMenuXML:AddNode("cParameter","45").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Bekreftet mottatt speditør").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0433953.png").*/
oMenuXML:AddNode("cImage","icon\png\delivery_man.png").
oMenuXML:AddNode("cParameter","47").
END.

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Levert kunde og fakturert").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0432624.png").*/
oMenuXML:AddNode("cImage","icon\png\package_ok.png").
oMenuXML:AddNode("cParameter","50").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Ikke levert").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0432665.png").*/
oMenuXML:AddNode("cImage","icon\png\package_edit.png").
oMenuXML:AddNode("cParameter","55").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Kanselerte").
oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
/*    oMenuXML:AddNode("cImage","png\j0432665.png").*/
oMenuXML:AddNode("cImage","icon\png\package_error.png").
oMenuXML:AddNode("cParameter","60").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Retur av varer").
oMenuXML:AddNode("cLaunch","ReturScanning.w").
oMenuXML:AddNode("cImage","icon\png\package_delete.png").
oMenuXML:AddNode("cParameter","10").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Bytte av varer").
oMenuXML:AddNode("cLaunch","ReturScanning.w").
oMenuXML:AddNode("cImage","icon\png\package_add.png").
oMenuXML:AddNode("cParameter","20").

IF JBoxSession:Instance:UserId = 'tomn' THEN
DO:
oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Postpakke levert speditør (Skanning)").
oMenuXML:AddNode("cLaunch","PostPakkeScanning.w").
oMenuXML:AddNode("cImage","icon\png\barcode_scanner.png").
END.

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Kunderegister").
oMenuXML:AddNode("cLaunch","Kunderegister.w").
oMenuXML:AddNode("cImage","icon\png\astrologer.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GANT Ordrestatus").
oMenuXML:AddNode("cLaunch","BrowserGantOrdrestatus.w"). 
oMenuXML:AddNode("cImage","icon\png\web.png").

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").  
oMenuXML:AddNode("cMenuLabel","Varemottak, lager og fakturakontroll").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Lagerliste artikkel/størrelse").
oMenuXML:AddNode("cLaunch","LagerListeButArtstr.w").
oMenuXML:AddNode("cImage","icon\png\document_plain.png").
oMenuXML:AddNode("cParameter","16|10").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Lagerliste modell/butikk/størrelse").
oMenuXML:AddNode("cLaunch","LagerListeModButStr.w").
oMenuXML:AddNode("cImage","icon\png\document_plain.png").
oMenuXML:AddNode("cParameter","Meny").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Varemottak nettbutiks lager").
oMenuXML:AddNode("cLaunch","PkSdlBehandling.w").
oMenuXML:AddNode("cImage","icon\png\warehouse.png").
oMenuXML:AddNode("cParameter","20").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Fakturakontroll").
oMenuXML:AddNode("cLaunch","PkSdlBehandling.w").
oMenuXML:AddNode("cImage","icon\png\invoice.png").
oMenuXML:AddNode("cParameter","30").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Bytte butikknr. på pakkseddel").
oMenuXML:AddNode("cLaunch","PkSdlBehandling.w").
oMenuXML:AddNode("cImage","icon\png\warehouse.png").
oMenuXML:AddNode("cParameter","10").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Intern overføring").
oMenuXML:AddNode("cLaunch","InternOverforinger.w").
oMenuXML:AddNode("cImage","icon\png\truck_blue.png").
oMenuXML:AddNode("cParameter","30").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Lagerjustering").
oMenuXML:AddNode("cLaunch","TransRegistrering.w").
oMenuXML:AddNode("cImage","icon\png\dna.png").
oMenuXML:AddNode("cParameter","7").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Marker pakkseddel som sendt Outlet").
oMenuXML:AddNode("cLaunch","PkSdlBehandling.w").
oMenuXML:AddNode("cImage","icon\png\forklift.png").
oMenuXML:AddNode("cParameter","40").

IF JBoxSession:Instance:UserId = 'tomn' THEN
DO: 
oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Web: Gant admin").
oMenuXML:AddNode("cLaunch","BrowserWindow.w"). 
oMenuXML:AddNode("cImage","icon\png\web.png").
oMenuXML:AddNode("cParameter","https://no.gant.com/adminauth/signin").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Web: Progress").
oMenuXML:AddNode("cLaunch","BrowserWindow.w"). 
oMenuXML:AddNode("cImage","icon\png\web.png").
oMenuXML:AddNode("cParameter","http://www.progress.com").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Web: Funksjonsbeskrivelsers").
oMenuXML:AddNode("cLaunch","BrowserWindow.w"). 
oMenuXML:AddNode("cImage","icon\png\web.png").
oMenuXML:AddNode("cParameter","C:\NSoft\Polygon\PRSRestCl\NetHelp\index.html").

END.

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").  
oMenuXML:AddNode("cMenuLabel","Kampanje").

oMenuXML:AddBlockNode("sub-menu","menu-item"). 
oMenuXML:AddNode("cMenuLabel","Kampanjeregister").
oMenuXML:AddNode("cLaunch","kampanjeregister.w").
oMenuXML:AddNode("cImage","icon\png\date_time.png").

/*IF JBoxSession:Instance:UserId = 'tomn' THEN*/
DO: 
  oMenuXML:AddBlockNode("sub-menu","menu-item"). 
  oMenuXML:AddNode("cMenuLabel","Solg% lager").
  oMenuXML:AddNode("cLaunch","SalgLager.w").
  oMenuXML:AddNode("cImage","icon\png\document_chart.png").
  oMenuXML:AddNode("cParameter","16|10").

  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Solgt% aktiv kampanje").
  oMenuXML:AddNode("cLaunch","KampanjeSalg.w").
  oMenuXML:AddNode("cImage","icon\png\chart_line.png").
  oMenuXML:AddNode("cParameter","16|10").
END.

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").  
oMenuXML:AddNode("cMenuLabel","Reservasjoner").
RESERVASJONER:
DO:
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Reservasjoner fra butikk").
  oMenuXML:AddNode("cLaunch","Reservasjonsordre.w").
  oMenuXML:AddNode("cImage","icon\png\purchase_order.png").
  oMenuXML:AddNode("cParameter","1"). /* 1 = Startes fra eCom */
END. /* RESERVASJONER */

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").  
oMenuXML:AddNode("cMenuLabel","Kundeservice").
KUNDESERVICE:
DO:
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Endre vare/antall").
  oMenuXML:AddNode("cLaunch","KundeOrdreEndre.w").
  oMenuXML:AddNode("cImage","icon\png\purchase_order.png").
  oMenuXML:AddNode("cParameter","10").
  
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Rette pris (Feil pris fra Phx)").
  oMenuXML:AddNode("cLaunch","KundeOrdreEndre.w").
  oMenuXML:AddNode("cImage","icon\png\purchase_order.png").
  oMenuXML:AddNode("cParameter","20").
  
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Retur etikett").
  oMenuXML:AddNode("cLaunch","KundeOrdreListe.w").
  oMenuXML:AddNode("cImage","icon\png\tag.png").
  oMenuXML:AddNode("cParameter","99").
  
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","Linker").
  oMenuXML:AddNode("cLaunch","linkSide.w").
  oMenuXML:AddNode("cImage","icon\png\link.png").
END. /* KUNDESERVICE */


IF JBoxSession:Instance:UserId = 'tomn' THEN
DO:
  oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
  oMenuXML:AddNode("cMenuLabel","System").

  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","PRS Brukere").
  oMenuXML:AddNode("cLaunch","prsJBoxUser.w").
  oMenuXML:AddNode("cImage","png\j0431631.png").
  
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","VG").
  oMenuXML:AddNode("cLaunch","http://www.vg.no").
  oMenuXML:AddNode("cLaunchType","URL").
  oMenuXML:AddNode("cImage","ico\tankgreen.ico").
  
  oMenuXML:AddBlockNode("sub-menu","menu-item").
  oMenuXML:AddNode("cMenuLabel","TEST DinTero").
  oMenuXML:AddNode("cLaunch","testDinTero.w").
  oMenuXML:AddNode("cImage","icon\png\web.png").
  
  IF SEARCH('tnc.txt') <> ? THEN 
  DO:
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Lokal fil").
    oMenuXML:AddNode("cLaunch","file:///C:/NSoft/Polygon/PRSRestCl/NetHelp/index.html").
    oMenuXML:AddNode("cLaunchType","URL").
    oMenuXML:AddNode("cImage","ico\tankred.ico").
  END.
  ELSE DO:
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Lokal fil").
    oMenuXML:AddNode("cLaunch","file:///C:/Polygon/PRSRestCl/NetHelp/index.html").
    oMenuXML:AddNode("cLaunchType","URL").
    oMenuXML:AddNode("cImage","ico\tankred.ico").
  END.
  
END.

/*oMenuXML:DumpXmlNodes().                   */
/*oMenuXML:saveToFile("c:\temp\meny.xml",no).*/

JBoxMainMenu:Instance:DeleteMenu().
/*run toexcelviafile.p (oMenuXML:hbttNodes,0).*/
JBoxMainMenu:Instance:LoadMenuFromXMLNodeTable(oMenuXML:hbttNodes).
/*JBoxMainMenu:Instance:LoadMenuFromXMLfile("c:\temp\meny.xml").*/

JBoxMainMenu:Instance:InitializeObject().
/*JBoxMainMenu:Instance:InitializeObject(iiMenuId).*/

/*JBoxMainMenu:Instance:StatusText = IF icUserId NE "" THEN icUserId ELSE JBoxSession:Instance:UserId + " / " + JBoxSession:Instance:UserName.*/
IF ttConfig.DebugAktiv THEN 
  JBoxMainMenu:Instance:StatusText = JBoxSession:Instance:UserId + " / " + JBoxSession:Instance:UserName + ' (DebugAktiv nivå ' + STRING(ttConfig.DebugLevel) + ')'.
ELSE  
  JBoxMainMenu:Instance:StatusText = JBoxSession:Instance:UserId + " / " + JBoxSession:Instance:UserName.

JBoxMainMenu:Instance:setWindowCaption("GANT eCom").
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
JBoxMainMenu:Instance:StartTabWindow("linkRutinebeskrivelse.w").
                                                
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
