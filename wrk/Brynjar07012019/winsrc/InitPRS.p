
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
DEF INPUT PARAM icUserId AS CHAR NO-UNDO.
DEF INPUT PARAM ihServer AS HANDLE NO-UNDO.

DEF VAR oMenuXML         AS JBoxXmlDoc NO-UNDO.
DEF VAR oJBoxMainForm    AS JBoxMainForm NO-UNDO. 
DEF VAR hDialogFrame     AS HANDLE NO-UNDO.
DEF VAR hTimer           AS HANDLE NO-UNDO.
DEF VAR bOkUser          AS LOG NO-UNDO.


ON 'close':U OF THIS-PROCEDURE RETURN.

JBoxSession:Instance:AppServerHandle = ihServer.
JBoxSession:Instance:LanguageCode = "NO".
JBoxSession:Instance:BaseLanguageCode = "NO".
JBoxSession:Instance:AppTitle = "MyPage".
JBoxSession:Instance:UseAdvGui = YES.
JBoxSession:Instance:setRowShadeColor(240,240,240).

RUN JBoxLogin.w (OUTPUT bOkUser).
IF NOT bOkUser THEN QUIT.

DYNAMIC-FUNCTION("getSessionData").


oMenuXML = NEW JBoxXmlDoc().
oMenuXML:AddBlockNode("menu").
oMenuXML:AddNode("cStateImage","png\j0432620.png"). /* logo */

oMenuXML:AddBlockNode("tv-nav-bar").
  oMenuXML:AddBlockNode("sub-menu").
  oMenuXML:AddNode("cMenuLabel","PRS ordrebehandling").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Polygon kunder").
    oMenuXML:AddNode("cLaunch","Polygon.w").
    oMenuXML:AddNode("cImage","png\j0431631.png").
/*
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Forsikringsrapport pivot dekninger").
    oMenuXML:AddNode("cLaunch","ForsikringPivot.w").
    oMenuXML:AddNode("cImage","gif\magnify.gif").
  
  oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
  oMenuXML:AddNode("cMenuLabel","Mypage grunndata").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Score poeng").
    oMenuXML:AddNode("cLaunch","ScoreParam.w").
    oMenuXML:AddNode("cImage","ico\wizard.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Parameter type").
    oMenuXML:AddNode("cLaunch","ScoreParameterType.w").
    oMenuXML:AddNode("cImage","ico\wsecedit.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Generelle koder").
    oMenuXML:AddNode("cLaunch","CodeValue.w").
    oMenuXML:AddNode("cImage","ico\boxes.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Hendelsestyper").
    oMenuXML:AddNode("cLaunch","EventType.w").
    oMenuXML:AddNode("cImage","ico\app32.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Forespørselstyper").
    oMenuXML:AddNode("cLaunch","RequestType.w").
    oMenuXML:AddNode("cImage","ico\blocks.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Topp 10 parameter gruppe").
    oMenuXML:AddNode("cLaunch","ScoreCategory.w").
    oMenuXML:AddNode("cImage","ico\folders.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Koderegister").
    oMenuXML:AddNode("cLaunch","CodeValue.w").
    oMenuXML:AddNode("cImage","ico\dictionary.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Pristabell").
    oMenuXML:AddNode("cLaunch","Pristabell.w").
    oMenuXML:AddNode("cImage","png\j0441326.png").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Sysreg").
    oMenuXML:AddNode("cLaunch","Sysreg.w").
    oMenuXML:AddNode("cImage","ico\verhistory32.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","System Parametre(ny)").
    oMenuXML:AddNode("cLaunch","wSystemParameterType.w").
    oMenuXML:AddNode("cImage","ico\verhistory32.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Menyvedlikehold").
    oMenuXML:AddNode("cLaunch","JBoxMsTreeMenuMaint.w").
    oMenuXML:AddNode("cImage","ico\pcase.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Brukervedlikehold").
    oMenuXML:AddNode("cLaunch","JBoxUser.w").
    oMenuXML:AddNode("cImage","png\j0432612.png").
    
  oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
  oMenuXML:AddNode("cMenuLabel","Mypage rapporter").
  
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Statistikk, prissjekk").
    oMenuXML:AddNode("cLaunch","PriceCheckStat.w").
    oMenuXML:AddNode("cImage","ico\btrez1.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Forespørsler").
    oMenuXML:AddNode("cLaunch","Request.w").
    oMenuXML:AddNode("cImage","ico\ico_Compil320001.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Hendelseslogg").
    oMenuXML:AddNode("cLaunch","EventLog.w").
    oMenuXML:AddNode("cImage","ico\alarm.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Front-end hendelser").
    oMenuXML:AddNode("cLaunch","FrontEndEventLog.w").
    oMenuXML:AddNode("cImage","ico\FFupdater.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Sesjonstabell").
    oMenuXML:AddNode("cLaunch","LoginSession.w").
    oMenuXML:AddNode("cImage","ico\usermain32.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Handlekurv").
    oMenuXML:AddNode("cLaunch","BasketHeader.w").
    oMenuXML:AddNode("cImage","png\j0431639.png").
    

  oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
  oMenuXML:AddNode("cMenuLabel","Bafo Div").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Fakturaoversikt").
    oMenuXML:AddNode("cLaunch","Fakturaoversikt.w").
    oMenuXML:AddNode("cLaunchType","data-browse").
    oMenuXML:AddNode("cImage","ico\tankgreen.ico").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Sett VG prosjektnr").
    oMenuXML:AddNode("cLaunch","SjekkVgProsjnr.w").
    oMenuXML:AddNode("cImage","ico\arrowdouble.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Medlemsliste").
    oMenuXML:AddNode("cLaunch","MemberList.w").
    oMenuXML:AddNode("cImage","png\j0432612.png").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Medlem drill-down").
    oMenuXML:AddNode("cLaunch","MedlemQry.w").
    oMenuXML:AddNode("cLaunchType","data-browse").
    oMenuXML:AddNode("cImage","png\j0432612.png").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Webkø 2.0").
    oMenuXML:AddNode("cLaunch","WebQueue.w").
    oMenuXML:AddNode("cImage","png\j0440384.png").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Produktoversikt").
    oMenuXML:AddNode("cLaunch","Produktoversikt.w").
    oMenuXML:AddNode("cLaunchType","data-browse").
    oMenuXML:AddNode("cImage","icons\plugin.png").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Forsikringsoversikt").
    oMenuXML:AddNode("cLaunch","Forsoversikt.w").
    oMenuXML:AddNode("cLaunchType","data-browse").
    oMenuXML:AddNode("cImage","png\j0433953.png").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","SysReg").
    oMenuXML:AddNode("cLaunch","SysReg.w").
    oMenuXML:AddNode("cImage","ico\ntservice.ico").

    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Postnr").
    oMenuXML:AddNode("cLaunch","Postnr.w").
    oMenuXML:AddNode("cImage","gif\flag_no.gif").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Priser").
    oMenuXML:AddNode("cLaunch","Pristabell.w").
    oMenuXML:AddNode("cImage","gif\restart32.gif").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Ordre").
    oMenuXML:AddNode("cLaunch","Ordre.w").
    oMenuXML:AddNode("cImage","png\j0433808.png").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Kombiprodukter").
    oMenuXML:AddNode("cLaunch","Kombiprodukt.w").
    oMenuXML:AddNode("cImage","gif\afmultwind.gif").
    
    oMenuXML:AddBlockNode("sub-menu","menu-item").
    oMenuXML:AddNode("cMenuLabel","Produkter og dekninger").
    oMenuXML:AddNode("cLaunch","ProdDekning.w").
    oMenuXML:AddNode("cImage","ico\shell322.ico").

/*    oMenuXML:AddBlockNode("sub-menu","menu-item").      */
/*    oMenuXML:AddNode("cMenuLabel","Menyvedlikehold").   */
/*    oMenuXML:AddNode("cLaunch","JBoxMsTreeMenuMaint.w").*/
/*    oMenuXML:AddNode("cImage","ico\pcase.ico").         */

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
oMenuXML:AddNode("cMenuLabel","Klient admin").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Klient").
oMenuXML:AddNode("cLaunch","Klient.w").
oMenuXML:AddNode("cImage","ico\sbmejb.ico").


oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Subklient").
oMenuXML:AddNode("cLaunch","Subklient.w").
oMenuXML:AddNode("cImage","ikon\date_time.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Subklient leverandør").
oMenuXML:AddNode("cLaunch","SubklientLeverandor.w").
oMenuXML:AddNode("cImage","ikon\date_time.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Selger").
oMenuXML:AddNode("cLaunch","Selger.w").
oMenuXML:AddNode("cImage","ikon\date_time.ico").

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
oMenuXML:AddNode("cMenuLabel","BATCH admin").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Scheduler").
oMenuXML:AddNode("cLaunch","wJobbKoScheduler.w").
oMenuXML:AddNode("cImage","ikon\date_time.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Jobbkø").
oMenuXML:AddNode("cLaunch","wJobbKo.w").
oMenuXML:AddNode("cImage","ico\shell322.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Jobbkø loggtype").
oMenuXML:AddNode("cLaunch","wJobbKoLoggType.w").
oMenuXML:AddNode("cImage","ico\shell322.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Filkatalog").
oMenuXML:AddNode("cLaunch","wFilKatalog.w").
oMenuXML:AddNode("cImage","ikon\folder_document.png").

/*oMenuXML:AddBlockNode("sub-menu","menu-item").   */
/*oMenuXML:AddNode("cMenuLabel","Fakturahode").    */
/*oMenuXML:AddNode("cLaunch","winsrc\wFaktHode.w").*/
/*oMenuXML:AddNode("cImage","ico\shell322.ico").   */

oMenuXML:AddBlockNode("sub-menu","sub-menu").
oMenuXML:AddNode("cMenuLabel","Kode oppsett").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Scheduler Intervall").
oMenuXML:AddNode("cLaunch","wJobbKoSchedulerIntervall.w").
oMenuXML:AddNode("cImage","ikon\date_time.ico").

/*oMenuXML:AddBlockNode("sub-menu","menu-item").  */
/*oMenuXML:AddNode("cMenuLabel","ePost medlem").  */
/*oMenuXML:AddNode("cLaunch","wAdrTlfmm.w").      */
/*oMenuXML:AddNode("cImage","ikon\date_time.ico").*/

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Jobbkø type").
oMenuXML:AddNode("cLaunch","wJobbkoType.w").
oMenuXML:AddNode("cImage","ico\shell322.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Jobbkø status").
oMenuXML:AddNode("cLaunch","wJobbkoStatus.w").
oMenuXML:AddNode("cImage","ico\shell322.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","EndringLogg type").
oMenuXML:AddNode("cLaunch","wEndringLoggType.w").
oMenuXML:AddNode("cImage","ico\shell322.ico").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Filkatalog parameter").
oMenuXML:AddNode("cLaunch","wFilkatalogparameter.w").
oMenuXML:AddNode("cImage","ikon\folder_window.png").

oMenuXML:AddBlockNode("tv-nav-bar","sub-menu").
oMenuXML:AddNode("cMenuLabel","GDPR").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GDPR oppsett").
oMenuXML:AddNode("cLaunch","wGDPRTabell.w").
oMenuXML:AddNode("cImage","ikon\tables.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GDPR PolicyType").
oMenuXML:AddNode("cLaunch","wGDPRPolicyType.w").
oMenuXML:AddNode("cImage","ikon\folder2_blue.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GDPR Policy").
oMenuXML:AddNode("cLaunch","wGDPRPolicy.w").
oMenuXML:AddNode("cImage","ikon\folder2_blue.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GDPR unntak").
oMenuXML:AddNode("cLaunch","wGDPRPolicyUnntak.w").
oMenuXML:AddNode("cImage","ikon\folder2_green.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GDPR Produktpolicy").
oMenuXML:AddNode("cLaunch","wGDPRProduktPolicy.w").
oMenuXML:AddNode("cImage","ikon\folder_cubes.png").

oMenuXML:AddBlockNode("sub-menu","menu-item").
oMenuXML:AddNode("cMenuLabel","GDPR Samtykketype").
oMenuXML:AddNode("cLaunch","wGDPRSamtykkeType.w").
oMenuXML:AddNode("cImage","ikon\folder.png").

oMenuXML:AddBlockNode("menu","menu-item").
oMenuXML:AddNode("cMenuLabel","Menyoppsett").
oMenuXML:AddNode("cLaunch","jboxMSTreeMenuMaint.w").
oMenuXML:AddNode("cImage","ico\wizard.ico").
END.    
*/

/*oMenuXML:DumpXmlNodes().                   */
/*oMenuXML:saveToFile("c:\temp\meny.xml",no).*/

JBoxMainMenu:Instance:DeleteMenu().
/*run toexcelviafile.p (oMenuXML:hbttNodes,0).*/
JBoxMainMenu:Instance:LoadMenuFromXMLNodeTable(oMenuXML:hbttNodes).
/*JBoxMainMenu:Instance:LoadMenuFromXMLfile("c:\temp\meny.xml").*/

JBoxMainMenu:Instance:InitializeObject().
/*JBoxMainMenu:Instance:InitializeObject(iiMenuId).*/

JBoxMainMenu:Instance:StatusText = IF icUserId NE "" THEN icUserId ELSE JBoxSession:Instance:UserId + " / " + JBoxSession:Instance:UserName.
JBoxMainMenu:Instance:setWindowCaption("Polygon " + " - TEST").
/*JBoxMainMenu:Instance:ViewStartPanel("http://bafomypage.northeurope.cloudapp.azure.com:81/ansa/").*/
/*JBoxMainMenu:Instance:ViewStartPanel("https://medlemsforsikringer.soderbergpartners.no/ansa").*/

/*IF oFirmaInfo:getParameterString('URLHjemmeside') NE ? THEN                            */
/*  JBoxMainMenu:Instance:ViewStartPanel(oFirmaInfo:getParameterString('URLHjemmeside')).*/
/*ELSE                                                                                   */
  JBoxMainMenu:Instance:ViewStartPanel('http://www.polygon.se').


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
