&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

DEFINE VARIABLE ix                    AS INTEGER                          NO-UNDO.
DEFINE VARIABLE hBrowse               AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hQuery                AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hToolbar              AS HANDLE                           NO-UNDO.
DEFINE VARIABLE hFieldMap             AS HANDLE                           NO-UNDO.
DEFINE VARIABLE oContainer            AS JBoxContainer                    NO-UNDO.
DEFINE VARIABLE cLogg                 AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cWebBrukerid          AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cWebPassord           AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE iLoop                 AS INTEGER                          NO-UNDO.
DEFINE VARIABLE cRecord               AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE lcHtml                AS LONGCHAR                         NO-UNDO.
DEFINE VARIABLE cEkstOrdreNr          AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cOrdreStatus          AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE lKOrdre_Id            AS DECIMAL                          NO-UNDO.
DEFINE VARIABLE cFeltVerdier          AS CHARACTER                        NO-UNDO.

DEFINE VARIABLE cMobilNr              AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cSubject              AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cMessage              AS CHARACTER                        NO-UNDO.
DEFINE VARIABLE cOrdreInfo            AS CHARACTER                        NO-UNDO.

DEFINE VARIABLE obOk                  AS LOG                              NO-UNDO.
DEFINE VARIABLE ocReturn              AS CHARACTER                        NO-UNDO.

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

DEFINE VARIABLE oJBoxInternetExplorer AS uc.JBoxInternetExplorer          NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rect1 btnTilbake btnFrem btnSMS btnNotat ~
JBoxInternetExplorer 
&Scoped-Define DISPLAYED-OBJECTS fiOrdreInfo JBoxInternetExplorer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WebDocumentLoaded C-Win 
FUNCTION WebDocumentLoaded RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WebNavigated C-Win 
FUNCTION WebNavigated RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WebNavigating C-Win 
FUNCTION WebNavigating RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFrem 
  LABEL "-->" 
  SIZE 5 BY 1.14.

DEFINE BUTTON btnNotat 
  LABEL "Notat.." 
  SIZE 15 BY 1.14 TOOLTIP "Legg inn notat på ordren som vises.".

DEFINE BUTTON btnSMS 
  LABEL "SMS" 
  SIZE 8 BY 1.14.

DEFINE BUTTON btnTilbake 
  LABEL "<--" 
  SIZE 5 BY 1.14.

DEFINE VARIABLE JBoxInternetExplorer AS CHARACTER 
  VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
  SIZE 126.4 BY 9.52 NO-UNDO.

DEFINE VARIABLE fiOrdreInfo          AS CHARACTER FORMAT "X(256)":U 
  VIEW-AS FILL-IN NATIVE 
  SIZE 91.4 BY 1 TOOLTIP "Informasjon om ordre som vises"
  FONT 6 NO-UNDO.

DEFINE RECTANGLE rect1
  EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
  SIZE 126.2 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
  btnTilbake AT ROW 1.24 COL 2 WIDGET-ID 4
  btnFrem AT ROW 1.24 COL 7 WIDGET-ID 6
  btnSMS AT ROW 1.24 COL 12 WIDGET-ID 10
  btnNotat AT ROW 1.24 COL 20 WIDGET-ID 12
  fiOrdreInfo AT ROW 1.33 COL 33 COLON-ALIGNED NO-LABEL
  JBoxInternetExplorer AT ROW 2.67 COL 1.6 NO-LABEL WIDGET-ID 2
  rect1 AT ROW 1.19 COL 1.8 WIDGET-ID 8
  WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
  SIDE-LABELS NO-UNDERLINE THREE-D 
  AT COL 1 ROW 1
  SCROLLABLE SIZE 127.4 BY 11.38 WIDGET-ID 100.


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
    HEIGHT             = 11.33
    WIDTH              = 128.2
    MAX-HEIGHT         = 23.52
    MAX-WIDTH          = 128.2
    VIRTUAL-HEIGHT     = 23.52
    VIRTUAL-WIDTH      = 128.2
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
ASSIGN 
  FRAME DEFAULT-FRAME:HEIGHT = 11.38
  FRAME DEFAULT-FRAME:WIDTH  = 127.4.

ASSIGN 
  btnNotat:HIDDEN IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN fiOrdreInfo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
  fiOrdreInfo:READ-ONLY IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN C-Win:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
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
  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt'
      ).
  
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFrem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFrem C-Win
ON CHOOSE OF btnFrem IN FRAME DEFAULT-FRAME /* --> */
  DO:
    oJBoxInternetExplorer:getDotNetWidget():GoForward().  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNotat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNotat C-Win
ON CHOOSE OF btnNotat IN FRAME DEFAULT-FRAME /* Notat.. */
  DO:
    RUN OrdreNotatRecord.  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSMS C-Win
ON CHOOSE OF btnSMS IN FRAME DEFAULT-FRAME /* SMS */
  DO:
    RUN sendSMSTilKunde.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilbake
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilbake C-Win
ON CHOOSE OF btnTilbake IN FRAME DEFAULT-FRAME /* <-- */
  DO:
    oJBoxInternetExplorer:getDotNetWidget():GoBack().  
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  DISPLAY fiOrdreInfo JBoxInternetExplorer 
    WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rect1 btnTilbake btnFrem btnSMS btnNotat JBoxInternetExplorer 
    WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeComponents C-Win 
PROCEDURE InitializeComponents :
&IF DEFINED(AdvGuiWin) &THEN
  DO WITH FRAME {&FRAME-NAME}:

    oJBoxInternetExplorer = NEW uc.JBoxInternetExplorer(THIS-PROCEDURE,JBoxInternetExplorer:HANDLE).
    oJBoxInternetExplorer:RegisterWithJukeBox(YES).
    oJboxInternetExplorer:ScriptErrorsSuppressed = YES.
    oJBoxInternetExplorer:getDotNetWidget():AllowNavigation = YES.
    /*  oJboxInternetExplorer:setURI("Https://no.gant.com/admin").*/
    /*  oJboxInternetExplorer:setURI("https://www.progress.com/").*/
  
    oContainer:setNoResizeY("rect1").
    
  END.
&ENDIF
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
    SUBSCRIBE 'settURL' ANYWHERE.
    
    rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.
  
    ASSIGN 
      cLogg = 'BrowserGantOrdrestatus' + REPLACE(STRING(TODAY),'/','')
      .

    RUN InitializeComponents.
  
    btnSMS:SENSITIVE = FALSE. 
    btnNotat:SENSITIVE = FALSE. 
    btnNotat:HIDDEN = TRUE.

    oJboxInternetExplorer:setURI("https://no.gant.com/adminauth/signin").
    cWebBrukerId = DYNAMIC-FUNCTION("getAttribute",SESSION,"webbrukerid").
    cWebPassord  = DYNAMIC-FUNCTION("getAttribute",SESSION,"webpassord").
  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start' 
      ).  
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NotatRecord C-Win 
PROCEDURE NotatRecord :
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcRowId AS CHARACTER NO-UNDO.
  
/*  IF cEkstOrdreNr = '' THEN                                                              */
/*  DO:                                                                                    */
/*    JBoxSession:Instance:ViewMessage("Ingen ordre tilgjengelig (" + cEkstOrdreNr + ").").*/
/*    RETURN.                                                                              */
/*  END.                                                                                   */
/*                                                                                         */
/*  RUN SUPER.                                                                             */
/*                                                                                         */
/*  /* Her lagres teksten i et notatfelt i recorden. */                                    */
/*  DO:                                                                                    */
/*    IF NOT DYNAMIC-FUNCTION('DoUpdate','KOrdreHode','',                                  */
/*       '',pcRowId,                                                                       */
/*        "VerkstedMerknad","Notat er lagt inn.",                                          */
/*       YES) THEN                                                                         */
/*      DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"",""). */
/*  END.                                                                                   */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterSkjerm C-Win
PROCEDURE OppdaterSkjerm:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    lcHtml = STRING(oJBoxInternetExplorer:getDotNetWidget():Document:body:innerHTML).
      
    DO iLoop = 1 TO NUM-ENTRIES(lcHtml,CHR(10)):
      cRecord = TRIM(ENTRY(iLoop,lcHtml,CHR(10))).
      
      IF cRecord MATCHES '*order-title*' THEN
      DO:
        ASSIGN 
          cEkstOrdreNr = ENTRY(4,cRecord,' ')
          cOrdreStatus = TRIM(ENTRY(8,cRecord,' ')) 
          .  
        CASE cOrdreStatus:
          WHEN 'At'       THEN cOrdreStatus = 'At Wharehouse'. 
          WHEN 'In'       THEN cOrdreStatus = 'In Transit'. 
          WHEN 'Awaiting' THEN cOrdreStatus = 'Awaiting Pickup'. 
          WHEN 'Pickup'   THEN cOrdreStatus = 'Pickup Overdue'. 
        END CASE.   
          
        rclStandardFunksjoner:SkrivTilLogg(cLogg,
          '    ' + 'OrdreNr: ' + cEkstOrdreNr + ' Ordrestatus: ' + cOrdreStatus + '.' 
          ).  

        cFeltVerdier = DYNAMIC-FUNCTION("getFieldValues","FIRST KOrdreHode", "WHERE  EkstOrdreNr = '" + 
          cEkstOrdreNr + "' AND LevStatus = '50'",
          "KOrdre_Id,Levstatus,Navn,MobilTlf").
        rclStandardFunksjoner:SkrivTilLogg(cLogg,
          '    ' + 'cFeltVerdier: ' + cFeltVerdier + '.' 
          ).
        IF NUM-ENTRIES(cFeltVerdier,'|') >= 4 THEN
        DO:
          ASSIGN
            lKOrdre_Id               = DEC(ENTRY(1,cFeltVerdier,'|'))
            cOrdreInfo               = ENTRY(3,cFeltVerdier,'|')
            cMobilNr                 = ENTRY(4,cFeltVerdier,'|') 
            fiOrdreInfo:SCREEN-VALUE = cOrdreInfo + ' Ordrestatus: ' + cOrdreStatus
            btnSMS:SENSITIVE         = TRUE 
            /*              btnNotat:SENSITIVE       = TRUE.*/
            .
        END.           
      END. 
    END.

  END.
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendSMSTilKunde C-Win 
PROCEDURE sendSMSTilKunde :
  /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:

    IF cMobilNr = '' THEN 
    DO:
      JBoxSession:Instance:ViewMessage('Mobilnr. ikke angitt!'). 
      RETURN.
    END.

    /*    IF NOT JBoxSession:Instance:ViewQuestionOkCancel('Skal SMS sendes?') THEN*/
    /*      RETURN.                                                                */

    ASSIGN 
      cSubject = 'Pakke klar til avhenting'
      cMessage = 'Hei, vi har en pakke til deg fra vår nettbutikk.' + CHR(10) + 'Hilsen GANT Tønsberg'
      . 

    CURRENT-WINDOW:SENSITIVE = FALSE.
    RUN SMS.w (lKOrdre_Id,
      cMobilNr,
      cSubject,
      cMessage,
      OUTPUT obOk,
      OUTPUT ocReturn).
    
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}. /* Gjør vinduet aktivt igjen. */
    SESSION:SET-WAIT-STATE("").
    CURRENT-WINDOW:SENSITIVE = TRUE.

    RETURN.
    
  END. /* FRAMe */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settURL C-Win
PROCEDURE settURL:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcURL AS CHARACTER NO-UNDO.
  
  oJboxInternetExplorer:setURI(pcURL).
  
END PROCEDURE.
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WebDocumentLoaded C-Win 
FUNCTION WebDocumentLoaded RETURNS CHARACTER
  (  ):
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
     
                  HtmlDocument doc = webBrowser1.Document;
                  HtmlElement username = doc.GetElementById("UserName");
                  HtmlElement password = doc.GetElementById("Password");
                  HtmlElement submit = doc.GetElementById("submit");
                  username.SetAttribute("value", "XXXXXXXX");
                  password.SetAttribute("value", "YYYYYYYYYY");
                  submit.InvokeMember("click");
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE bResult AS CHARACTER NO-UNDO.
    
  DO WITH FRAME {&FRAME-NAME}:
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + 'WebDocumentLoaded: ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Url) 
      ).  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + 'Document: ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document) 
      ).  
    IF STRING(oJBoxInternetExplorer:getDotNetWidget():Url) = "https://no.gant.com/adminauth/signin" THEN 
    DO: 
      rclStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + 'GetElementById:author ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("author")) 
        ).  
      rclStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + 'GetElementById:_username ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_username")) 
        ).  
      rclStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + 'GetElementById:_username:value ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_username"):GetAttribute("value")) 
        ).  
      rclStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + 'GetElementById:password ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("password")) 
        ).  
      rclStandardFunksjoner:SkrivTilLogg(cLogg,
        '  ' + 'GetElementById:_submit ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_submit")) 
        ).  

      cWebBrukerid = DYNAMIC-FUNCTION("getAttribute",SESSION,"webbrukerid").
      cWebPassord  = DYNAMIC-FUNCTION("getAttribute",SESSION,"webpassord").
      
      oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_username"):SetAttribute("value",cWebBrukerid). 
      oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("password"):SetAttribute("value",cWebPassord). 
    END.

    /* Når det gjøres view på en ordre, hentes dennes status og info. Informasjon om ordren legges opp i skjermen       */
    /* i feltet på verktøylinjen. Videre enables SMS knappen. Bruker kan da sende SMS så lenge denne ordren er i fokus. */
    IF STRING(oJBoxInternetExplorer:getDotNetWidget():Url) BEGINS "https://no.gant.com/admin/orders/view/" THEN
    VIEWBLOKK: 
    DO: 
      RUN OppdaterSkjerm.
    END. /* VIEWBLOKK */
    /* Her har bruker beveget seg til en annen side, og man har ikke lenger ordren i fokus. Feltene i verktøylinjen */
    /* blankes, knappen disables. SMS kan ikke lenger sendes.                                                       */
    ELSE 
    NOVIEWBLOKK: DO:
      ASSIGN 
        lKOrdre_Id               = 0
        cMobilNr                 = ''
        cOrdreInfo               = ''
        cEkstOrdreNr             = ''
        cOrdreStatus             = '' 
        fiOrdreInfo:SCREEN-VALUE = ''
        btnSMS:SENSITIVE         = FALSE 
        btnNotat:SENSITIVE       = FALSE. 
      .  
    END. /* NOVIEWBLOKK */
  END.
  RETURN bResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WebNavigated C-Win 
FUNCTION WebNavigated RETURNS CHARACTER
  (  ):
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE result AS CHARACTER NO-UNDO.

  rclStandardFunksjoner:SkrivTilLogg(cLogg,
    '  ' + 'WebNavigated: ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Url) 
    ).  
               
  IF 'https://no.gant.com/' = STRING(oJBoxInternetExplorer:getDotNetWidget():Url) THEN 
    oJboxInternetExplorer:setURI("Https://no.gant.com/admin").     
                     
  RETURN result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WebNavigating C-Win 
FUNCTION WebNavigating RETURNS CHARACTER
  (  ):
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE result AS CHARACTER NO-UNDO.

  rclStandardFunksjoner:SkrivTilLogg(cLogg,
    '  ' + 'WebNavigating: ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Url) 
    ).  

  IF STRING(oJBoxInternetExplorer:getDotNetWidget():Url) = "https://no.gant.com/adminauth/signin" THEN 
  DO: 
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + '(2)GetElementById:author ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("author")) 
      ).  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + 'GetElementById:_username ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_username")) 
      ).  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + 'GetElementById:_username:value ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_username"):GetAttribute("value")) 
      ).  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + 'GetElementById:password ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("password")) 
      ).  
    rclStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ' + 'GetElementById:_submit ' + STRING(oJBoxInternetExplorer:getDotNetWidget():Document:GetElementById("_submit")) 
      ).  
  END.

  RETURN result.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

