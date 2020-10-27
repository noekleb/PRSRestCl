&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME SMSWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS SMSWin 
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
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEFINE VARIABLE lKOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
  DEFINE VARIABLE icMobilNr AS CHARACTER NO-UNDO INIT "9092 3875".
  DEFINE VARIABLE icSubject AS CHARACTER NO-UNDO INIT "Emne".
  DEFINE VARIABLE icMessage AS CHARACTER NO-UNDO INIT "Ny test fra Skotex".
  DEFINE VARIABLE obOk      AS LOG  NO-UNDO.
  DEFINE VARIABLE ocMessage AS CHARACTER NO-UNDO.
&ELSE
  DEFINE INPUT  PARAMETER lKOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>>9" NO-UNDO.
  DEFINE INPUT  PARAMETER icMobilNr AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER icSubject AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER icMessage AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER obOk      AS LOG  NO-UNDO.
  DEFINE OUTPUT PARAMETER ocMessage AS CHARACTER NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE bOk         AS LOG    NO-UNDO.
DEFINE VARIABLE ix          AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBrowse     AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
DEFINE VARIABLE hToolbar    AS HANDLE NO-UNDO.
DEFINE VARIABLE hFieldMap   AS HANDLE NO-UNDO.
DEFINE VARIABLE oContainer  AS JBoxContainer NO-UNDO.
DEFINE VARIABLE cReturn     AS CHARACTER NO-UNDO.

DEFINE VARIABLE cMailServer      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAuthorize       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAuthType        AS CHARACTER NO-UNDO.   
DEFINE VARIABLE cMailUser        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailPwd         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailProgram     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSmsDomain       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSMSReplyTo      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSMSProvider     AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiEmne edSMS fiChars fiNumMsg fiTil ~
fiProvider fiSvarTil fiDomain btnSend btnCancel tbVisLogg 
&Scoped-Define DISPLAYED-OBJECTS fiEmne edSMS fiChars fiNumMsg fiTil ~
fiProvider fiSvarTil fiDomain tbVisLogg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR SMSWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14 TOOLTIP "Avbryter sending."
     BGCOLOR 8 .

DEFINE BUTTON btnSend AUTO-END-KEY DEFAULT 
     LABEL "Send" 
     SIZE 15 BY 1.14 TOOLTIP "Sender SMS."
     BGCOLOR 8 .

DEFINE VARIABLE edSMS AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 71 BY 8.81 NO-UNDO.

DEFINE VARIABLE fiChars AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Antall tegn i sms teksten." NO-UNDO.

DEFINE VARIABLE fiDomain AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiEmne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 TOOLTIP "SMS'ens emne" NO-UNDO.

DEFINE VARIABLE fiNumMsg AS INTEGER FORMAT "->>>9":U INITIAL 0 
     LABEL "antall medlinger" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Antall meldinger" NO-UNDO.

DEFINE VARIABLE fiProvider AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fiSvarTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Svar" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 TOOLTIP "For svar på SMS." NO-UNDO.

DEFINE VARIABLE fiTil AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 TOOLTIP "Mottager av SMS" NO-UNDO.

DEFINE VARIABLE tbVisLogg AS LOGICAL INITIAL NO 
     LABEL "Vis logg" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 TOOLTIP "Viser loggfil etter sending." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fiEmne AT ROW 1.95 COL 10.4 COLON-ALIGNED
     edSMS AT ROW 3.62 COL 5 NO-LABEL WIDGET-ID 4
     fiChars AT ROW 13.14 COL 14.2 COLON-ALIGNED
     fiNumMsg AT ROW 13.14 COL 51.4 COLON-ALIGNED
     fiTil AT ROW 14.19 COL 14.2 COLON-ALIGNED
     fiProvider AT ROW 14.19 COL 35.4 COLON-ALIGNED NO-LABEL
     fiSvarTil AT ROW 15.24 COL 14.2 COLON-ALIGNED
     fiDomain AT ROW 15.24 COL 35.4 COLON-ALIGNED NO-LABEL
     btnSend AT ROW 16.71 COL 5.4 WIDGET-ID 6
     btnCancel AT ROW 16.71 COL 60.8 WIDGET-ID 8
     tbVisLogg AT ROW 16.91 COL 29 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 77.6 BY 17.33
         DEFAULT-BUTTON btnCancel CANCEL-BUTTON btnSend WIDGET-ID 100.


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
  CREATE WINDOW SMSWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Send SMS"
         HEIGHT             = 17.33
         WIDTH              = 79
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB SMSWin 
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
/* SETTINGS FOR WINDOW SMSWin
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 17.33
       FRAME DEFAULT-FRAME:WIDTH            = 77.6.

ASSIGN 
       edSMS:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE.

ASSIGN 
       tbVisLogg:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(SMSWin)
THEN SMSWin:HIDDEN = YES.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME SMSWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SMSWin SMSWin
ON END-ERROR OF SMSWin /* Send SMS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SMSWin SMSWin
ON WINDOW-CLOSE OF SMSWin /* Send SMS */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel SMSWin
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSend SMSWin
ON CHOOSE OF btnSend IN FRAME DEFAULT-FRAME /* Send */
DO:
  RUN SendSMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME edSMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL edSMS SMSWin
ON VALUE-CHANGED OF edSMS IN FRAME DEFAULT-FRAME
DO:
  ASSIGN fiChars  = LENGTH(fiEmne:SCREEN-VALUE) + LENGTH(edSMS:SCREEN-VALUE)
         fiNumMsg = TRUNC(fiChars / 161,0) + 1
         .
  DISPLAY fiChars fiNumMsg WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiEmne
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiEmne SMSWin
ON VALUE-CHANGED OF fiEmne IN FRAME DEFAULT-FRAME /* Emne */
DO:
  ASSIGN fiChars  = LENGTH(fiEmne:SCREEN-VALUE) + LENGTH(edSMS:SCREEN-VALUE)
         fiNumMsg = TRUNC(fiChars / 161,0) + 1
         .
  DISPLAY fiChars fiNumMsg WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK SMSWin 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI SMSWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(SMSWin)
  THEN DELETE WIDGET SMSWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI SMSWin  _DEFAULT-ENABLE
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
  DISPLAY fiEmne edSMS fiChars fiNumMsg fiTil fiProvider fiSvarTil fiDomain 
          tbVisLogg 
      WITH FRAME DEFAULT-FRAME IN WINDOW SMSWin.
  ENABLE fiEmne edSMS fiChars fiNumMsg fiTil fiProvider fiSvarTil fiDomain 
         btnSend btnCancel tbVisLogg 
      WITH FRAME DEFAULT-FRAME IN WINDOW SMSWin.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject SMSWin 
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
  cReturn = DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1",
                             "WHERE SysHId = 50 AND SysGr = 50 AND ParaNr < 10 BY ParaNr").
  IF cReturn = "" OR NUM-ENTRIES(cReturn,"|") < 8 THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Parameteroppsett mangler for å sende SMS","","").
    APPLY "close" TO THIS-PROCEDURE.
    RETURN.
  END.
  DO ix = 1 TO NUM-ENTRIES(cReturn,"|") BY 2:
    CASE ENTRY(ix,cReturn,"|"):
      WHEN "Mailhub"     THEN cMailServer  = ENTRY(ix + 1,cReturn,"|").
      WHEN "DoAuth"      THEN cAuthorize   = ENTRY(ix + 1,cReturn,"|").
      WHEN "AuthType"    THEN cAuthType    = ENTRY(ix + 1,cReturn,"|").
      WHEN "User"        THEN cMailUser    = ENTRY(ix + 1,cReturn,"|").
      WHEN "Password"    THEN cMailPwd     = ENTRY(ix + 1,cReturn,"|").
      WHEN "Mailprogram" THEN cMailProgram = ENTRY(ix + 1,cReturn,"|").
      WHEN "SMSDomain"   THEN cSmsDomain   = ENTRY(ix + 1,cReturn,"|").
      WHEN "SMSProvider" THEN cSmsProvider = ENTRY(ix + 1,cReturn,"|").
      WHEN "SMSReplyTo"  THEN cSmsReplyTo  = ENTRY(ix + 1,cReturn,"|").
    END CASE.
  END.
  IF cSMSReplyTo  = "" THEN cSMSReplyTo = cMailUser.

  ASSIGN icMobilnr               = REPLACE(icMobilnr,' ','')
         fiEmne:SCREEN-VALUE     = icSubject
         fiTil:SCREEN-VALUE      = REPLACE(icMobilnr," ","")
         fiSvarTil:SCREEN-VALUE  = ENTRY(1,cSMSReplyTo,"@")
         fiProvider:SCREEN-VALUE = "@" + cSMSProvider
         fiDomain:SCREEN-VALUE   = "@" + cSmsDomain
         edSMS:SCREEN-VALUE      = icMessage
         tbVisLogg:HIDDEN        = TRUE
         .
  APPLY "value-changed" TO edSMS.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop SMSWin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendSMS SMSWin 
PROCEDURE sendSMS :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE cReturn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCommandstring AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLogfil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSMSbodyfil AS CHARACTER   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  
  IF JBoxServerAPI:Instance:CallServerProc("Kodrehode_sendMail.p",
                                            STRING(lKOrdre_Id) + '|' + 
                                            TRIM(fiEmne:SCREEN-VALUE) + '|' +
                                            TRIM(edSMS:SCREEN-VALUE) + '|' +
                                            TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE,
                                            ?) THEN
    JBoxSession:Instance:ViewMessage("SMS sendt til kunde på mobil " + TRIM(fiTil:SCREEN-VALUE) + ".").
  ELSE 
    JBoxSession:Instance:ViewMessage("Feil ved sending av SMS. Se loggfil."). 
     
  APPLY "close" TO THIS-PROCEDURE.
  
/*  IF cMailProgram = "SN_SMS" THEN                                                                             */
/*  DO:                                                                                                         */
/*    cLogfil = "log\sendsms_" + JBoxSession:Instance:UserId + ".log".                                          */
/*    OUTPUT TO VALUE(cLogfil).                                                                                 */
/*    OUTPUT CLOSE.                                                                                             */
/*    FILE-INFO:FILE-NAME = SEARCH(cLogfil).                                                                    */
/*    cLogfil = FILE-INFO:FULL-PATHNAME.                                                                        */
/*                                                                                                              */
/*    cSMSbodyfil = "log\smsbody_" + JBoxSession:Instance:UserId + ".log".                                      */
/*    OUTPUT TO VALUE(cSMSbodyfil).                                                                             */
/*    OUTPUT CLOSE.                                                                                             */
/*    FILE-INFO:FILE-NAME = SEARCH(cSMSbodyfil).                                                                */
/*    cSMSbodyfil = FILE-INFO:FULL-PATHNAME.                                                                    */
/*                                                                                                              */
/*    MESSAGE 'test-1' SKIP                                                                                     */
/*    'cSMSbodyfil:' cSMSbodyfil SKIP                                                                           */
/*    'cLogfil:' cLogfil                                                                                        */
/*    VIEW-AS ALERT-BOX.                                                                                        */
/*                                                                                                              */
/*    OUTPUT TO VALUE(cSMSbodyfil).                                                                             */
/*    PUT UNFORMATTED (IF TRIM(edSMS:SCREEN-VALUE) NE ? THEN TRIM(edSMS:SCREEN-VALUE) ELSE " ") SKIP.           */
/*    OUTPUT CLOSE.                                                                                             */
/*/*     cCommandstring = "c:\polygon\prs\cmd\sendEmail.exe -q"   + ' '  + */                                   */
/*    cCommandstring = "cmd\sendEmail.exe -q"   + ' '  +                                                        */
/*                     "-f support@polygon.se -t"        + ' "'  +                                              */
/*                     TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE     + '" '  +                         */
/*                     "-s smtp.office365.com:587" + ' '  +                                                     */
/*                     "-xu support@polygon.se"        + ' '  +                                                 */
/*                     "-xp Tenn1s39 -u"              + ' "' +                                                  */
/*                     (IF fiEmne:SCREEN-VALUE NE ? THEN REPLACE(fiEmne:SCREEN-VALUE,'"'," ") ELSE " ")  +      */
/*                     '"  -o message-file=' + cSMSbodyfil + ' -l ' + cLogfil.                                  */
/*                                                                                                              */
/*/* MESSAGE cCommandstring                 */                                                                  */
/*/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */                                                                  */
/*/* OUTPUT TO "c:\tmp\smsmail.bat".      */                                                                    */
/*/* PUT UNFORMATTED cCommandstring SKIP. */                                                                    */
/*/* OUTPUT CLOSE.                        */                                                                    */
/*                                                                                                              */
/*      OS-COMMAND SILENT VALUE(cCommandstring).                                                                */
/*      INPUT FROM VALUE(cLogfil).                                                                              */
/*      IMPORT UNFORMATTED cc.                                                                                  */
/*      INPUT CLOSE.                                                                                            */
/*      IF cc MATCHES "*successfully*" THEN                                                                     */
/*          bOk = TRUE.                                                                                         */
/*      IF bOk THEN cReturn = "SMS".                                                                            */
/*      IF tbVisLogg:CHECKED THEN DO:                                                                           */
/*        IF SEARCH(cLogfil) NE ? THEN DO:                                                                      */
/*          FILE-INFO:FILE-NAME = SEARCH(cLogfil).                                                              */
/*          OS-COMMAND NO-WAIT notepad VALUE(FILE-INFO:FULL-PATHNAME).                                          */
/*        END.                                                                                                  */
/*      END.                                                                                                    */
/*  END.                                                                                                        */
/*  ELSE IF cMailProgram = "blat" THEN DO:                                                                      */
/*    RUN JBoxSendBlatMail.p ("To;" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE,                       */
/*                            TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE,                             */
/*                            "",                                                                               */
/*                            IF fiEmne:SCREEN-VALUE NE ? THEN fiEmne:SCREEN-VALUE ELSE "",                     */
/*                            TRIM(edSMS:SCREEN-VALUE),                                                         */
/*                            "",                                                                               */
/*                            cMailServer,                                                                      */
/*                            cMailUser,                                                                        */
/*                            cMailPwd,                                                                         */
/*                            "",                                                                               */
/*                            "",                                                                               */
/*                            "",                                                                               */
/*                            "",                                                                               */
/*                            NO,                                                                               */
/*                            tbVisLogg:CHECKED, /* Vis logg */                                                 */
/*                            ?,                                                                                */
/*                            OUTPUT bOk,                                                                       */
/*                            OUTPUT cReturn                                                                    */
/*                            ).                                                                                */
/*    IF NOT bOk THEN                                                                                           */
/*      DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").                                                        */
/*  END.                                                                                                        */
/*  ELSE DO:                                                                                                    */
/*/*     bOK = DYNAMIC-FUNCTION("runProc","jbserv_call_smtpmail.p",                          */                 */
/*/*                         cMailServer                                                     */                 */
/*/*                 + "|" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE    /* Til */ */                 */
/*/*                 + "|" + TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE  /* Fra */ */                 */
/*/*                       + "|||||" /* |CC|BCC|Attach|localfiles| */                        */                 */
/*/*                       + fiEmne:SCREEN-VALUE                                             */                 */
/*/*                 + "|" + edSMS:SCREEN-VALUE                                              */                 */
/*/*                       + "||||" /* |cont.type|bodytype|importance| */                    */                 */
/*/*                       + (IF cAuthorize = "1" THEN "YES" ELSE "NO")                      */                 */
/*/*                 + "|" + cAuthType                                                       */                 */
/*/*                 + "|" + cMailUser                                                       */                 */
/*/*                 + "|" + cMailPwd                                                        */                 */
/*/*                 + "|" + cMailProgram                                                    */                 */
/*/*                        ,?).                                                             */                 */
/*                                                                                                              */
/*      RUN prssmtpmailv5_7a.p (                                                                                */
/*        /*mailhub    */   cMailServer /* cSMTPserver */,                                                      */
/*        /*EmailTo    */   TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE /* cMailReceiver */,             */
/*        /*EmailFrom  */   TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE /* cMailSender */,             */
/*        /*EmailCC    */   '',                                                                                 */
/*        /*Attachments*/   "",                                                                                 */
/*        /*LocalFiles */   "",                                                                                 */
/*        /*Subject    */   fiEmne:SCREEN-VALUE /* cMailSubject */,                                             */
/*        /*Body       */   edSMS:SCREEN-VALUE  /* cMailBody    */,                                             */
/*        /*MIMEHeader */   "" /* cMailContentType */,                                                          */
/*        /*BodyType   */   "",                                                                                 */
/*        /*Importance */   0,                                                                                  */
/*        /*L_DoAUTH   */   FALSE,                                                                              */
/*        /*C_AuthType */   "" /* cMailAuthType */,                                                             */
/*        /*C_User     */   cMailUser,                                                                          */
/*        /*C_Password */   cMailPwd,                                                                           */
/*        /*oSuccessful*/  OUTPUT bOk,                                                                          */
/*        /*vMessage   */  OUTPUT cReturn) NO-ERROR.                                                            */
/*                                                                                                              */
/*/*     RUN jbserv_call_smtpmail.p (                                                       */                  */
/*/*                        cMailServer                                                     */                  */
/*/*                + "|" + TRIM(fiTil:SCREEN-VALUE) + fiProvider:SCREEN-VALUE    /* Til */ */                  */
/*/*                + "|" + TRIM(fiSvarTil:SCREEN-VALUE) + fiDomain:SCREEN-VALUE  /* Fra */ */                  */
/*/*                      + "|||||" /* |CC|BCC|Attach|localfiles| */                        */                  */
/*/*                      + fiEmne:SCREEN-VALUE                                             */                  */
/*/*                + "|" + edSMS:SCREEN-VALUE                                              */                  */
/*/*                      + "||||" /* |cont.type|bodytype|importance| */                    */                  */
/*/*                      + (IF cAuthorize = "1" THEN "YES" ELSE "NO")                      */                  */
/*/*                + "|" + cAuthType                                                       */                  */
/*/*                + "|" + cMailUser                                                       */                  */
/*/*                + "|" + cMailPwd                                                        */                  */
/*/*                + "|" + cMailProgram                                                    */                  */
/*/*                       ,?                                                               */                  */
/*/*                       ,""                                                              */                  */
/*/*                       ,OUTPUT cReturn                                                  */                  */
/*/*                       ,OUTPUT bOk                                                      */                  */
/*/*                       ).                                                               */                  */
/*                                                                                                              */
/*    IF bOk THEN cReturn = REPLACE(cReturn,"Email","SMS").                                                     */
/*    DYNAMIC-FUNCTION("DoMessage",0,0,cReturn,"","").                                                          */
/*    IF tbVisLogg:CHECKED THEN DO:                                                                             */
/*      IF SEARCH("socketemail.log") NE ? THEN DO:                                                              */
/*        FILE-INFO:FILE-NAME = SEARCH("socketemail.log").                                                      */
/*        OS-COMMAND NO-WAIT notepad VALUE(FILE-INFO:FULL-PATHNAME).                                            */
/*      END.                                                                                                    */
/*      ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Finner ikke socketmail.log","","").                              */
/*    END.                                                                                                      */
/*  END.                                                                                                        */
/*END.                                                                                                          */
/*                                                                                                              */
/*IF bOk THEN DO:                                                                                               */
/*  ASSIGN obOk      = YES                                                                                      */
/*         ocMessage = "SMS " + STRING(TODAY) + ", " + STRING(TIME,"HH:MM") + " " + fiTil:SCREEN-VALUE + CHR(10)*/
/*                   + fiEmne:SCREEN-VALUE + CHR(10)                                                            */
/*                   + edSMS:SCREEN-VALUE                                                                       */
/*                   .                                                                                          */
/*/*   MESSAGE ocMessage                      */                                                                */
/*/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */                                                                */
/*  APPLY "close" TO THIS-PROCEDURE.                                                                            */

END. /* FRAME */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

