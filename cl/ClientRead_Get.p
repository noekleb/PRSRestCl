
/*------------------------------------------------------------------------
    File        : Client_read.p
    Purpose     : 

    Syntax      : Gjør en READ mot et endpoint.

    Description : 

    Author(s)   : 
    Created     : Mon Jan 29 20:41:06 CET 2018 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING Progress.Json.ObjectModel.JsonObject.

DEFINE VARIABLE oClient AS IHttpClient NO-UNDO.
DEFINE VARIABLE oURI AS URI NO-UNDO.
DEFINE VARIABLE oCredentials AS Credentials NO-UNDO.
DEFINE VARIABLE oRequest AS IHttpRequest NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE iNumCookies AS INTEGER NO-UNDO.
DEFINE VARIABLE oCookies AS Cookie EXTENT NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rclStandardFunksjoner AS cls.StdFunk.clStandardFunksjoner NO-UNDO.

/* *****  Main Block ***** */
rclStandardFunksjoner  = NEW cls.StdFunk.clStandardFunksjoner() NO-ERROR.

/* Creating a Client. */
oClient = ClientBuilder:Build():KeepCookies(CookieJarBuilder:Build():CookieJar):Client.

ASSIGN
    cLogg = 'ClientRead_Get' + REPLACE(STRING(TODAY),'/','') 
    .

/* Creating a URI */
/*oURI = NEW URI('http', 'localhost',8080).*/
oURI = NEW URI('http', '193.214.52.6',8810). /* For PROD */
oURI:Path = "/PRSRest/rest/LokasjonsListeService/LokasjonsListe?filter=%7B%22ablFilter%22:%22TelleNr%20=%20%2728779%27%22%7D".
/*oURI:Path = "/PRSRest/rest/LokasjonsListeService/LokasjonsListe".*/


/*/* Creating a request. */                                     */
/*oCredentials = NEW Credentials('RESTDomain','CarkW', 'carkw').*/
/*oRequest = RequestBuilder:Build('GET', oURI)                  */
/*               :UsingBasicAuthentication(oCredentials)        */
/*               :AcceptJson()                                  */
/*               :Request.                                      */
               
/* Creating a request. */
/* Creating a request. */
oCredentials = NEW Credentials('Tomcat Manager Application','tomcat', 'tomcat').
oRequest = RequestBuilder:Build('GET', oURI)
               :usingBasicAuthentication(oCredentials) 
               :AcceptJson()
               :Request.
               
oResponse = ResponseBuilder:Build():Response.

/* Execute the request. */
oClient:Execute(oRequest, oResponse).     


/* Processing the responce */
IF oResponse:StatusCode <> 200 THEN
            DO: 
                MESSAGE
                    oResponse:StatusCode SKIP 
                    oResponse:StatusReason SKIP
                VIEW-AS ALERT-BOX.
                RETURN ERROR 'Request error: ' + string(oResponse:StatusCode).
            END.
            ELSE                               
                CAST(oResponse:Entity, JsonObject):WriteFile('konv\responseRead' + 
                                                              REPLACE(STRING(TODAY),'/','') +
                                                              '_' + 
                                                              REPLACE(STRING(TIME,"HH:MM:SS"),':','') + 
                                                              '.json', TRUE).               
               
/* Checking Cookies */               
iNumCookies = oResponse:GetCookies(OUTPUT oCookies).

/* Writing Cookies */                  
OUTPUT TO cookies.txt.                
    PUT iNumCookies.
    
    DEF VAR i AS INT NO-UNDO.             
    DO i = 1 TO iNumCookies:              
        PUT UNFORMATTED STRING(oCookies[i]).
        PUT UNFORMATTED SKIP.               
    END.                                  
OUTPUT CLOSE.
            
MESSAGE
    oResponse:StatusCode SKIP 
    oResponse:StatusReason SKIP
VIEW-AS ALERT-BOX.

CATCH e1 AS Progress.Lang.AppError:
    DO ix = 1 TO e1:NumMessages:
        rclStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ' + e1:GetMessage(ix) 
            ).    
    END.

    IF e1:ReturnValue > "" THEN
        rclStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Returverdi: ' + e1:ReturnValue 
            ).    
END CATCH.
CATCH e2 AS Progress.Lang.Error:
    DO ix = 1 TO e2:NumMessages:
        rclStandardFunksjoner:SkrivTilLogg(cLogg,
            '  ' + e2:GetMessage(ix) 
            ).    
    END.
END CATCH.

            