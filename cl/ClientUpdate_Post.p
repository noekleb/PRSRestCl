
/*------------------------------------------------------------------------
    File        : Client1.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jan 29 20:41:06 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.*.
USING OpenEdge.Net.URI.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.ObjectModelParser.
USING Progress.Json.ObjectModel.JsonArray.

DEFINE VARIABLE httpUrl AS CHARACTER NO-UNDO.
DEFINE VARIABLE oCredentials AS Credentials NO-UNDO.
DEFINE VARIABLE oClient AS IHttpClient NO-UNDO.
DEFINE VARIABLE oRequest AS IHttpRequest NO-UNDO.
DEFINE VARIABLE oResponse AS IHttpResponse NO-UNDO.
DEFINE VARIABLE oRequestBody AS String NO-UNDO.
DEFINE VARIABLE oURI AS URI NO-UNDO.

DEFINE VARIABLE oJson AS JsonObject NO-UNDO.
DEFINE VARIABLE oParser     AS ObjectModelParser NO-UNDO.
DEFINE VAR lcJSon AS LONGCHAR NO-UNDO.

DEFINE VARIABLE iLength     AS INTEGER           NO-UNDO.
DEFINE VARIABLE iCount      AS INTEGER           NO-UNDO.
DEFINE VARIABLE pbOk AS LOG NO-UNDO.

/* ----------- temp table ------------------- */
DEFINE TEMP-TABLE ttprod SERIALIZE-NAME 'Prod' BEFORE-TABLE bttprod  
FIELD id            AS CHARACTER
FIELD seq           AS INTEGER      INITIAL ?
FIELD prodnr AS CHARACTER LABEL "Prod.nr"
FIELD besk AS CHARACTER LABEL "Beskrivelse"
FIELD sperret AS INTEGER INITIAL "0" LABEL "Sperret fra"
FIELD koststed AS CHARACTER LABEL "Kostnadssted"
FIELD Lokasjon AS CHARACTER LABEL "Lokasjon"
FIELD Company AS CHARACTER LABEL "Company"
FIELD StartPeriode AS INTEGER FORMAT ">>>>>9" LABEL "StartPeriode"
FIELD SluttPeriode AS INTEGER FORMAT ">>>>>9" LABEL "StartPeriode"
FIELD NRKId AS CHARACTER LABEL "NRKId"
/*
FIELD opdat AS DATE LABEL "Sist oppdatert"
FIELD gfra AS INTEGER INITIAL "0" LABEL "Gyldig fra"
FIELD KostAnsNr AS INTEGER LABEL "Ansattnummer"
FIELD KostEMail AS CHARACTER LABEL "EPost"
FIELD kommentar AS CHARACTER LABEL "Kommentar"
FIELD DatoTidOpprettet AS DATETIME LABEL "Opprettet"
FIELD DatoTidEndret AS DATETIME LABEL "Endret"
INDEX idx_Endret  DatoTidEndret  ASCENDING 
INDEX idx_Opprettet  DatoTidOpprettet  ASCENDING 
INDEX opdat  opdat  ASCENDING 
*/
INDEX Besk  besk  ASCENDING 
INDEX seq IS PRIMARY UNIQUE seq
INDEX prodnr IS  UNIQUE  prodnr  ASCENDING . 

DEFINE DATASET dsprod  SERIALIZE-NAME 'dsProd' FOR ttprod.

/* ----------- temp table ------------------- */

SESSION:DEBUG-ALERT = TRUE.

{"prg\ttLokasjonsliste.i"}
{"prg\dslokasjonsliste.i"}

/* Creating a Client. */
oClient = ClientBuilder:Build():KeepCookies(CookieJarBuilder:Build():CookieJar):Client.

/* Creating a URI */
/*oURI = NEW URI('http', 'localhost',8080).*/
oURI = NEW URI('http', '193.214.52.6',8810). /* For PROD */
oURI:Path = '/PRSRest/rest/LokasjonsListeService/LokasjonsListe/'.

/* Creating a request. */
oCredentials = NEW Credentials('Tomcat Manager Application','tomcat', 'tomcat').
/*oCredentials = NEW Credentials('RESTDomain','cark', 'cark').*/

/* Bygger JSon objectet */
oJson = NEW JsonObject().

/* Laster JSon object... */
DATASET dsTelleLinje:READ-JSON('file', 'konv\Lokasjonsliste.JSon', 'EMPTY').

/* Vise hva som ble lest inn */
pbOk = DATASET dsTelleLinje:WRITE-JSON ("file", 'konv\updateTelleLinje.JSon',TRUE).

/* Klargjør data til overføring. */
DATASET dsTelleLinje:WRITE-JSON ("longchar", lcJSon, YES).  

oParser    = NEW ObjectModelParser().                    
oJSon = CAST(oParser:Parse(lcJSon),JsonObject).

oRequest = RequestBuilder:put(oURI, oJson)
                :usingBasicAuthentication(oCredentials) 
                :AcceptJson() /* we want to get JSON back */ 
                :Request.

oResponse = oClient:Execute(oRequest).
MESSAGE
    oResponse:StatusCode SKIP 
    oResponse:StatusReason SKIP
VIEW-AS ALERT-BOX.

/* **********************  Internal Procedures  *********************** */
