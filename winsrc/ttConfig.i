
/*------------------------------------------------------------------------
    File        : ttConfig.i
    Purpose     : Få en enhetlig definisjon av temp tabellen.

    Syntax      :

    Description : Definerer ttConfig tabellen. Denne inneholder informasjon 
                  som kan settes i json konfigurasjonsfil. Denne leses ved 
                  oppstart av applikasjonen og overstyrer default verdiene.

    Author(s)   : Tom Nøkleby
    Created     : Fri Oct 09 09:33:02 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttConfig
  FIELD cHost AS CHARACTER 
  FIELD cPort AS CHARACTER 
  FIELD cAppServer AS CHARACTER
  FIELD iButNr AS INTEGER
  FIELD iKasseNr AS INTEGER  
  FIELD webBrukerid AS CHARACTER 
  FIELD webpassord AS CHARACTER
  FIELD DebugLevel AS INTEGER 
  FIELD DebugAktiv AS LOG
  FIELD DebugLogFile AS CHARACTER   
  .


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
