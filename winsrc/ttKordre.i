
/*------------------------------------------------------------------------
    File        : ttKordre.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Tue Feb 05 13:15:43 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE {&New} {&Shared} TEMP-TABLE ttKORdreHode 
    FIELD KOrdre_Id AS DEC FORMAT ">>>>>>>>>>>>>9"
    FIELD EkstOrdreNr AS CHARACTER 
    FIELD DatotidOpprettet AS DATETIME 
    FIELD Navn AS CHARACTER 
    FIELD Manko AS LOG
    INDEX HodeMako KOrdre_Id Manko.

DEFINE {&New} {&Shared} TEMP-TABLE ttKOrdreLinje
    FIELD ButNr AS INT FORMAT ">>>>>9"
    FIELD KOrdre_Id AS DEC FORMAT ">>>>>>>>>>>>>9"
    FIELD KOrdreLinjeNr AS INTEGER 
    FIELD EkstOrdreNr AS CHARACTER 
    FIELD Manko AS LOG
    FIELD ArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Beskr AS CHARACTER 
    FIELD LevKod AS CHARACTER 
    FIELD LevFargKod AS CHARACTER 
    FIELD StrKode AS INT FORMAT ">>>>>9"
    FIELD Storl AS CHARACTER 
    FIELD Antall AS DECIMAL FORMAT "->>>,>>9"
    INDEX OppSlag KOrdre_Id Manko
    INDEX LinjeManko KOrdre_Id KOrdreLinjeNr ArtikkelNr ButNr StrKode
    .

DEFINE {&New} {&Shared} TEMP-TABLE ttArtBas
    FIELD ArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Beskr AS CHARACTER 
    FIELD LevKod AS CHARACTER 
    FIELD LevFargKod AS CHARACTER 
    FIELD ButNr AS INT FORMAT ">>>>>9"
    FIELD StrKode AS INT FORMAT ">>>>>9"
    FIELD Storl AS CHARACTER 
    FIELD Lagant AS DECIMAL FORMAT "->>>,>>9"
    FIELD Bestant AS DECIMAL FORMAT "->>>,>>9"
    FIELD Diff AS DECIMAL FORMAT "->>>,>>9"
    INDEX Lager ArtikkelNr ButNr StrKode.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
