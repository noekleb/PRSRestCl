
/*------------------------------------------------------------------------
    File        : clientDinTero.i
    Purpose     : 

    Syntax      :

    Description : Temp tabeller som benyttes mot DinTero APIet.

    Author(s)   : tomn
    Created     : Sat Oct 10 17:15:29 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEFINE TEMP-TABLE ttoJSon NO-UNDO 
        FIELD grant_type AS CHARACTER
        FIELD audience   AS CHARACTER.

    DEF TEMP-TABLE ttUrl
        FIELD return_url AS CHAR 
        .
    
    DEF TEMP-TABLE ttOrder
        FIELD amount AS DEC 
        FIELD currency AS CHAR 
        FIELD merchant_reference AS CHAR 
        .
    
    DEF TEMP-TABLE ttItems
        FIELD id AS CHAR 
        FIELD line_id AS CHAR  
        FIELD description AS CHAR 
        FIELD quantity AS INT 
        FIELD amount AS DEC  
        FIELD vat_amount AS DEC 
        FIELD vat AS DEC 
        .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
