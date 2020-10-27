/* fix-opprett_lokasjonsliste_JSon.p */
DEF VAR cFil AS CHAR NO-UNDO.
                          
{"prg\ttLokasjonsliste.i"}
{"prg\dslokasjonsliste.i"}

ASSIGN
    cFil = 'konv\Lokasjonsliste.JSon'
    .
    
    
/* Lagt til kommentar her. */
FOR EACH TelleLinje NO-LOCK WHERE 
    TelleLinje.TelleNr = 3:

    CREATE ttTelleLinje.
    BUFFER-COPY TelleLinje TO ttTelleLinje.
    /*
    ASSIGN 
        ttTelleLinje.TelleNr 
        ttTelleLinje.LinjeNr 
        ttTelleLinje.Butik 
        ttTelleLinje.Kode 
        ttTelleLinje.ArtikkelNr
        ttTelleLinje.Storl 
        ttTelleLinje.AntallTalt
        ttTelleLinje.VVareKost 
        ttTelleLinje.Merknad 
        ttTelleLinje.Oppdatert
        ttTelleLinje.Beskr 
        ttTelleLinje.SeqNr 
        ttTelleLinje.BrukerID
        ttTelleLinje.RegistrertDato 
        ttTelleLinje.RegistrertTid
        ttTelleLinje.RegistrertAv 
        .
    */
END.

DATASET dsTelleLinje:WRITE-JSON('file', cFil, TRUE).

