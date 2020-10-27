CREATE SERVER hServer.
IF NUM-ENTRIES(SESSION:PARAM,";") > 1 AND ENTRY(2,SESSION:PARAM,";") NE "" THEN
  hServer:CONNECT(ENTRY(2,SESSION:PARAM,";")) NO-ERROR.
ELSE
/*  hServer:CONNECT("-URL http://aia.appfarm.no/aia/Aia?AppService=Appfarm") NO-ERROR. */
  hServer:CONNECT("-H localhost -S 3195 -AppService asappfarm -DirectConnect") NO-ERROR. 

IF hServer:CLIENT-CONNECTION-ID = "" THEN
  RETURN FALSE.
ELSE
  RETURN TRUE.
