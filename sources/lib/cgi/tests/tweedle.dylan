module: cgi-tweedle
synopsis: 
author: 
copyright: 

define sideways method cgi-main ()
  format-out("Content-type: text/html\r\n\r\n");
  format-out("<HTML><HEAD><TITLE>Thank You!</TITLE></HEAD><BODY>");
  dispatch-cgi-function(tweedle-dee, tweedle-dum); 
  format-out("</BODY></HTML>");
end method;

define function tweedle-dee ()
  format-out("Tweedle Dee appreciates your support.");
end function;

define function tweedle-dum ()
  format-out("Tweedle Dum appreciates your support.");
end function;

define function default-response ()
  format-out("Please choose one of the birds.");
end function;

main();

