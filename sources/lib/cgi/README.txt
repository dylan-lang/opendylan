This is a get-it-out-the-door release of a HyperText Transport
Protocol (HTTP) Common Gateway Interface (CGI) library for the Dylan
programming language.  Dylan because of its power, expressiveness, and
abstraction capabilities is a great language for writing CGI's.

This library works with Gwydion Dylan (well at least with Mindy).  The
code is going to need some tweaking for Functional Developer (if it can be
made to run at all with Functional Developer).

To use cgi-lib, you need to compile it with d2c or mindy, and make the
compiled library (or a symbolic link to it) visible to d2c when you
are compiling your cgi.  Your two basic choices are putting the
compiled library in /lib/dylan or putting it in the directory where
you are compiling your cgi.

The main functionality of cgi-lib is to map HTML form variables to
method keywords.  To use cgi-lib, you need to define a method
cgi-main.  Then in cgi-main, you need to place a call to
dispatch-cgi-method with the possible functions that may be called.
The function that is called when the cgi is executed is determined
from a post method (form) input of the name "function".

define method cgi-main ()
  format-out("Content-type: text/html\r\n\r\n");
  format-out("<HTML><BODY>");
  dispatch-cgi-function(tweedle-dee, tweedle-dum); 
  format-out("</BODY></HTML>");
end method;

Please see /examples/minimal/ for a minimal example.

This code is free for non-commercial use.

Peter Hinely <phinely@hawaii.edu>

--

A couple of additional notes regarding the Functional Developer version:

o Because Functional Developer doesn't handle "main" the same was as Gwydion
  Dylan does, you have to add an explicit call to main() at the end of
  your application in order for it to do anything.

o In Functional Developer, dispatch-cgi-function is a macro. Functional Developer
  does not store a function's name in an accessible way, so the macro
  registers the cgi functions under the names by which they are referenced 
  in the call to dispatch-cgi-function. 
