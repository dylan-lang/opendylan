Module:   dfmc-modeling
Author:   Greg Sullivan
Synopsis: support for additional api in preparation for integrating new typist 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// <CALL-SITE-SUMMARY>

define abstract class <call-site-summary> (<object>)
end class <call-site-summary>;

define class <default-call-site-summary> (<call-site-summary>)
end;

define variable *the-default-call-site-summary* :: <default-call-site-summary> 
  = make(<default-call-site-summary>);

define sealed inline method make (class == <call-site-summary>, #rest keys,
                                  #key lambda, arg-types)
    => (css :: <call-site-summary>)
  *the-default-call-site-summary*
end method make;

define method print-object (css :: <call-site-summary>, s :: <stream>) => ()
end;

define method print-object (css :: <default-call-site-summary>, s :: <stream>) => ()
  format(s, "<default-css>")
end;

define thread variable *current-css* = #f;

define macro with-current-css
  { with-current-css (?:expression) ?:body end }
    => { dynamic-bind (*current-css* = ?expression)
           ?body
         end }
end macro;

define inline function current-css () => (css :: <call-site-summary>);
  *current-css*;
end;

define generic get-default-call-site-summary (m :: <&method>)
  => (r :: false-or(<call-site-summary>));

// (gts,98jul2) In the code I got from Kevin, get-default-call-site-summary 
// was specialized on <&accessor-method> and <&lambda>, which seems reasonable. 
// However, it seems there are instances of (just) <&method>, so I think 
// the default behavior has to be on <&method> instead of <&lambda>.

define method get-default-call-site-summary (lambda :: <&method>)
  => (r :: false-or(<call-site-summary>))
  gts-debug("css", "in get-default-call-site-summary on <&method>, returning %=.\n",
                   *the-default-call-site-summary*);
  *the-default-call-site-summary*
end method;

define method get-default-call-site-summary (a :: <&accessor-method>)
  => (r :: false-or(<call-site-summary>))
  gts-debug("css", "in get-default-call-site-summary on <&accessor-method>, returning #f.\n");
  #f
end;

// dummy <call-site-summary-table>s will have either 0 or 1 
// element(s) in them

define class <call-site-summary-table> (<table>)
end class;

define constant call-site-summary-key-test = \==;

define function call-site-summary-key-hash (key, hash-state)
     => (id :: <integer>, state)
  values(0, hash-state);
end function;

define method table-protocol (t :: <call-site-summary-table>)
  => (test :: <function>, hash :: <function>);
  values(call-site-summary-key-test, call-site-summary-key-hash);
end method;
