Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <type-estimate> = <&type>;
define constant <type-estimate-raw> = <&raw-type>;
define constant <type-estimate-class> = <&class>;
define constant <type-estimate-limited-instance> = <&singleton>;
define constant <type-estimate-union> = <&union>;
define constant <type-estimate-values> = <values-type>;
define constant <type-estimate-bottom> = <&bottom-type>;
define constant <type-estimate-top> = <&top-type>;

define constant ^type-estimate-class-of = ^base-type;
define constant type-estimate-class = identity;



define variable *constant-fold-in-typist* = #f;

define variable *infer-through-non-locals* = #f;


// following is while we are doing 'old typist' inference:
// not flowing through calls, not maintaining CSS's.

// ** see modeling/call-site-summary.dylan for temporary version of 
//    <call-site-summary-table>

define function call-site-summaries(l :: <&lambda>)
  => (res :: <call-site-summary-table>)
  let t = make(<call-site-summary-table>, size: 1);
  t[0] := get-default-call-site-summary(l);
  t
end function;

