module:    harp-utils
Synopsis:  Macros which might be useful to all Dylaners
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////// All the macros in this file are potentially useful for all Dylan 
////// applications.


//// A handly macro for defining a name alias, along with it's setter
//// counterpart.

define macro alias-with-setter-definer
  { define alias-with-setter ?:name = ?alias:name }
    => { define constant ?name = ?alias;
         define constant ?name ## "-setter" = ?alias ## "-setter" }
end macro;


//// Increment and decrement macros 


define macro inc!
 { inc! (?place:expression) }
   => { ?place := ?place + 1 }
 { inc! (?place:expression, ?amount:expression) }
   => { ?place := ?place + ?amount }
end macro;


define macro dec!
 { dec! (?place:expression) }
   => { ?place := ?place - 1 }
 { dec! (?place:expression, ?amount:expression) }
   => { ?place := ?place - ?amount }
end macro;


//// Pushes and pops

define macro pushnew!
  { pushnew! (?new-val:expression, ?vector-place:expression) }
    => { ?vector-place := add-new!(?vector-place, ?new-val) }
end macro;


define macro push!
  { push! (?new-val:expression, ?vector-place:expression) }
    => { ?vector-place := add!(?vector-place, ?new-val) }
end macro;



define macro list-pop!
  { list-pop! (?loc:expression) }
    => { begin
           let $list$ = ?loc;
           ?loc := $list$.tail;
           $list$.head
         end }
end macro;

define macro stretchy-vector-pop!
  { stretchy-vector-pop! (?loc:expression) }
    => { begin
           let $vec$ = ?loc;
           let $size$ = $vec$.size - 1;
           let $last$ = $vec$[$size$];
           $vec$.size := $size$;
           $last$
         end }
end macro;
