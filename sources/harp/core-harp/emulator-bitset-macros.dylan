module:    harp-utils
Synopsis:  Macros for manipulating bitsets.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define macro do-bit-set
  { do-bit-set (?index:name :: ?i-type:expression in ?bit-set:expression)
      ?:body
    end }
    => { let $bit-set$ = ?bit-set;
         for ($word$ :: <integer> from 0 below size($bit-set$),
              $word-index$ :: <integer> from 0 by $int-size$)
           let $this-word$ :: <integer> = $bit-set$[$word$];
           for ($bit$ :: <integer> from 0 below $int-size$)
             unless (logand($this-word$, ash(1, $bit$)) == 0)
               let ?index :: ?i-type = $word-index$ + $bit$;
               ?body;
             end unless;
           end for;
         end for }

i-type:
  { <object> } => { <integer> }
  { ?:expression } => { ?expression }

end macro;
