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
    => { let $bit-set$ :: <vector-32bit> = ?bit-set;
         for (?=$word$ :: <integer> from 0 below size($bit-set$),
              $word-index$ :: <integer> from 0 by $int-size$)
           let $this-word$ :: <machine-word> =
	       element-no-bounds-check($bit-set$, ?=$word$);
	   unless (machine-word-equal?
		     ($this-word$,
		      coerce-integer-to-machine-word(0)))
           for ($bit$ :: <integer> from 0 below $int-size$)
	     let ?=$bit-mask$ :: <machine-word> =
	       machine-word-unsigned-shift-left
	       (coerce-integer-to-machine-word(1), $bit$);
             unless (machine-word-equal?
		       (machine-word-logand
			  ($this-word$, ?=$bit-mask$),
			coerce-integer-to-machine-word(0)))
               let ?index :: ?i-type = $word-index$ + $bit$;
               ?body;
             end unless;
           end for;
	   end unless;
         end for }

i-type:
  { <object> } => { <integer> }
  { ?:expression } => { ?expression }

end macro;
