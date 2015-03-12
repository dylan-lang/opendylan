Module: dfmc-c-back-end
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $initial-string-stream-contents-size = 10000;

define class <c-back-end> (<back-end>)
  constant slot lambda-stream
    = make(<string-stream>, direction: #"output",
           contents: make(<byte-string>,
                          size: $initial-string-stream-contents-size));
end;

register-back-end(<c-back-end>, #"c", #f);

define method initialize (back-end :: <c-back-end>, #key, #all-keys) => ()
  next-method();
  stream-contents(back-end.lambda-stream, clear-contents?: #t);
end method;

define method back-end-record-repeated-object-sizes?
    (back-end :: <c-back-end>) => (well? :: <boolean>)
  #t
end method;

define constant $xep-string             = "xep";
define constant $rest-xep-string        = "rest_xep";
define constant $rest-key-xep-string    = "rest_key_xep";
define constant $gf-xep-string          = "gf_xep";
define constant $key-mep-string         = "key_mep";
define constant $gf-optional-xep-string = "gf_optional_xep";

define constant $dylan-type-string = "dylan_value";

define constant $initialize-closure-string
  = "INIT_CLOSURE";
define constant $make-closure-string
  = "MAKE_CLOSURE";
define constant $make-closure-with-signature-string
  = "MAKE_CLOSURE_SIG";
define constant $make-closure-initd-string
  = "MAKE_CLOSURE_INITD";
define constant $make-closure-initd-with-signature-string
  = "MAKE_CLOSURE_INITD_SIG";
define constant $make-method-with-signature-string
  = "MAKE_METHOD_SIG";
define constant $set-method-signature-string
  = "SET_METHOD_SIG";

define constant $initialize-keyword-closure-string
  = "INIT_KEYWORD_CLOSURE";
define constant $make-keyword-closure-string
  = "MAKE_KEYWORD_CLOSURE";
define constant $make-keyword-closure-with-signature-string
  = "MAKE_KEYWORD_CLOSURE_SIG";
define constant $make-keyword-closure-initd-string
  = "MAKE_KEYWORD_CLOSURE_INITD";
define constant $make-keyword-closure-initd-with-signature-string
  = "MAKE_KEYWORD_CLOSURE_INITD_SIG";
define constant $make-keyword-method-with-signature-string
  = "MAKE_KEYWORD_METHOD_SIG";
define constant $set-keyword-method-signature-string
  = "SET_KEYWORD_METHOD_SIG";

// define constant $box-string
//   = "BOX";
define constant $closure-reference-string
  = "CREF";
define constant $method-reference-string
  = "MREF";
define constant $function-register-string
  = "get_teb()->function";
define constant $capture-environment-string
  = "CAPTURE_ENVIRONMENT";
define constant $capture-keyword-environment-string
  = "CAPTURE_KEYWORD_ENVIRONMENT";
// define constant $closure-string
//   = "CLOSURE";

define class <multiple-value-temporary-reference> (<object>)
  constant slot ref-temp :: <multiple-value-temporary>, required-init-keyword: ref-temp:;
  constant slot ref-index :: <integer> = 0, init-keyword: ref-index:;
  constant slot lhs? :: <boolean> = #f, init-keyword: lhs?:;
end class;

define method mv-temp-ref(tmp :: <multiple-value-temporary>, i :: <integer>)
  make(<multiple-value-temporary-reference>, ref-temp: tmp, ref-index: i)
end method;

define method mv-temp-ref(tmp , i :: <integer>)
  tmp;
end method;

define method mv-temp-lhs(tmp :: <multiple-value-temporary>, i :: <integer>)
  make(<multiple-value-temporary-reference>, ref-temp: tmp, ref-index: i, lhs?: #t)
end method;

define method mv-temp-lhs(tmp, i :: <integer>)
  tmp;
end method;

define class <address-of-temporary> (<object>)
  constant slot addr-temporary, required-init-keyword: temporary:;
end class;

define method make-address-of(o)
  make(<address-of-temporary>, temporary: o);
end method;

define method emit-reference
    (b :: <c-back-end>, s :: <stream>,
     o :: <address-of-temporary>) => ()
  format(s, "&");
  emit-reference(b, s, o.addr-temporary);
end method;

define method back-end-word-size
    (object :: <c-back-end>) => (size :: <integer>)
  if (member?(target-architecture-name(), #(#"alpha", #"x86_64")))
    8
  else
    4
  end;
end method;

define function format-emit
    (b :: <c-back-end>, s :: <stream>,  d :: <integer>,
     format-string :: <byte-string>, #rest arguments)
  let i :: <integer> = 0;
  for (c in format-string)
    select (c)
      '~' =>       print-message(arguments[i], s); i := i + 1;
      '%' =>       emit-object(b, s, arguments[i]); i := i + 1;
      '@' =>       emit-reference(b, s, arguments[i]); i := i + 1;
      '?' =>       emit-indirect-reference(b, s, arguments[i]);
                   i := i + 1;
      '^' =>       emit-name(b, s, arguments[i]); i := i + 1;
      '#' =>       if (arg-used?(arguments[i]))
                     format-emit*(b, s, "@ = ", arguments[i]);
                   end if;
                   i := i + 1;
      '\t' =>      for (i from 0 below d) write(s, "  "); end;
      otherwise => write-element(s, c);
    end select;
  end for;
end function;

define method arg-used?(o :: <multiple-value-temporary-reference>)
  (o ~== #f) & o.ref-temp.used?
end method;

define method arg-used?(o)
  (o ~== #f) & o.used?
end method;

define inline function format-emit*
    (b :: <c-back-end>, s :: <stream>,
     format-string :: <byte-string>, #rest arguments)
  apply(format-emit, b, s, 1, format-string, arguments)
end function;

//// TOP-LEVEL

define method emit-all (back-end :: <c-back-end>, cr :: <compilation-record>,
                        #rest flags, #key dfm-output? = #f, #all-keys)
  with-simple-abort-retry-restart
      ("Abort the emission phase", "Restart the emission phase")
    /*
    // Hack!!! Displaced.
    register-object(unit, unit.top-level-lambda);
    // Some references are inserted by the back-end itself. These are they:
    register-object(unit, &false);
    register-object(unit, &true);
    // Descend the roots locating all internal and external objects.
    trace-objects(unit);
    */
    let heap = cr.compilation-record-model-heap;
    let literals = heap.heap-defined-object-sequence;
    when (dfm-output?)
      emit-all-dfm(back-end, cr, flags);
    end when;
    for (literal in literals)
      emit-code(back-end, literal);
    end for;
    with-labeling-from-dynamic
      for (code in heap.heap-root-system-init-code)
        emit-init-code(back-end, code.^iep);
      end for;
      for (code in heap.heap-root-init-code)
        emit-init-code(back-end, code.^iep);
      end for;
    end;
    retract-local-methods-in-heap(heap);
  end;
end method;

// Must retract local methods only after code generation of compilation-record

define method retract-local-methods-in-heap(heap) => ()
  if (*retract-dfm?*)
    for (literal in heap.heap-defined-object-sequence)
      if (instance?(literal, <&iep>) & ~lambda-top-level?(literal))
        // format-out?("\nRETRACTING %=\n", literal);
        retract-method-dfm(literal);
        retract-method-dfm(literal.function);
      end if;
    end for;
  end if;
end method;
