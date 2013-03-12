Module: dfmc-java-back-end
Author: Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// names and types resolution.

// underneath <method> comes the typed functions
// functional arguments are always hoisted to <function>, since signature strings
// may not recurse.
// underneath the typed function classes (which are "dylan/#aux.meth#789" etc)
// come the actual implementations.
// For closures, the implementations have environment slots, and constructors.
// For non-closures, a single instance will be made and put in the binding.
// The top level method of a closure tree has a single instance in a binding.







define function java-name-mangle (thing)
  let result = local-mangle (*java-back-end*, as (<string>, thing).as-lowercase);
//  format-out ("java-mangled to %s\n", result);
  result
end;

/* not used?
define function best-java-name-for (n :: <named-object>, prefix :: <string>) => (str :: <string>)
  let  deb = n.^debug-name;
  if (deb)
    java-name-mangle (deb)
  else
    let  emitted = n.emitted-name;
    if (instance? (emitted, <integer>))
      format-to-string ("%s#%d", prefix, emitted)
    else
      emitted | format-to-string ("some_anonymous_%s", prefix)
    end
  end
end;
*/

// was function, but appears not to typecheck
/*  COMMENT OUT FOR NOW
define method java-lib-name (n :: <library-binding>) => (<byte-string>)
  let  library = n.home.home-library;
  let  name  = java-name-mangle (library.debug-name);
  let  result = "dylan";
  if (name ~= "dylan")
    result := concatenate (result, "/",
                           java-name-mangle (library.library-description-emit-name))
  end;
  result
end;
*/

define function home (thing) => (thing)
  error ("home needs to be exported");
end;

// HOME is undefined !!!!  ARSE
define method java-lib-name (n :: <module-binding> /*<canonical-module-binding>*/ ) => (str :: <string>)
format-out ("java-lib-name problem!!!  what is home?\n");
my-break (n);
  let  library = n.home.home-library;
  let  name  = java-name-mangle (library.debug-name);
  let  result = "dylan";
  if (name ~= "dylan")
    result := concatenate (result, "/",
                           java-name-mangle (library.debug-name))
  end;
  result
end;

define method java-lib-name (n :: <&function>) => (str :: <byte-string>)
  let  library = n.^function-signature.model-creator.form-compilation-record.compilation-record-library;
//format-out ("the library for %s is %s\n", n, library);
  let  foo = as (<string>, library.library-description-emit-name);
//format-out ("the debug name for library is %s\n", foo);
  let  name  = java-name-mangle (foo);
  let  result = "dylan";
  if (name ~= "dylan")
    result := concatenate (result, "/",
                           java-name-mangle (library.library-description-emit-name))
  end;
  result
end;


define sealed generic get_cr_for_thing (x) => (cr :: <compilation-record>);

define method get_cr_for_thing (x :: <compilation-record>) => (cr :: <compilation-record>)
  x
end;

define method get_cr_for_thing (x :: <class-definition>) => (cr :: <compilation-record>)
  x.form-compilation-record
end;

define method java-lib-name (n :: <&class>) => (str :: <byte-string>)
//format-out ("the model-creator for %s is %s\n", n, n.model-creator);
  let  creator = n.model-creator;
  let  cr = get_cr_for_thing (creator);
  let  library = cr.compilation-record-library;
//format-out ("the library for %s is %s\n", n, library);
  let  foo = as (<byte-string>, library.library-description-emit-name);
//format-out ("the debug name for library is %s\n", foo);
  let  name  = java-name-mangle (foo);
  let  result = "dylan";
  if (name ~= "dylan")
    result := concatenate (result, "/",
                           java-name-mangle (library.library-description-emit-name))
  end;
  result
end;

define function java-classname (cls :: <&class>) => (uniq :: <unique-string>)
//  format-out ("java-classname on %s\n", as (<byte-string>, cls.^debug-name));
  concatenate (java-lib-name (cls), "/",  java-name-mangle (cls.^debug-name)).uniq
end;

define function java-lang (cname :: <byte-string>) => (uniq :: <unique-string>)
  concatenate ("java/lang/", cname).uniq
end;
define function classsig (cname :: <unique-string>) => (uniq :: <unique-string>)
  concatenate ("L", cname.the-string, ";").uniq
end;
define function arraysig (cname :: <unique-string>) => (uniq :: <unique-string>)
  concatenate ("[", cname.the-string).uniq
end;



define function dylan-lang (str :: <string>) => (res :: <unique-string>)
  concatenate ("dylan/", java-name-mangle (str)).uniq
end;

define constant  $dylan-object-class-name$ = dylan-lang ("<object>");
define constant  $dylan-object-class-sig$ = classsig ($dylan-object-class-name$);

define constant  $dylan-class-class-name$ = dylan-lang ("<class>");
define constant  $dylan-class-class-sig$ = classsig ($dylan-class-class-name$);

define constant  $dylan-boolean-class-name$ = dylan-lang ("<boolean>");
define constant  $dylan-boolean-class-sig$ = classsig ($dylan-boolean-class-name$);

define constant  $dylan-small-integer-class-name$ = dylan-lang ("<integer>");
define constant  $dylan-small-integer-class-sig$ = classsig ($dylan-small-integer-class-name$);

define constant  $dylan-single-float-class-name$ = dylan-lang ("<single-float>");
define constant  $dylan-single-float-class-sig$ = classsig ($dylan-single-float-class-name$);

define constant  $dylan-double-float-class-name$ = dylan-lang ("<double-float>");
define constant  $dylan-double-float-class-sig$ = classsig ($dylan-double-float-class-name$);

define constant  $dylan-empty-list-class-name$ = dylan-lang ("<empty-list>");
define constant  $dylan-empty-list-class-sig$ = classsig ($dylan-empty-list-class-name$);

define constant  $dylan-pair-class-name$ = dylan-lang ("<pair>");
define constant  $dylan-pair-class-sig$ = classsig ($dylan-pair-class-name$);

define constant  $dylan-sov-class-name$ = dylan-lang ("<simple-object-vector>");
define constant  $dylan-sov-class-sig$ = classsig ($dylan-sov-class-name$);

define constant  $dylan-sequence-class-name$ = dylan-lang ("<sequence>");
define constant  $dylan-sequence-class-sig$ = classsig ($dylan-sequence-class-name$);

define constant  $dylan-function-class-name$ = dylan-lang ("<function>");
define constant  $dylan-function-class-sig$ = classsig ($dylan-function-class-name$);

define constant  $dylan-method-class-name$ = dylan-lang ("<method>");
define constant  $dylan-method-class-sig$ = classsig ($dylan-method-class-name$);

define constant  $dylan-generic-function-class-name$ = dylan-lang ("<generic-function>");
define constant  $dylan-generic-function-class-sig$ = classsig ($dylan-generic-function-class-name$);

define constant  $dylan-symbol-class-name$ = dylan-lang ("<symbol>");
define constant  $dylan-symbol-class-sig$ = classsig ($dylan-symbol-class-name$);

define constant  $dylanexception-class-name$ = dylan-lang ("dylancondition");
define constant  $dylanexception-class-sig$ = classsig ($dylanexception-class-name$);

define constant  $dylan-type-class-name$ = dylan-lang ("<type>");
define constant  $dylan-type-class-sig$ = classsig ($dylan-type-class-name$);

define constant  $dylan-thread-class-name$ = dylan-lang ("dylanthread");
define constant  $dylan-thread-class-sig$ = classsig ($dylan-thread-class-name$);

define constant  $dylan-simple-vector-sig$ = arraysig ($dylan-object-class-sig$);

define constant  $dylan-type-vector-sig$ = arraysig ($dylan-type-class-sig$);


// define constant  $java-iep-fixed-args$ = 4;  // not used
