module:    native-rtg
Synopsis:  Support for registering top-level items for the runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// Registration support:-
/////
///// Top level forms for the runtime generator are all registered in a first
///// pass, and then all output in a second pass. The registration phase
///// involves defining a variable which will hold a constant-reference
///// for the top level form, as well as recording an outputting function
///// which will be invoked in the second pass.
/////
///// The registration mechanism is separated into 2 separate groups:
/////   1. literals / variables / external-constants
/////   2. functions, including simple primitives & entry point primitives
///// The second pass of each of these may be invoked independently of the others
/////
///// This files defines macros for using the registration mechanism, as 
///// well as some registration support functions.


///// DLL support
/////
///// There is a small amount of runtime support for each client DLL, and a large
///// amount of runtime support for the base (Dylan) DLL. The registration 
///// mechanism permits registration for each of these independently. The 
///// two resultant bodies of runtime code are referred to as the "client"
///// and "base" runtimes, respectively. By default, registrations are for the 
///// base runtime only, but the macros provide mechanism for registering
///// in the client as well as or instead of the base.



///// Macros for defining the various types of top level forms



//// Literals variables & external constants:


/// Defining literals within the runtime:- 

define macro runtime-literal-definer
  { define ?lit-type runtime-literal ?:name 
      = ?string:expression, #key ?data:expression, ?section:expression = #"untraced-data"}
    => { define variable ?name = #f;
         register-constant(method (be :: <harp-back-end>, outputter) 
                             let ref = constant-mangled-ref(be, ?string);
                             ?name := ref;
                             ?lit-type(be, outputter, ref, ?data, ?section);
                           end method,
                           ?#"name") }
lit-type:
  { } 
    => { output-literal }
  { raw } 
    => { output-raw-literal }
  { byte } 
    => { output-raw-byte-literal }
end macro;


// Literal dumping support

define method output-literal (be :: <harp-back-end>, 
                              outputter, 
                              ref :: <constant-reference>,
                              lit :: <byte-string>,
                              given-section :: <symbol>)
  let section = if (given-section == #"untraced-data")
                  #"untraced-objects"; // no reason to have a string in untraced-data
                else given-section
                end if;
  output-definition(be, outputter, ref, section: section);
  output-data-item(be, outputter, dylan-byte-string-class);
  output-data-item(be, outputter, tag-as-integer(be, lit.size));
  output-data-byte(be, outputter, lit);
  // Don't forget to allow for the zero termination ...
  output-data-byte(be, outputter, 0);
  output-data-footer(be, outputter, ref);
end method;


define method output-raw-literal (be :: <harp-back-end>, 
                                  outputter, 
                                  ref :: <constant-reference>,
                                  lit :: <vector>,
                                  section :: <symbol>)
  output-definition(be, outputter, ref, section: section);
  for (word in lit)
    output-data-item(be, outputter, word);
  end for;
  output-data-footer(be, outputter, ref);
end method;


define method output-raw-byte-literal (be :: <harp-back-end>, 
                                       outputter, 
                                       ref :: <constant-reference>,
                                       lit :: <vector>,
                                       section :: <symbol>)
  output-definition(be, outputter, ref, section: section);
  for (byte in lit)
    output-data-byte(be, outputter, byte);
  end for;
  output-data-footer(be, outputter, ref);
end method;





/// Defining variables within the runtime:- 

define macro runtime-variable-definer
  { define ?var-type runtime-variable ?:name = ?string:expression, 
           #key ?data:expression = 0, 
                ?ref:expression = #f, 
                ?repeat:expression = #f,
                ?section:expression = #"untraced-data", 
                ?base?:expression = #t, 
                ?public?:expression = #t, 
                ?client?:expression = #f}
    => { define variable ?name = #f;
         register-constant(method (be :: <harp-back-end>, outputter) 
			     let mangle = ?var-type;
                             let ref = mangle(be, ?string);
			     let repeat = ?repeat;
			     let repeat =
			       if (instance?(repeat, <function>)) repeat(be)
			       else repeat end;
                             ?name := ref;
                             output-variable(be, outputter, ref,
					     (?ref & ins--constant-ref(be, ?ref))
					      | ?data, 
                                             repeat: repeat,
                                             public?: ?public?,
                                             section: ?section);
			     output-data-footer(be, outputter, ref);
                           end method,
                           ?#"name",
                           client?: ?client?,
                           base?: ?base?) }
var-type:
  { } 
    => { mangled-indirect-ref }
  { direct } 
    => { mangled-ref }
  { c } 
    => { c-mangled-ref }
  { c-full } 
    => { c-full-mangled-ref }
  { c-full-indirect } 
    => { c-full-mangled-indirect-ref }
end macro;



/// Defining external constants:-

define macro runtime-external-definer
  { define ?ext-type runtime-external ?:name 
      = ?string:expression, #key ?data:* = #f, 
                                 ?base?:expression = #t, 
                                 ?client?:expression = #f }
    => { define variable ?name = #f;
         register-constant(method (?=be :: <harp-back-end>, outputter) 
                             let ref = if (?data)
                                         ?ext-type(?=be, ?string, ?data);
                                       else
                                         ?ext-type(?=be, ?string);
                                       end if;
                             ?name := ref;
                             output-external(?=be, outputter, ref);
                           end method,
                           ?#"name",
                           client?: ?client?,
                           base?: ?base?) }

ext-type:
  { } 
    => { constant-mangled-ref }
  { dylan-library } 
    => { dylan-library-constant-mangled-ref }
  { dylan-internal } 
    => { dylan-internal-constant-mangled-ref }
  { dylan-internal-indirect } 
    => { dylan-internal-variable-mangled-ref }
  { dylan-wrapper } 
    => { dylan-library-wrapper-mangled-ref }
  { dylan-iep } 
    => { dylan-library-iep-mangled-ref }
  { c-fun } 
    => { c-mangled-ref }
  { c-indirect } 
    => { c-mangled-indirect-ref }
  { c-full-indirect } 
    => { c-full-mangled-indirect-ref }
  { win-fun } 
    => { stdcall-mangled-ref }
  { unmangled } 
    => { ins--constant-ref }
  { ?:name }
    => { "dylan-" ## ?name ## "-mangled-ref" }
end macro;




/// Defining variables within the runtime:- 

/*
define macro runtime-alias-definer
  { define runtime-alias ?:name = ?alias:name }
    => { define variable ?name = #f;
         register-constant(method (be :: <harp-back-end>, outputter) 
                             ?name := ?alias;
                           end method,
                           ?#"name") }
end macro;
*/




//// Simple primitives and functions


// Defining a simple primitive/function does 3 things:- 
//   1. defines what HARP code is generated for a primitive
//   2. adds the code generation function to a list of registered primitives
//   3. defines a variable which contains a constant reference
//      for that primitive, so that it may be referenced from others 


define macro runtime-function-aux-definer
  { define ?adjectives runtime-function-aux ?:name 
        (?mangler:expression, ?name-string:expression, ?options:*)
      ?body:*
    end }
    => { define variable ?name ## "-ref" = #f;

         define method "op--" ## ?name
               (?=be :: <harp-back-end>) => ()
           with-harp (?=be)
             ?body
           end with-harp;
         end method;

         register-function
           (method (be :: <harp-back-end>, outputter, output-one-fn :: <function>, 
                    #key limit, #all-keys)
              ignore(outputter);
              ignore(limit);
              let prim-name = ?mangler(be, ?name-string);
              ?name ## "-ref" := ins--constant-ref(be, prim-name);
              output-one-fn(prim-name, "op--" ## ?name, ?adjectives);
            end method,
            ?#"name",
            ?options) }

options:
  { } 
    => { base?: #t, client?: #f }  // the defaults
  { shared ... } 
    => { client?: #t, base?: #t, ... }
  { client ... } 
    => { client?: #t, base?: #f, ... }
  { base ... } 
    => { client?: #f, base?: #t, ... }
  { ?:name ... }
    => {  ... }

adjectives:
  { } 
    => { export: #"code-stub" }  // the defaults
  { leaf ... } 
    => { defasm: #t, ... }
  { frame ... } 
    => { defasm: #f, ... } // actually the default anyway
  { call-in ... } 
    => { call-in: #t, ... }
  { no-export ... } 
    => { export: #f, ... }
  { no-public ... } 
    => { public: #f, ... }
  { init ... }
    => { section: #"init-code", ... }
  { shared ... } 
    => { export: #f, ... }
  { client ... } 
    => { export: #f, ... }
  { base ... } 
    => { export: #"code-stub", ... }
  { no ?:name ... }
    => { ?#"name", #f, ... }
  { ?:name ... }
    => { ?#"name", #t, ... }
end macro;


define macro runtime-function-definer
  { define ?adjectives:* runtime-function ?:name 
      ?body:*
    end }
    => { define ?adjectives runtime-function-aux ?name
             (raw-mangle, as-lowercase(?"name"), ?adjectives)
           ?body
         end }
end macro;

define macro runtime-primitive-definer
  { define used-by-client ?adjectives:* runtime-primitive ?:name 
      ?body:*
    end }
    => { define ?adjectives runtime-primitive ?name ?body end;
         define unmangled runtime-external ?name ## "-used-by-client"
           = primitive-name(?=be, as-lowercase(?"name")),
             client?: #t
       }
             
  { define ?adjectives:* runtime-primitive ?:name 
      ?body:*
    end }
    => { define ?adjectives runtime-function-aux "primitive-" ## ?name
             (primitive-name, as-lowercase(?"name"), ?adjectives)
           ?body
         end }
end macro;

// Different Operating-System versions of the harp-back-end
// will provide their own implementations of these runtime primitives

define macro generic-runtime-primitive-definer
  { define ?adjectives:* generic-runtime-primitive ?:name }
    => {
         define open generic "genop--" ## ?name
           (be :: <harp-back-end>) => ();

         define ?adjectives runtime-primitive ?name
           "genop--" ## ?name (?=be)
         end;
       }
end macro;

define macro generic-c-runtime-primitive-definer
  { define ?adjectives:* generic-c-runtime-primitive ?:name }
    => {
         define open generic "genop--" ## ?name
           (be :: <harp-back-end>) => ();

         define ?adjectives c-runtime-primitive ?name
           "genop--" ## ?name (?=be)
         end;
       }
end macro;


define macro c-runtime-primitive-definer
  { define used-by-client ?adjectives:* c-runtime-primitive ?:name 
      ?body:*
    end }
    => { define ?adjectives c-runtime-primitive ?name ?body end;
         define unmangled runtime-external ?name ## "-used-by-client"
           = c-primitive-name(?=be, as-lowercase(?"name")),
             client?: #t
       }
             
  { define ?adjectives:* c-runtime-primitive ?:name
      ?body:*
    end }
    => { define ?adjectives call-in runtime-function-aux "primitive-" ## ?name
             (c-primitive-name, as-lowercase(?"name"), ?adjectives)
           ?body
         end }
end macro;



//// Entry point primitives


define macro entry-point-definer
  { define entry-point ?:name 
        (?be:name :: ?be-type:expression, ?num:variable, #key ?limit:expression = 9)
      ?body:*
    end }
    => { define variable ?name ## "-refs" = #f;

         define method "op--" ## ?name ## "-entry-point"
               (?be :: ?be-type, ?num) => ()
           with-harp (?be)
             ?body
           end with-harp;
         end method;

         register-function
           (method (be :: <harp-back-end>, outputter, output-one-fn :: <function>,
                    #key limit = ?limit, #all-keys) 
	      let limit :: <integer> =
		if (instance?(limit, <function>)) limit(be)
		else limit end;
              ignore(outputter);
              ?name ## "-refs" 
                := output-entry-point-set
                     (be,
                      output-one-fn,
                      "op--" ## ?name ## "-entry-point",
                      ?"name",
                      limit);
            end method,
            ?#"name") }
end macro;

// Entry point support

define method output-entry-point-set
    (be :: <harp-back-end>, 
     output-one-fn :: <function>, 
     emitter-fn :: <function>, 
     name-stem :: <byte-string>, 
     limit :: <integer>)
    => (refs :: <vector>)

  local method output-an-entry-point (r) => (ref :: <constant-reference>)
          let name = entry-point-name(be, name-stem, r);
          output-one-fn(name, method (be) emitter-fn(be, r) end, 
                        defasm: #t, export: #"code-stub");
          ins--constant-ref(be, name);
        end;

  let refs = make(<vector>, size: limit + 2);
  for (r from 0 to limit)
    refs[r] := output-an-entry-point(r);
  end for;

  refs[limit + 1] :=  output-an-entry-point(#"dynamic");

  refs;
end method;



//// Switch tables 



define macro runtime-switch-table-definer
  { define runtime-switch-table ?:name 
      size   ?size:expression;
      prolog (?prolog:*);
      epilog (?epilog:*);
      ?cases:case-body
    end }
    => { define variable ?name = #f;


         // Register the building of the table
         register-constant
           (method (?=be :: <harp-back-end>, outputter)
	      let ?name ## "-entries" =
                output-table-data-entries ?name ## "-entries"
		  prolog (?prolog)
		  epilog (?epilog)
		  ?cases
                end; 
              ?name := output-table(?=be, outputter, ?"name", ?name ## "-entries", ?size);
            end method,
            ?#"name");

         // Register a function for each special case of the table
         register-function
           (method (?=be :: <harp-back-end>, outputter, ?=output-one-fn :: <function>,
                    #key limit, #all-keys) 
              ignore(limit);
              ignore(outputter);
              output-table-entries ?name ## "-entries"
		prolog (?prolog)
		epilog (?epilog)
		?cases
              end;
            end method,
            ?#"name" ## "-entries")

       }
end macro;

define macro output-table-entries

  { output-table-entries ?:name 
      prolog (?prolog:*)
      epilog (?epilog:*)
    end }
    => { #() }

  { output-table-entries ?:name 
      prolog (?prolog:*)
      epilog (?epilog:*)
      ?cases => ?:body;
      ?other-cases:*
    end }
    => { begin
       
           local method gen-table-entry (?=be :: <harp-back-end>)
                   with-harp (?=be)
                     ?prolog;
                     ?body;
                     ?epilog
                   end with-harp;
                 end;

           let res = output-table-entry
                       (?=be, ?=output-one-fn, gen-table-entry, ?cases, ?"name");
           output-table-entries ?name
                  prolog (?prolog)
                  epilog (?epilog)
                  ?other-cases
           end;
         end
       }

cases:
  { otherwise  }
    => { #"default" }
  { ?vals:* }
    => { vector(?vals) }

vals:
  {} => {}
  { ?val:expression, ... } => { ?val, ... }
end macro;

define macro output-table-data-entries

  { output-table-data-entries ?:name 
      prolog (?prolog:*)
      epilog (?epilog:*)
    end }
    => { #() }

  { output-table-data-entries ?:name 
      prolog (?prolog:*)
      epilog (?epilog:*)
      ?cases => ?:body;
      ?other-cases:*
    end }
    => { begin

           let res = output-table-data-entry
                       (?=be, ?cases, ?"name");
           pair(res, 
                output-table-data-entries ?name
                  prolog (?prolog)
                  epilog (?epilog)
                  ?other-cases
                end);
         end
       }

cases:
  { otherwise  }
    => { #"default" }
  { ?vals:* }
    => { vector(?vals) }

vals:
  {} => {}
  { ?val:expression, ... } => { ?val, ... }
end macro;


// Switch table support


define method output-table-entry
    (be :: <harp-back-end>, 
     output-one-fn :: <function>, 
     emitter-fn :: <function>,
     cases,
     name-stem :: <byte-string>)
    => ()

  let name = table-entry-name(be, name-stem, cases);
  output-one-fn(name, emitter-fn, defasm: #f);
end method;

define method output-table-data-entry
    (be :: <harp-back-end>, 
     cases,
     name-stem :: <byte-string>)
    => (ref-info :: <pair>)

  let name = table-entry-name(be, name-stem, cases);
  pair(ins--constant-ref(be, name), cases);
end method;


define method table-entry-name 
    (be :: <harp-back-end>, name :: <byte-string>, cases :: <vector>)
    => (name :: <byte-string>)
  raw-mangle(be, format-to-string("%s-%=", as-lowercase(name), cases[0]));
end method;


define method table-entry-name 
    (be :: <harp-back-end>, name :: <byte-string>, cases == #"default")
    => (name :: <byte-string>)
  raw-mangle(be, format-to-string("%s-default", as-lowercase(name)));
end method;


define method output-table
    (be :: <harp-back-end>, outputter, 
     name :: <byte-string>, entries :: <list>,
     table-size :: <integer>) => (ref :: <constant-reference>)
  // entries is a list of pairs, where each pair is of the the form 
  // returned by output-table-entry. I.e. (constant-ref . cases)

  let ref = constant-mangled-ref(be, name.as-lowercase);
  let default-key = find-key(entries, method (elt) elt.tail == #"default" end);
  let default-val = if (default-key) entries[default-key].head end;

  // Build a vector filled with appropriate constant refs
  let data = make(<vector>, size: table-size, fill: default-val);
  for (entry in entries)
    let cases = entry.tail;
    let ref = entry.head;
    unless (cases = #"default")
      for (i :: <integer> in cases)
        data[i] := ref;
      end for;
    end unless;
  end for;

  // Output that vector
  output-raw-literal(be, outputter, ref, data, #"untraced-data");

  ref;
end method;


ignore(op--jump-into-switch-table);

define method op--jump-into-switch-table 
    (be :: <harp-back-end>, table :: <constant-reference>, offset-in-bytes)
  with-harp (be)
    nreg dest;
    let max-num-arg-regs = be.registers.arguments-passed-in-registers;
    ins--ld(be, dest, table, offset-in-bytes);
    ins--jmp(be, dest, max-num-arg-regs);
  end with-harp;
end method;



///// The registration mechanism support


/// General support, including name lookup
//
// The registration mechanism works in two stages. An ordered sequence 
// of symbols is used to key into a table of output functions. 

define method register
    (seq :: <stretchy-vector>, table :: <table>, fn :: <function>, name :: <symbol>)
    => (seq :: <stretchy-vector>)
  table[name] := fn;
  add-new!(seq, name);
end method;


define method lookup-fn 
    (val :: <symbol>, table :: <table>) => (fn :: <function>)
  table[val];
end method;



/// Now the specific support. 
/// This is divided into two parallel register sets, one for the base
/// runtime, and one for the client runtime.

define thread variable *generating-client* = #f;

define macro when-base
  { when-base ?:body end }
    => { unless (*generating-client*) ?body end }
end macro;

define macro when-client
  { when-client ?:body end }
    => { if (*generating-client*) ?body end }
end macro;


/// Registration is also separated into two distinct
/// registers - one for data (literals and constants), and the other for code
/// (primitives, functions, entry-points etc). 



///  1. Runtime Data


define variable *base-runtime-data* = make(<stretchy-vector>);
define variable *client-runtime-data* = make(<stretchy-vector>);

define variable *base-data-table* :: <table> = make(<table>);
define variable *client-data-table* :: <table> = make(<table>);

/*
define method reset-data () => ()
  *base-runtime-data*.size := 0;
  *client-runtime-data*.size := 0;
  *base-data-table* := make(<table>);
  *client-data-table* := make(<table>);
end method;
*/

define method register-constant
     (output-fn :: <function>, name :: <symbol>, 
      #key client? = #f, base? = (~ client?))
      => ()
  if (base?)
    *base-runtime-data* :=
      register(*base-runtime-data*, *base-data-table*, output-fn, name);
  end if;
  if (client?)
    *client-runtime-data* :=
      register(*client-runtime-data*, *client-data-table*, output-fn, name);
  end if;
end method;

define method output-data
     (be :: <harp-back-end>, outputter, #key client? = #f) => ()
  let (runtime-data, data-table) = 
    if (client?) 
      values(*client-runtime-data*, *client-data-table*)
    else values(*base-runtime-data*, *base-data-table*)
    end;
  dynamic-bind (*generating-client* = client?)
    for (val in runtime-data)
      let output-fn = lookup-fn(val, data-table);
      output-fn(be, outputter);
    end for;
  end dynamic-bind;
end method;



///  2. Runtime functions:


define variable *base-runtime-functions* = make(<stretchy-vector>);
define variable *client-runtime-functions* = make(<stretchy-vector>);

define variable *base-functions-table* :: <table> = make(<table>);
define variable *client-functions-table* :: <table> = make(<table>);

/*
define method reset-functions () => ()
  *base-runtime-functions*.size := 0;
  *client-runtime-functions*.size := 0;
  *base-functions-table* := make(<table>);
  *client-functions-table* := make(<table>);
end method;
*/

define method register-function
     (output-function :: <function>, name :: <symbol>, 
      #key client? = #f, base? = (~ client?))
      => ()
  if (base?)
    *base-runtime-functions* :=
      register(*base-runtime-functions*, *base-functions-table*,
                output-function, name);
  end if;
  if (client?)
    *client-runtime-functions* :=
      register(*client-runtime-functions*, *client-functions-table*,
                output-function, name);
  end if;
end method;

define method output-functions
     (be :: <harp-back-end>, outputter, output-one-fn :: <function>, 
      #key limit = #f, client? = #f, #all-keys) => ()
  let output-keys = if (limit) vector(limit: limit) else #[] end;
  let (runtime-functions, functions-table) = 
    if (client?) 
      values(*client-runtime-functions*, *client-functions-table*)
    else values(*base-runtime-functions*, *base-functions-table*)
    end;
  dynamic-bind (*generating-client* = client?)
    for (val in runtime-functions)
      let output-function = lookup-fn(val, functions-table);
      apply(output-function, be, outputter, output-one-fn, output-keys);
    end for;
  end dynamic-bind;
end method;





