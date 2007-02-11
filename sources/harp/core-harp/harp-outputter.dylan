module:    harp-outputter
Synopsis:  HARP output generation
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// The <harp-outputter> class.
//// This represents a target destination for linking, and is supported
//// by the outputter protocol, which is defined below.


define abstract primary open class <harp-outputter> (<object>)
end class;



///// Specializable functions to output code from HARP
/////
///// Specific libraries can register their services on top of this
///// framework, permitting HARP support for assembler, COFF, DOSS etc.



//// Functions that are used by clients of HARP for the purposes of emitting 
//// code:


// MAKE-HARP-OUTPUTTER
// makes an outputer for HARP assembler output. The filename 
// will normally be a string (in which case, it should not include 
// an extension). Some outputters may permit other values (e.g. streams)
//
// The result of this function
// may be passed to the outputter parameter for the output-* functions
// described below.
//
// If type is not supplied, then it will default to
// backend.default-harp-output-type. If supplied, then it may 
// either be a symbol naming an output type (e.g. #"coff") or a 
// sequence of symbols naming a composite output type.
// 
// If print-harp? is supplied as true, then a #"print-harp" outputter
// type is created too.
//

define variable print-harp-by-default = #f;

define sealed generic make-harp-outputter
    (backend :: <harp-back-end>, filename, #key type, print-harp?)
    => (outputter :: <harp-outputter>);

define method make-harp-outputter
    (backend :: <harp-back-end>, filename, 
     #key type =  backend.default-harp-output-type, print-harp? = unsupplied())
    => (outputter :: <harp-outputter>)
  let print-harp? = if (supplied?(print-harp?)) print-harp? else print-harp-by-default end if;
  let types = if (print-harp?) list(#"print-harp", type) else type end;
  make-harp-outputter-by-type(backend, filename, types);
end method;



// Use this function to cache compiled-lambdas in compilation records

define open generic output-compilation-record-data
  (back-end :: <harp-back-end>, name :: <byte-string>, compiled-lambda) => ();

define method output-compilation-record-data
  (back-end :: <harp-back-end>, name :: <byte-string>, compiled-lambda) => ()
end method;


// A Model Object Protocol for the Harp Back End

// This enables the Harp Back End to work directly with compiler model-objects, 
// as well as maintaining compatibility with other clients at the same time


// This is a directive to outputters that a client will pass models through to Harp

define open generic model-object-protocol(outputter :: <harp-outputter>) => ();

define method model-object-protocol(outputter :: <harp-outputter>) => ()
end method;


// This is an outputter predicate that determines if models are currently the unit of 
// canonicalization

define open generic model-object-protocol?(outputter :: <harp-outputter>)
 => (model-object-protocol?);

define method model-object-protocol?(outputter :: <harp-outputter>)
 => (model-object-protocol?)
  #f
end method;

define open generic model-object-protocol?-setter
(model-object-protocol?, outputter :: <harp-outputter>)
 => (model-object-protocol?);

define method model-object-protocol?-setter
(model-object-protocol?, outputter :: <harp-outputter>)
 => (model-object-protocol?)
  #f
end method;


define open generic dynamic-linking-protocol(outputter :: <harp-outputter>) => ();

define method dynamic-linking-protocol(outputter :: <harp-outputter>) => ()
end method;


// Byte-string model-objects have to be modelled properly by clients
// for consistency;
// Harp References to bare strings will consistently be interpreted as names
// of model-objects only

define class <string-model-object>(<object>)
  constant slot string-model-object :: <byte-string>, required-init-keyword: model:;
end class;

define method make-string-model-object(object :: <byte-string>)
 => (model-object :: <string-model-object>)
  make(<string-model-object>, model: object)
end method;

// This method is in here for Outputters that don't support Model Objects
// to work with Clients that do

define method model-object-as-string(object :: <string-model-object>) => (name :: <byte-string>)
  model-object-as-string(object.string-model-object)
end method;


// These are merely in here for efficiency -- they enable the Compiler Client
// not to emit names of models that are derived from other models multiply,
// by defining the derived model at the same time that the parent model is defined

define class <derived-model-object>(<object>)
  constant slot derived-model-object, required-init-keyword: model:;
  constant slot derived-model-object-suffix, required-init-keyword: suffix:;
end class;

define method make-derived-model-object(object, suffix :: <byte-string>)
 => (model-object :: <derived-model-object>)
  make(<derived-model-object>, model: object, suffix: suffix)
end method;

define method outputter-model-object(object :: <derived-model-object>)
 => (model-object)
  object.derived-model-object
end method;

define method outputter-model-object-name(object :: <derived-model-object>, parent-name :: <byte-string>)
 => (derived-name :: <byte-string>)
  concatenate(parent-name, object.derived-model-object-suffix)
end method;


// These are constants that enable outputters to use names and model-objects
// consistently

// An unsupplied-name is an indication to the outputter to process the model-object;
// An unsupplied model is an indication to outputter to just use the supplied-name;
// It is a requirement that one or the other must be supplied;
// We can't use #f to default model-objects because #f is a legal model-object


define constant $outputter-unsupplied-name = "unsupplied-name";

define method outputter-name-unsupplied() => (unsupplied :: <byte-string>)
  $outputter-unsupplied-name
end method;

define method outputter-name-supplied?(name :: <byte-string>) => (supplied? :: <boolean>)
  name ~== $outputter-unsupplied-name
end method;


define constant $dummy-name = $outputter-unsupplied-name;
define constant $dummy-model-object = unsupplied();


// For Outputters that support the model-object protocol, constant-references in
// generated code are interpreted as refering to models, not names of models;
// The Client must ensure that references are only made to actual models;
// Strings that are intended to be actual models must be explicitly turned into 
// real models by Clients;
// Bare strings continue to be names of models

define method canonical-code-object
  (outputter :: <harp-outputter>,
   reference :: <constant-reference>) => (name :: <byte-string>, model-object)
  if (outputter.model-object-protocol?)
    let object = reference.cr-refers-to-object;
    select (object by instance?)
      <byte-string>          => values(object, $dummy-model-object);
      <string-model-object>  => values($dummy-name, object.string-model-object);
      otherwise              => values($dummy-name, object);
    end select
  else
    values(reference.cr-refers-to, $dummy-model-object)
  end if;
end method;


// For Clients that use model-objects, a "lambda-name" may refer to a model-object

define method canonical-lambda-object
  (lambda :: <fully-compiled-lambda>) => (name :: <byte-string>, model-object)
  let lambda-object = lambda.lambda-model-object;

  select (lambda-object by instance?)
    <byte-string> => values(lambda-object, $dummy-model-object);
    otherwise => values($dummy-name, lambda-object);
  end select;
end method;


// This enables compatibility of Harp Outputters that don't support model-objects
// with Clients that do, by coercing models to their names

define method canonical-data-object
  (data-item, model-object) => (data-item)
  if (supplied?(model-object))
    model-object-as-string(model-object)
  else
    data-item
  end if
end method;


// Name emission is strictly controlled by Model-Object Outputters;
// This is the function that does the coercion only if the Client hasn't
// supplied a name;
// The intention is that the coercion only happens once per model-object
// per outputter session

define method model-object-name
  (model-object, name :: <byte-string>) => (name :: <byte-string>)
  if (outputter-name-supplied?(name))
    name
  else
    model-object-as-string(model-object)
  end if
end method;

// This coerces lambda-names to strings

define method lambda-name(lambda :: <compiled-lambda>)
 => (lambda-name :: <byte-string>)
  let lambda-object = lambda.lambda-name-internal;
  if (instance?(lambda-object, <string>)) lambda-object
  else model-object-as-string(lambda-object)
  end if;
end method;



// One form of canonicalisation cannot be to names, since the interactor
// expects a handle of unspecified type. It is permitted for the handle
// to be distinct from the model-object - in which case a method must be added
// to canonical-interactor-object by the client for that model-object type.

define open generic canonical-interactor-object (object) => (handle);

define method canonical-interactor-object
    (object :: <object>) => (object :: <object>)
  object
end method;

define sealed method canonical-interactor-object
    (reference :: <constant-reference>) => (object :: <object>)
  reference.cr-refers-to-object.canonical-interactor-object;
end method;





// CLOSE-HARP-OUTPUTTER
// Must be called when output is complete
// Must be specialized by an outputter library
//
define open generic close-harp-outputter
    (backend :: <harp-back-end>, outputter :: <harp-outputter>,
     #key) => ();




// OUTPUT-COMMENT outputs the comment to the outputter.
// This will not necessary do anything (depending on the outputter).
// Must be specialized by an outputter library
//
define open generic output-comment
    (be :: <harp-back-end>, outputter :: <harp-outputter>, comment) => ();


// OUTPUT-LINE-COMMENT outputs the comment string to the outputter,
// on a single line.
// This will not necessary do anything (depending on the outputter).
// Must be specialized by an outputter library
//
define open generic output-line-comment
    (be :: <harp-back-end>, outputter :: <harp-outputter>, comment) => ();


// OUTPUT-EXTERNAL defines the name as an external symbol.
// Name will normally be a <constant-reference> or <byte-string>
// The import? keyword must be set to #t for externals from other libraries
// Must be specialized by an outputter library
//
define open generic output-external 
    (be :: <harp-back-end>, outputter :: <harp-outputter>, name,
     #key, #all-keys) => ();


define open generic output-implicit-externals
    (be :: <harp-back-end>, outputter :: <harp-outputter>);


// OUTPUT-PUBLIC defines the name as a public symbol.
// Name will normally be a <constant-reference> or <byte-string>
// Must be specialized by an outputter library
//
define open generic output-public 
    (be :: <harp-back-end>, outputter :: <harp-outputter>, name,
     #key, #all-keys) => ();


// OUTPUT-EXPORT defines the name as an exported symbol.
// Name will normally be a <constant-reference> or <byte-string>
// Must be specialized by an outputter library
//
define open generic output-export
    (be :: <harp-back-end>, outputter :: <harp-outputter>, name) => ();


// OUTPUT-DEFINITION defines the name symbolically at the current
// point in the section.
// Name will normally be a <constant-reference> or <byte-string>
// If a section is given it should be a <symbol> describing where to 
// put the definition. The following sections have special meanings:-
//      #"data"             - ambiguously traced data section (this is the default)
//      #"ambiguous-data"   - synonym for the above
//      #"variables"        - section traced as roots by the GC
//      #"objects"          - section traced as a heap by the GC
//      #"untraced-objects" - heap section, untraced by the GC
//      #"untraced-data"    - untraced data section 
//      #"code"             - the code or text section
//      #"init-code"        - separate initialization part of the code section
//
// Must be specialized by an outputter library. 
//
define open generic output-definition 
    (be :: <harp-back-end>, outputter :: <harp-outputter>, name,
     #key, #all-keys) => ();


// OUTPUT-VARIABLE defines a variable and associates it with an
// initial value. If the repeat key is given, it should be an integer
// which specifies that the initial value should be repeated N times.
// If public? is given a true value, then the name is declared to be 
// public, as with output-public. If export? is given a true value, 
// then the variable is exported. If import-value? is given a true value
// then the initial value is imported from another library. If a section is 
// given, it should be specified as for output-definition.
//
// Must be specialized by an outputter library.
//
define open generic output-variable
    (be :: <harp-back-end>, outputter :: <harp-outputter>, name, initial-value, 
     #key, #all-keys) => ();


// OUTPUT-HEADER should be the first output function called on the
// outputter
// Must be specialized by an outputter library
//
define open generic output-header 
    (be :: <harp-back-end>, outputter :: <harp-outputter>) => ();


// OUTPUT-FOOTER should be the last output function called on the
// outputter
// Must be specialized by an outputter library
//
define open generic output-footer 
    (be :: <harp-back-end>, outputter :: <harp-outputter>) => ();


// OUTPUT-DATA-START should be called before any calls to output data
// (e.g. with output-variable)
// Must be specialized by an outputter library
//
define open generic output-data-start 
    (be :: <harp-back-end>, outputter :: <harp-outputter>) => ();


// OUTPUT-CODE-START should be called before any calls to output code
// (e.g. with output-lambda-preamble)
// Must be specialized by an outputter library
//
define open generic output-code-start 
    (be :: <harp-back-end>, outputter :: <harp-outputter>) => ();


// OUTPUT-GLUE-SYMBOLS should be used for just one outputted file in 
// a library, to define any library-specific symbols. If called, it should be 
// called immediately before the call to output-footer
// Must be specialized by an outputter library
//
define open generic output-glue-symbols 
    (be :: <harp-back-end>, outputter :: <harp-outputter>,
     #key, #all-keys) => ();


// OUTPUT-DATA-ITEM
// Puts data into the current place in the current section.
// The import? keyword must be set to #t for items imported from other libraries
// Must be specialized by an outputter library
//
define open generic output-data-item
    (be :: <harp-back-end>, outputter :: <harp-outputter>, item,
     #key, #all-keys) => ();


// OUTPUT-DATA-BYTE
// Puts data into the current place in the current section.
// Must be specialized by an outputter library
//
define open generic output-data-byte
    (be :: <harp-back-end>, outputter :: <harp-outputter>, byte) => ();


// ELF Outputters use this emitter to emit type and size of data, so the 
// Linker can create appropriate dynamic relocation records for them

define open generic output-data-footer
    (be :: <harp-back-end>, outputter :: <harp-outputter>, item,
     #key, #all-keys) => ();


define method output-data-footer
    (be :: <harp-back-end>, outputter :: <harp-outputter>, item,
     #key, #all-keys) => ()
end method;


// OUTPUT-COMPILED-LAMBDA
// Outputs the code for a compiled lambda
// Must be specialized by an outputter library
// The section keyword may be accepted to group initialization code.
// Acceptable values are #"code" and #"init-code" with meanings as for 
// output-definition
//
define open generic output-compiled-lambda
    (be :: <harp-back-end>, outputter :: <harp-outputter>,
     code :: <fully-compiled-lambda>,
     #key, #all-keys) => ();



// OUTPUTTER-DOWNLOADABLE-DATA
// Returns an object suitable for downloading across the tether (or #f)
// A default method returns #f. Some outputter implementation somewhere
// should do something better

define open generic outputter-downloadable-data
    (be :: <harp-back-end>, outputter :: <harp-outputter>) => (data);

define method outputter-downloadable-data
    (be :: <harp-back-end>, outputter :: <harp-outputter>) => (data)
 #f;
end method;


//// Functions that may be specialized by HARP back ends:

define open generic default-harp-output-type
    (backend :: <harp-back-end>) => (type :: <object>);

define method default-harp-output-type
    (backend :: <harp-back-end>) => (type :: <list>)
  // for now, as a default, we'll generate nothing
  #();
end method;



//// Functions that may be specialized by outputter libraries

// MAKE-HARP-OUTPUTTER-BY-TYPE
// Outputter libraries may specialize this with singleton specializers on
// type. Normally, these specializers will be singletons of symbols.

define open generic make-harp-outputter-by-type
    (backend :: <harp-back-end>, filename, type)
    => (outputter :: <harp-outputter>);




// FILE-EXTENSION-FOR-OUTPUTTER-TYPE
// May be specialized by an outputter library if the outputter type
// uses OPEN-OUTPUT-STREAM for stream creation.
//

define open generic file-extension-for-outputter-type
    (backend :: <harp-back-end>, type :: <symbol>) 
    => (extension :: <byte-string>);


define open generic stream-type-for-outputter-type
    (backend :: <harp-back-end>, type :: <symbol>) 
    => (stream-type :: <class>);

define method stream-type-for-outputter-type
    (backend :: <harp-back-end>, type :: <symbol>)
    => (stream-type :: <class>)
  <file-stream>
end method;


// OPEN-OUTPUT-STREAM
// A function to create a file stream given a name and an extension.
// This has little business being in this library - but centralising
// somewhere related to HARP will allow for expected future changes 
// to the streams library with as little pain as possible.
//

define open generic open-output-stream 
      (back-end :: <harp-back-end>, file-name, extension)  => (s :: <stream>);

define method open-output-stream
      (back-end :: <harp-back-end>, 
       file-name :: <byte-string>, extension :: <byte-string>) 
      => (s :: <stream>)
  let full-name = concatenate(file-name, ".", extension);
  make(<file-stream>,
       locator: as(<file-locator>, full-name),
       direction: #"output",
       stream-lock: #f);
end method;

define method open-output-stream
      (back-end :: <harp-back-end>, 
       file-name :: <byte-string>, type :: <symbol>) 
      => (s :: <stream>)
  let extension = file-extension-for-outputter-type(back-end, type);
  let stream-type = stream-type-for-outputter-type(back-end, type);
  let full-name = concatenate(file-name, ".", extension);
  make(stream-type,
       locator: as(<file-locator>, full-name),
       direction: #"output",
       stream-lock: #f);
end method;

define method open-output-stream
      (back-end :: <harp-back-end>, 
       file-name :: <locator>, type) 
      => (s :: <stream>)
  open-output-stream(back-end, as(<string>, file-name), type);
end method;


// CLOSE-OUTPUT-STREAM
// Provided for completeness - and so that outputter implementations 
// are not obliged to know anything about streams.

define method close-output-stream (s :: <stream>) => ()
  close(s);
end method;

